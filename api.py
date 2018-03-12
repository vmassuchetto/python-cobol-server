#!/usr/bin/python
# -*- coding: utf-8 -*-

import os
import sys
import hashlib
import getopt
import re
import gettext
import json
import subprocess
import copy
import cPickle as pickle
from lib.odict.odict import OrderedDict
from lib.pythoncobol import cobol
from conf import APPNAME, CACHE_COPYBOOKS, CACHE_PROGRAMS, \
    COMPILER_COMMAND, COMPILER_ERROR, COMPILER_ERROR_REGEX, \
    OPERATIONS, CACHEDIR, TEMPLATEDIR, LOCALESDIR

#
# A possible gettext bug fot Python 2 leads to .decode('utf-8') everything
# that is passed to '_()'
# see https://stackoverflow.com/questions/5545197/utf-8-error-with-python-and-gettext
#
try:
    import locale
    import gettext

    gettext.textdomain(APPNAME)
    gettext.bindtextdomain(APPNAME, LOCALESDIR)
    current_locale, encoding = locale.getdefaultlocale()
    translation = gettext.translation(APPNAME, LOCALESDIR, languages=[ current_locale ], fallback = True)
    _ = translation.gettext
except ImportError:
    pass

class CobolApiException(Exception):

    def __init__(self, message):
        if isinstance(message, unicode):
            super(CobolApiException, self).__init__(message.encode('utf-8'))
            self.message = message
        elif isinstance(message, str):
            super(CobolApiException, self).__init__(message)
            self.message = message.decode('utf-8')
        else:
            raise TypeError

    def __unicode__(self):
        return self.message

class CobolApi:

    _cli = False
    _args = {}
    _copybook = OrderedDict()
    _kwargs = ['help', 'operation=', 'copybook=', 'datafile=',
        'datafilekeys=', 'keyname=', 'keyvalue=', 'input=',
        'superkeyname=', 'superkeypart=']

    def print_help(self):
        help = \
"""
""" + _("Common options").decode('utf-8') + """:

    --help          """ + _("print this help message").decode('utf-8') + """
    --operation     """ + _("operation name: %s").decode('utf-8') % ', '.join(OPERATIONS) + """
                    """ + _("(required)").decode('utf-8') + """
    --copybook      """ + _("path to the copybook file").decode('utf-8') + " " + _("(required)").decode('utf-8') + """
    --datafile      """ + _("path to the datafile").decode('utf-8') + " " + _("(required)").decode('utf-8') + """
    --datafilekeys  """ + _("comma separated list of keys").decode('utf-8') + ", " + _("(required)").decode('utf-8') + """
                    """ + _("put the main key first and the alternate keys last").decode('utf-8') + """
    --keyname       """ + _("key to use in operation").decode('utf-8') + """
    --keyvalue      """ + _("value to match the key").decode('utf-8') + """
    --input         """ + _("input JSON object for the operation").decode('utf-8') + """
                    """ + _("check the README.md file for examples").decode('utf-8') + """

""" + _("Partial search options").decode('utf-8') + """:

    --superkeyname  """ + _("super key in case of doing a partial search").decode('utf-8') + """
    --superkeypart  """ + _("part of --keyname in the --superkeyname as a [start:end] format").decode('utf-8') + """
                    """ + _("check the README.md file for examples").decode('utf-8') + """
                    """ + _("of how to use partial key searches").decode('utf-8') + """
"""
        print(help)

    def parse_args_stdin(self):
        try:
            options, remainder = getopt.getopt(sys.argv[1:], [], self._kwargs)
        except getopt.GetoptError:
            self.print_help()
            exit(1)

        if len(options) == 0:
            self.print_help()
            exit(1)

        for opt, arg in options:
            self._args[opt.replace('=', '').replace('--', '')] = arg

    def check_args(self):
        # no option provided
        if len(self._args) == 0:
            self.fail(_('ERROR: No arguments provided.').decode('utf-8'))

        # supported operations
        if 'operation' not in self._args:
            self.fail(_('ERROR: --operation option required.').decode('utf-8'))
        self._args['operation'] = self._args['operation'].lower()
        if self._args['operation'] not in OPERATIONS:
            self.fail(_('ERROR: Supported operations: %s.').decode('utf-8') % ', '.join(OPERATIONS))

        # required options
        if self._args['operation'] == 'describe':
            required_options = ['operation', 'copybook']
            required_files = ['copybook']
        else:
            required_options = ['operation', 'copybook', 'datafile', 'datafilekeys']
            required_files = ['copybook', 'datafile']

        if not set(required_options).issubset(self._args.keys()):
            self.fail(_('ERROR: Required options: %s.').decode('utf-8') % ', '.join(["--" + r for r in required_options]))

        # files are valid
        for f in required_files:
            if not os.path.isfile(self._args[f]) or \
                os.path.getsize(self._args[f]) <= 0:
                self.fail(_('ERROR: Invalid file in --%s option.').decode('utf-8') % f)

        self.parse_copybook()

        # required options
        if self._args['operation'] in ('select', 'update', 'delete'):
            if 'keyname' not in self._args or 'keyvalue' not in self._args:
                self.fail(_('ERROR: --keyname and --keyvalue required for operation %s.').decode('utf-8') % self._args['operation'])

        # validate JSON input
        if 'input' in self._args:
            if self._cli:
                try:
                    self._args['input'] = json.loads(self._args['input'])
                except ValueError:
                    self.fail(_('ERROR: Could not parse JSON object input.').decode('utf-8'))

            for field in self._args['input']:
                invalid = []
                if field not in self._copybook:
                    invalid.append(field)
                if len(invalid) > 0:
                    self.fail(_('ERROR: Invalid fields: %s.').decode('utf-8') % ', '.join(invalid))

        # superkey
        if 'superkeyname' in self._args:
            if self._args['superkeyname'] not in self._copybook:
                self.fail(_('ERROR: Superkey not in the copybook.').decode('utf-8'))
            if 'superkeypart' not in self._args:
                self.fail(_('ERROR: --superkeypart required for partial key searches.').decode('utf-8'))
        elif 'keyname' in self._args:
            self._args['superkeyname'] = self._args['keyname']
            self._args['superkeypart'] = ''

        # required for insert operation
        if 'insert' in self._args['operation']:
            for k in self._args['datafilekeys'].split(','):
                if k not in self._args['input']:
                    self.fail(_('ERROR: File keys "%s" must be provided in --input.' % self._args['datafilekeys']).decode('utf-8'))

    def syscall(self, command):
        p = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE)
        return p.communicate()

    def fail(self, message):
        raise CobolApiException(message)

    def get_file_contents(self, path):
        try:
            with open(path, 'r') as f:
                contents = f.read()
        except:
            self.fail(_('ERROR: Could not read the file %s.').decode('utf-8') % path)

        return contents

    def put_file_contents(self, contents, path):
        try:
            with open(path, 'w') as f:
                f.write(contents)
        except:
            self.fail(_('ERROR: Could not write to the file %s.').decode('utf-8') % path)

    def parse_copybook(self):
        """
        Parse the copybook into a readable Python object and save it in a
        pickle cache file for future use.
        """

        self._copybook = OrderedDict()

        hashstr = hashlib.md5(json.dumps(self._args['copybook'])).hexdigest()[:32].upper()
        cache_file = CACHEDIR + hashstr + '.PKL'

        if CACHE_COPYBOOKS and \
            os.path.isfile(cache_file) and \
            os.path.getsize(cache_file) > 0 and \
            os.path.getmtime(self._args['copybook']) < os.path.getmtime(cache_file):
            with open(cache_file, 'r') as pickle_file:
                self._copybook = pickle.load(pickle_file)
            return

        with open(self._args['copybook'], 'r') as f:
            data = cobol.clean_cobol(f.readlines())
            data = cobol.parse_cobol(data, True)
            data = cobol.denormalize_cobol(data)
            data = cobol.clean_names(data, True, True)

            for d in data:
                self._copybook[d['name']] = d

        # cached information
        for f in self._copybook:
            self._copybook[f]['length'] = self.get_field_length(f)
            self._copybook[f]['type'] = self.get_field_type(f)
            self._copybook[f]['children'] = self.get_field_children_first_level(f)

        if CACHE_COPYBOOKS:
            with open(cache_file, 'wb') as pickle_file:
                pickle.dump(self._copybook, pickle_file)

    def get_key_definitions(self):
        """
        Build definition string for keys in a datafile.
        """
        i = 0
        key_definitions = ''

        for k in self._args['datafilekeys'].split(','):

            i += 1
            if i == 1:
                key_definitions += 'RECORD KEY IS %s' % k
            else:
                key_definitions += '\n' \
                    + '%sALTERNATE RECORD KEY IS %s' % (' ' * 17, k) + '\n' \
                    + '%sWITH DUPLICATES' % (' ' * 23)

        return key_definitions

    def bind_template(self, template_file, vars):
        """
        Fill template variables in a template.
        """
        template = self.get_file_contents(template_file)
        for key in vars:
            template = template.replace('{{ ' + key + ' }}', str(vars[key]))

        tags = re.findall("\{\{\s+?(.+)\s+?\}\}", template)
        if len(tags) > 0:
            self.fail(_('ERROR: Template tags not binded: %s.').decode('utf-8') % ', '.join(tags))

        return template

    def get_output_file(self):
        """
        Remove variable values from the input arguments to generate a unique name
        for the program in the cache directory.
        """
        args_copy = self._args.copy()

        if 'keyvalue' in args_copy:
            del args_copy['keyvalue']

        if args_copy['operation'] in ('INSERT') and 'keyname' in args_copy:
            del args_copy['keyvalue']

        if args_copy['operation'] in ('INSERT', 'UPDATE'):
            for key in args_copy['input']:
                args_copy['input'][key] = ''

        hashstr = hashlib.md5(json.dumps(args_copy)).hexdigest()[:32]

        return CACHEDIR + hashstr.upper()


    def compile(self, vars, template_file, source_file, output_file):
        """
        Build and executes the compilation command.
        """
        binded_file = self.bind_template(template_file, vars)
        self.put_file_contents(binded_file, source_file)

        # build and execute the compilation command
        cmd = COMPILER_COMMAND % {
                'source_file': source_file,
                'output_file': output_file }
        stdout, stderr = self.syscall(cmd)

        # compilation error, parse output and send to screen
        if COMPILER_ERROR in stdout:
            errors = re.findall(COMPILER_ERROR_REGEX, stdout, re.MULTILINE)
            self.fail(_('ERROR: Compilation failed: %s.').decode('utf-8') % \
                ' // '.join([re.sub(r'\s+', ' ', e[1]) for e in errors]))

    def get_field_children_first_level(self, field):
        """
        Get the first level children of grouped fields.
        """
        if 'children' in self._copybook[field]:
            return self._copybook[field]['children']

        if 'pic_info' in self._copybook[field]:
            return list()

        children = list()
        i = self._copybook.keys().index(field) + 1
        level = self._copybook[field]['level']
        items = self._copybook.items()
        while i < len(items) and items[i][1]['level'] > level:
            if items[i][1]['level'] - 2 >= level + 2:
                i += 1
                continue
            children.append(items[i][1]['name'])
            i += 1

        return children

    def get_field_children_all_levels(self, field):
        """
        Get all levels children of grouped fields.
        """
        children = self.get_field_children_first_level(field)
        result = list()

        if len(children) == 0:
            return result

        for c in children:
            result = list(set(children + self.get_field_children_all_levels(c)))

        return result

    def get_field_type(self, field):
        """
        Will get the field PIC declaration or will get the first
        PIC type declaration with a the total length on a grouped field level.
        """
        if 'type' in self._copybook[field]:
            return self._copybook[field]['type']

        if self._copybook[field]['pic'] is not None:
            return self._copybook[field]['pic'][0]

        i = self._copybook.keys().index(field) + 1
        items = self._copybook.items()
        while items[i] is not None:
            if items[i][1]['pic'] is not None:
                return items[i][1]['pic'][0]
            i += 1

    def get_field_level(self, field):
        return self._copybook[field]['level']

    def get_field_length(self, field):
        """
        Will get the length of single fields or iterate on grouped field
        levels to get the total length.
        """
        if 'length' in self._copybook[field]:
            return self._copybook[field]['length']

        if 'pic_info' in self._copybook[field]:
            return self._copybook[field]['pic_info']['length']

        length = 0
        i = self._copybook.keys().index(field) + 1
        level = self._copybook[field]['level']
        items = self._copybook.items()
        while i < len(items) and items[i][1]['level'] > level:
            if 'pic_info' in items[i][1]:
                length += items[i][1]['pic_info']['length']
            i += 1

        return length

    def get_main_record(self):
        return self._copybook.items()[0][1]['name']

    def get_operation_vars(self, operation):
        """
        Fill template_vars with specific variables for each operation.
        """

        template_vars = {
            'COPYBOOK': self._args['copybook'],
            'DATAFILE': self._args['datafile'],
            'KEYDEFS': self.get_key_definitions(),
            'MAINRECORD': self.get_main_record()
        }

        # field sequence
        template_vars['FIELDS'] = ''
        for key in self._copybook:
            indent = ' ' * (6 + self._copybook[key]['level'])
            template_vars['FIELDS'] += '%s%02d %s%s.\n' % (indent,
                self._copybook[key]['level'], self._copybook[key]['name'],
                ' PIC ' + self._copybook[key]['pic'] if self._copybook[key]['pic'] is not None else '')

        # key parameters
        if operation in ('select', 'update', 'delete'):
            template_vars['KEYNAME'] = self._args['keyname']
            template_vars['KEYPIC'] = 'PIC %s(%02d)' % (
                self.get_field_type(self._args['keyname']),
                self.get_field_length(self._args['keyname']))
            template_vars['SUPERKEYNAME'] = self._args['superkeyname']
            template_vars['SUPERKEYPART'] = self._args['superkeypart']

        # display statements
        if operation in ('select'):
            if 'input' in self._args:
                display = []
                displayed_fields = []

                for key in self._args['input']:

                    if key in displayed_fields:
                        continue

                    display.append('%sDISPLAY %s WITH NO ADVANCING' % (' ' * 17, key))
                    displayed_fields = list(set(displayed_fields + self.get_field_children_all_levels(key)))

                # split one record per line
                display.append('%sDISPLAY ""' % (' ' * 17))
                template_vars['DISPLAY'] = '\n'.join(display)

            else:
                template_vars['DISPLAY'] = '%sDISPLAY %s' % (' ' * 17, self.get_main_record())

        # move statements
        if operation in ('insert', 'update'):
            if 'input' not in self._args:
                self.fail(_('ERROR: --input value required for %s operation.').decode('utf-8') % operation)

            args = []
            move = []

            for key in self._args['input']:
                args.append('%s03 ARG-%s PIC %s(%02d).' % (' ' * 12, key, self.get_field_type(key), self.get_field_length(key)))
                move.append('%sMOVE ARG-%s TO %s.' % (' ' * 12, key, key))

            template_vars['ARGS'] = '\n'.join(args)
            template_vars['MOVE'] = '\n'.join(move)

        return template_vars

    def build_field_params(self, fields):
        params = []
        for key in fields:
            params.append(fields[key].ljust(self.get_field_length(key), ' '))

        return ''.join(params)

    def parse_fields(self, selected_fields, fields, raw):
        """
        Recursively iterate on the raw line return by the Cobol program to
        fill the dictionary keys.
        """
        result = OrderedDict()

        if len(fields) == 0:
            return result

        offset = 0
        for field in fields:

            # field is already displayed and parsed
            if field in result or field not in selected_fields:
                continue

            length = self.get_field_length(field)
            value = raw[offset : offset + length]
            offset += length

            result[field] = value.strip()
            result.update(self.parse_fields(selected_fields, self.get_field_children_first_level(field), value))

        return result

    def parse_select_output(self, output):
        results = []

        if 'input' in self._args:
            fields = self._args['input']
        else:
            fields = self._copybook.keys()[1:]

        for line in output.splitlines():
            if line is None:
                continue

            result = self.parse_fields(fields, fields, line)
            results.append(result)

        return results

    def run_describe(self):
        describe = ''
        for key in self._copybook:
            describe += '%s%02d %s%s\n' % (' ' * self._copybook[key]['level'],
                self._copybook[key]['level'], self._copybook[key]['name'],
                ' PIC ' + self._copybook[key]['pic'] if self._copybook[key]['pic'] is not None else ' <%s>' % self.get_field_length(key))

        return describe

    def run_select(self, output_file):
        cmd = '%s "%s"' % (output_file, self._args['keyvalue'])
        stdout, stderr = self.syscall(cmd)
        if stdout:
            return json.dumps(self.parse_select_output(stdout))

    def run_insert(self, output_file):
        params = self.build_field_params(self._args['input'])
        cmd = '%s "%s"' % (output_file, ''.join(params))
        stdout, stderr = self.syscall(cmd)
        stdout = stdout.rstrip()
        if stdout:
            return stdout

    def run_update(self, output_file):
        params = self.build_field_params(self._args['input'])
        cmd = '%s "%s" "%s"' % (output_file, self._args['keyvalue'], ''.join(params))
        stdout, stderr = self.syscall(cmd)
        stdout = stdout.rstrip()
        if stdout:
            return stdout

    def run_delete(self, output_file):
        cmd = '%s "%s"' % (output_file, self._args['keyvalue'])
        stdout, stderr = self.syscall(cmd)
        stdout = stdout.rstrip()
        if stdout:
            return stdout

    def run(self, json_args = None):
        if not json_args:
            self._cli = True
            self.parse_args_stdin()
        else:
            try:
                self._args = json.loads(json_args)
            except ValueError:
                self.fail(_('ERROR: Could not parse JSON object input.').decode('utf-8'))

        self.check_args()

        if self._args['operation'] == 'describe':
            describe = self.run_describe()
            if self._cli:
                print(describe)
                return
            return json.dumps(describe)

        output_file = self.get_output_file()
        source_file = output_file + '.CBL'
        template_file = TEMPLATEDIR + self._args['operation'].upper() + '.CBL'

        if not CACHE_PROGRAMS or \
            not os.path.isfile(source_file) or \
            not os.path.isfile(output_file) or \
            os.path.getmtime(template_file) > os.path.getmtime(source_file) or \
            os.path.getmtime(__file__) > os.path.getmtime(source_file):
            template_vars = self.get_operation_vars(self._args['operation'])
            self.compile(template_vars, template_file, source_file, output_file)

        if self._args['operation'] == 'select':
            result = self.run_select(output_file)
        elif self._args['operation'] == 'insert':
            result = self.run_insert(output_file)
        elif self._args['operation'] == 'update':
            result = self.run_update(output_file)
        elif self._args['operation'] == 'delete':
            result = self.run_delete(output_file)

        if self._cli:
            print(result)
            return
        return result

if __name__ == "__main__":
    CobolApi().run()