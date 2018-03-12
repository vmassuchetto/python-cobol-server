# -*- coding: utf-8 -*-
import os

APPNAME = 'cobolapi'
BASEDIR = os.path.dirname(os.path.abspath(__file__))
CACHEDIR = BASEDIR + '/cache/'
TEMPLATEDIR = BASEDIR + '/templates/'
LOCALESDIR = BASEDIR + '/locales/'

CACHE_COPYBOOKS = True
CACHE_PROGRAMS = True

COMPILER_COMMAND = 'env -i bash --noprofile --norc -c "cd `dirname \"%(output_file)s\"` 2>&1 && /opt/microfocus/VisualCOBOL/bin/cob -x -o \"%(output_file)s\" \"%(source_file)s\" 2>&1 && rm \"%(output_file)s.int\" \"%(output_file)s.idy\" \"%(output_file)s.o\" 2>&1"'
COMPILER_ERROR = 'cob64: error'
COMPILER_ERROR_REGEX = r'^(\*\*|\s\s)\s+(?P<error>.*)$'

OPERATIONS = ['describe', 'select', 'insert', 'update', 'delete']

SERVER_HOST = '0.0.0.0'
SERVER_PORT = 2501