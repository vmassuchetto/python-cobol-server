import re

RESERVED_KEYWORDS = [ 'ACCEPT', 'ACCESS', 'ADD', 'ADDRESS', 'ADVANCING',
    'AFTER', 'ALL', 'ALPHABET', 'ALPHABETIC', 'ALPHABETIC-LOWER',
    'ALPHABETIC-UPPER', 'ALPHANUMERIC', 'ALPHANUMERIC-EDITED', 'ALSO', 'ALTER',
    'ALTERNATE', 'AND', 'ANY', 'APPLY', 'ARE', 'AREA', 'AREAS', 'ASCENDING',
    'ASSIGN', 'AT', 'AUTHOR', 'BASIS', 'BEFORE', 'BEGINNING', 'BINARY', 'BLANK',
    'BLOCK', 'BOTTOM', 'BY', 'CALL', 'CANCEL', 'CBL', 'CD', 'CF', 'CH',
    'CHARACTER', 'CHARACTERS', 'CLASS', 'CLASS-ID', 'CLOCK-UNITS', 'CLOSE',
    'COBOL', 'CODE', 'CODE-SET', 'COLLATING', 'COLUMN', 'COMMA', 'COMMON',
    'COMMUNICATION', 'COMP', 'COMP-1', 'COMP-2', 'COMP-3', 'COMP-4', 'COMP-5',
    'COMPUTATIONAL', 'COMPUTATIONAL-1', 'COMPUTATIONAL-2', 'COMPUTATIONAL-3',
    'COMPUTATIONAL-4', 'COMPUTATIONAL-5', 'COMPUTE', 'COM-REG', 'CONFIGURATION',
    'CONTAINS', 'CONTENT', 'CONTINUE', 'CONTROL', 'CONTROLS', 'CONVERTING',
    'COPY', 'CORR', 'CORRESPONDING', 'COUNT', 'CURRENCY', 'DATA', 'DATE-COMPILED',
    'DATE-WRITTEN', 'DAY', 'DAY-OF-WEEK', 'DBCS', 'DE', 'DEBUG-CONTENTS',
    'DEBUGGING', 'DEBUG-ITEM', 'DEBUG-LINE', 'DEBUG-NAME', 'DEBUG-SUB-1',
    'DEBUG-SUB-2', 'DEBUG-SUB-3', 'DECIMAL-POINT', 'DECLARATIVES', 'DELETE',
    'DELIMITED', 'DELIMITER', 'DEPENDING', 'DESCENDING', 'DESTINATION', 'DETAIL',
    'DISPLAY', 'DISPLAY-1', 'DIVIDE', 'DIVISION', 'DOWN', 'DUPLICATES', 'DYNAMIC',
    'EGCS', 'EGI', 'EJECT', 'ELSE', 'EMI', 'ENABLE', 'END', 'END-ADD', 'END-CALL',
    'END-COMPUTE', 'END-DELETE', 'END-DIVIDE', 'END-EVALUATE', 'END-IF', 'ENDING',
    'END-INVOKE', 'END-MULTIPLY', 'END-OF-PAGE', 'END-PERFORM', 'END-READ',
    'END-RECEIVE', 'END-RETURN', 'END-REWRITE', 'END-SEARCH', 'END-START',
    'END-STRING', 'END-SUBTRACT', 'END-UNSTRING', 'END-WRITE', 'ENTER', 'ENTRY',
    'ENVIRONMENT', 'EOP', 'EQUAL', 'ERROR', 'ESI', 'EVALUATE', 'EVERY',
    'EXCEPTION', 'EXIT', 'EXTEND', 'EXTERNAL', 'FALSE', 'FD', 'FILE',
    'FILE-CONTROL', 'FILLER', 'FINAL', 'FIRST', 'FOOTING', 'FOR', 'FROM',
    'FUNCTION', 'GENERATE', 'GIVING', 'GLOBAL', 'GO', 'GOBACK', 'GREATER',
    'GROUP', 'HEADING', 'HIGH-VALUE', 'HIGH-VALUES', 'ID', 'IDENTIFICATION', 'IF',
    'IN', 'INDEX', 'INDEXED', 'INDICATE', 'INHERITS', 'INITIAL', 'INITIALIZE',
    'INITIATE', 'INPUT', 'INPUT-OUTPUT', 'INSERT', 'INSPECT', 'INSTALLATION',
    'INTO', 'INVALID', 'INVOKE', 'I-O', 'I-O-CONTROL', 'IS', 'JUST', 'JUSTIFIED',
    'KANJI', 'KEY', 'LABEL', 'LAST', 'LEADING', 'LEFT', 'LENGTH', 'LESS', 'LIMIT',
    'LIMITS', 'LINAGE', 'LINAGE-COUNTER', 'LINE', 'LINE-COUNTER', 'LINES',
    'LINKAGE', 'LOCAL-STORAGE', 'LOCK', 'LOW-VALUE', 'LOW-VALUES', 'MEMORY',
    'MERGE', 'MESSAGE', 'METACLASS', 'METHOD', 'METHOD-ID', 'MODE', 'MODULES',
    'MORE-LABELS', 'MOVE', 'MULTIPLE', 'MULTIPLY', 'NATIVE', 'NATIVE_BINARY',
    'NEGATIVE', 'NEXT', 'NO', 'NOT', 'NULL', 'NULLS', 'NUMBER', 'NUMERIC',
    'NUMERIC-EDITED', 'OBJECT', 'OBJECT-COMPUTER', 'OCCURS', 'OF', 'OFF',
    'OMITTED', 'ON', 'OPEN', 'OPTIONAL', 'OR', 'ORDER', 'ORGANIZATION', 'OTHER',
    'OUTPUT', 'OVERFLOW', 'OVERRIDE', 'PACKED-DECIMAL', 'PADDING', 'PAGE',
    'PAGE-COUNTER', 'PASSWORD', 'PERFORM', 'PF', 'PH', 'PIC', 'PICTURE', 'PLUS',
    'POINTER', 'POSITION', 'POSITIVE', 'PRINTING', 'PROCEDURE',
    'PROCEDURE-POINTER', 'PROCEDURES', 'PROCEED', 'PROCESSING', 'PROGRAM',
    'PROGRAM-ID', 'PURGE', 'QUEUE', 'QUOTE', 'QUOTES', 'RANDOM', 'RD', 'READ',
    'READY', 'RECEIVE', 'RECORD', 'RECORDING', 'RECORDS', 'RECURSIVE',
    'REDEFINES', 'REEL', 'REFERENCE', 'REFERENCES', 'RELATIVE', 'RELEASE',
    'RELOAD', 'REMAINDER', 'REMOVAL', 'RENAMES', 'REPLACE', 'REPLACING', 'REPORT',
    'REPORTING', 'REPORTS', 'REPOSITORY', 'RERUN', 'RESERVE', 'RESET', 'RETURN',
    'RETURN-CODE', 'RETURNING', 'REVERSED', 'REWIND', 'REWRITE', 'RF', 'RH',
    'RIGHT', 'ROUNDED', 'RUN', 'SAME', 'SD', 'SEARCH', 'SECTION', 'SECURITY',
    'SEGMENT', 'SEGMENT-LIMIT', 'SELECT', 'SELF', 'SEND', 'SENTENCE', 'SEPARATE',
    'SEQUENCE', 'SEQUENTIAL', 'SERVICE', 'SET', 'SHIFT-IN', 'SHIFT-OUT', 'SIGN',
    'SIZE', 'SKIP1', 'SKIP2', 'SKIP3', 'SORT', 'SORT-CONTROL', 'SORT-CORE-SIZE',
    'SORT-FILE-SIZE', 'SORT-MERGE', 'SORT-MESSAGE', 'SORT-MODE-SIZE',
    'SORT-RETURN', 'SOURCE', 'SOURCE-COMPUTER', 'SPACE', 'SPACES',
    'SPECIAL-NAMES', 'STANDARD', 'STANDARD-1', 'STANDARD-2', 'START', 'STATUS',
    'STOP', 'STRING', 'SUB-QUEUE-1', 'SUB-QUEUE-2', 'SUB-QUEUE-3', 'SUBTRACT',
    'SUM', 'SUPER', 'SUPPRESS', 'SYMBOLIC', 'SYNC', 'SYNCHRONIZED', 'TABLE',
    'TALLY', 'TALLYING', 'TAPE', 'TERMINAL', 'TERMINATE', 'TEST', 'TEXT', 'THAN',
    'THEN', 'THROUGH', 'THRU', 'TIME', 'TIMES', 'TITLE', 'TO', 'TOP', 'TRACE',
    'TRAILING', 'TRUE', 'TYPE', 'UNIT', 'UNSTRING', 'UNTIL', 'UP', 'UPON',
    'USAGE', 'USE', 'USING', 'VALUE', 'VALUES', 'VARYING', 'WHEN',
    'WHEN-COMPILED', 'WITH', 'WORDS', 'WORKING-STORAGE', 'WRITE', 'WRITE-ONLY',
    'ZERO', 'ZEROES', 'ZEROS']

class CobolPatterns:
    row_pattern_base = r'^(FD\s+(?P<file_descriptor>\S+)|(?P<level>\d{2})\s+(?P<name>\S+))?'
    row_pattern_occurs = r'(\s+OCCURS\s+(?P<occurs>\d+)\s+TIMES)?'
    row_pattern_indexed_by = r"(\s+INDEXED BY\s+(?P<indexed_by>\S+))?"
    row_pattern_redefines = r"(\s+REDEFINES\s+(?P<redefines>\S+))?"
    row_pattern_pic = r'(\s+PIC\s+(?P<pic>\S+))?'
    row_pattern_end = r'\.$'

    row_pattern = re.compile(row_pattern_base + 
                             row_pattern_redefines + 
                             row_pattern_indexed_by + 
                             row_pattern_pic +
                             row_pattern_occurs + 
                             row_pattern_end)

    pic_pattern_repeats = re.compile(r'(.)\((\d+)\)')
    pic_pattern_float = re.compile(r'S?[9Z]*[.V][9Z]+')
    pic_pattern_integer = re.compile(r'S?[9Z]+')


# Parse the pic string
def parse_pic_string(pic_str):
    # Expand repeating chars
    while True:
        match = CobolPatterns.pic_pattern_repeats.search(pic_str)

        if not match:
            break
        
        expanded_str = match.group(1) * int(match.group(2))
        
        pic_str = CobolPatterns.pic_pattern_repeats.sub(expanded_str, pic_str, 1)

    # Match to types
    if CobolPatterns.pic_pattern_float.match(pic_str):
        data_type = 'Float'
    elif CobolPatterns.pic_pattern_integer.match(pic_str):
        data_type = 'Integer'
    else:
        data_type = 'Char'

    # Handle signed
    if pic_str[0] == "S":
        data_type = "Signed " + data_type
        pic_str = pic_str[1:]

    # Handle precision
    decimal_pos = 0

    if 'V' in pic_str:
        decimal_pos = len(pic_str[pic_str.index('V') + 1 :])
        pic_str = pic_str.replace('V', '')

    return {'type':data_type, 'length':len(pic_str), 'precision':decimal_pos}

# Cleans the COBOL by converting the cobol informaton to single lines
def clean_cobol(lines):
    holder = []

    output = []

    for row in lines:            
        row = row[6:72].rstrip()

        if row == "" or row[0] in ('*','/'):
            continue

        holder.append(row if len(holder) == 0 else row.strip())

        if row[-1] == ".":
            output.append(" ".join(holder))

            holder = []
            

    if len(holder) > 0:
        print "[WARNING] probably invalid COBOL - found unfinished line: ", " ".join(holder)

    return output

"""
Parses the COBOL
 - converts the COBOL line into a dictionarty containing the information
 - parses the pic information into type, length, precision 
 - handles redefines
"""
def parse_cobol(lines, skip_redefines = False):
    output = []
    redefines_level = 1000

    intify = ["level","occurs"]

    # All in 1 line now, let's parse
    for row in lines:

        match = CobolPatterns.row_pattern.match(row.strip())
        
        if not match:
            print "Found unmatched row", row.strip()
            continue

        match = match.groupdict()

        if match['file_descriptor'] is not None:
            continue

        if int(match['level']) > redefines_level:
            continue
        else:
            redefines_level = 1000

        for i in intify:
            match[i] = int(match[i] ) if match[i] is not None else None

        if match['pic'] is not None:
            match['pic_info'] = parse_pic_string(match['pic'])

        if match['redefines'] is not None:
            
            if skip_redefines:
                redefines_level = match['level']
                continue

            # Find item that is being redefined.
            try:
                redefinedItemIndex, redefinedItem = [(index, item) for index, item in enumerate(output) if item['name'] == match['redefines']][0]

                related_group = get_subgroup( redefinedItem['level'] , output[ redefinedItemIndex+1 : ] )
                
                output = output[:redefinedItemIndex] + output[ redefinedItemIndex + len(related_group) + 1 : ]

                match['redefines'] = None
            except IndexError:
                print "Could not find the field to be redefined (%s) for row: %s" % (match['redefines'], row.strip())

        output.append(match)

    return output

# Helper function
# Gets all the lines that have a higher level then the parent_level until
# a line with equal or lower level then parent_level is encountered
def get_subgroup(parent_level, lines):
    output = []

    for row in lines:
        if row["level"] > parent_level:
            output.append(row)
        else:
            return output

    return output

def denormalize_cobol(lines):
    return handle_occurs(lines, 1)

# Helper function
# Will go ahead and denormalize the COBOL
# Beacuse the OCCURS are removed the INDEXED BY will also be removed
def handle_occurs(lines, occurs, level_diff=0, name_postfix=""):
    output = []

    for i in range(1, occurs+1):
        
        skipTill = 0
        new_name_postfix = name_postfix if occurs == 1 else name_postfix + '-' + str(i)

        for index, row in enumerate(lines):
            if index < skipTill:
                continue

            new_row = row.copy()

            new_row['level'] += level_diff

            # Not needed when flattened
            new_row['indexed_by'] = None

            if row['occurs'] is None:
                # First time occurs is just 1, we don't want to add _1 after *every* field
                new_row['name'] = row['name'] + new_name_postfix
                # + "-" + str(i) if occurs > 1 else row['name'] + name_postfix

                output.append(new_row)
            
            else:
                if row["pic"] is not None:
                    # If it has occurs and pic just repeat the same line multiple times
                    new_row['occurs'] = None

                    for j in range(1, row["occurs"]+1):
                        row_to_add = new_row.copy()

                        # First time occurs is just 1, we don't want to add _1 after *every* field
                        row_to_add["name"] = row['name'] + new_name_postfix + '-' + str(j)
                        # + "-" + str(i) + "-" + str(j) if occurs > 1 else row['name'] + name_postfix + "-" + str(j) 

                        output.append(row_to_add)

                else:
                    # Get all the lines that have to occur
                    occur_lines = get_subgroup(row['level'], lines[index+1:])

                    # Calculate the new level difference that has to be applied
                    new_level_diff = level_diff + row['level'] - occur_lines[0]['level']

                    output += handle_occurs(occur_lines, row['occurs'], new_level_diff, new_name_postfix)

                    skipTill = index + len(occur_lines) + 1

    return output

"""
Clean the names.

Options to:
 - strip prefixes on names
 - enforce unique names
 - make database safe names by converting - to _
"""
def clean_names(lines, ensure_unique_names=False, strip_prefix=False, make_database_safe=False):
    names = {}

    for row in lines:
        if strip_prefix:
            row['name'] = row['name'][ row['name'].find('-')+1 : ]

            if row['name'] in RESERVED_KEYWORDS:
                row['name'] += '-R'

            if row['indexed_by'] is not None:
                row['indexed_by'] = row['indexed_by'][ row['indexed_by'].find('-')+1 : ]

        if ensure_unique_names:
            i = 1
            while (row['name'] if i == 1 else row['name'] + "-" + str(i)) in names:
                i += 1

            names[row['name'] if i == 1 else row['name'] + "-" + str(i)] = 1

            if i > 1:
                row['name'] = row['name'] + "-" + str(i)

        if make_database_safe:
            row['name'] = row['name'].replace("-","_")


    return lines

def process_cobol(lines):
    return clean_names(denormalize_cobol(parse_cobol(clean_cobol(lines))), True, True, True)    

# Prints a Copybook compatible file
def print_cobol(lines):
    output = []

    default_padding = ' ' * 7

    levels = [0]

    for row in lines:
        row_output = []

        if row['level'] > levels[-1]:
            levels.append(row['level'])
        else:
            while row['level'] < levels[-1]:
                levels.pop()

        row_output.append( (len(levels)-1) * '  ' )
        row_output.append( "{0:02d}  ".format(row['level']) )
        row_output.append( row['name'])

        if row['indexed_by'] is not None:
            row_output.append(" INDEXED BY "+row['indexed_by'])

        if row['occurs'] is not None:
            row_output.append( " OCCURS {0:04d} TIMES".format(row['occurs']) )

        if row['pic'] is not None:
            row_output.append( " PIC " + row['pic'] )

        row_output.append(".")

        tot_length = 0
        max_data_length = 66
        outp = default_padding

        for data in row_output:

            if len(outp) + len(data) + 1 > max_data_length:
                # Makes rows 80 chars
                outp += (80-len(outp)) * ' '

                output.append(outp)

                # Start the following line with an extra padding
                outp = default_padding + (len(levels)-1) * '  ' + '    '

            outp += data

        outp += (80-len(outp)) * ' '
        output.append(outp)

    print "\n".join(output)

if __name__ == '__main__':
    import argparse
    import os.path

    parser = argparse.ArgumentParser(description="Parse COBOL Copybooks")
    parser.add_argument("filename", help="The filename of the copybook.")
    parser.add_argument("--skip-all-processing", help="Only processes the redefines.", default=False, action="store_true")
    parser.add_argument("--skip-redefines", help="Skip redefined items.", default=False, action="store_true")
    parser.add_argument("--skip-unique-names", help="Skips making all names unique.", default=False, action="store_true")
    parser.add_argument("--skip-denormalize", help="Skips denormalizing the COBOL.", default=False, action="store_true")
    parser.add_argument("--skip-strip-prefix", help="Skips stripping the prefix from the names.", default=False, action="store_true")

    args = parser.parse_args() 

    if not os.path.isfile(args.filename):
        print "Could not find", args.filename
        exit()

    with open(args.filename,'r') as f:
        lines = parse_cobol(clean_cobol(f.readlines(), args.skip_redefines))

        if not args.skip_all_processing:
            if not args.skip_denormalize:
                lines = denormalize_cobol(lines)

            if not args.skip_strip_prefix or not args.skip_unique_names:
                lines = clean_names(lines, not args.skip_unique_names, not args.skip_strip_prefix)

        print_cobol(lines)
