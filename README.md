This is a general purpose service to perform basic operations on Cobol
datafiles using JSON or a command line interface. It will parse the copybooks
using [python-cobol](https://github.com/royopa/python-cobol) and dynamically
build a program to read or write the required data.

# Setup

All dependencies are bundled on `lib` (hard firewalls) and it is coded in
Python 2.6.6 (old mainframes).

# Usage

## Command line

Run `./api.py` without parameters to get the available options:

    Common options:

        --help          print this help message
        --operation     operation name: describe, select, insert, update, delete
                        (required)
        --copybook      path to the copybook file (required)
        --datafile      path to the datafile (required)
        --datafilekeys  comma separated list of keys, (required)
                        put the main key first and the alternate keys last
        --keyname       key to use in operation
        --keyvalue      value to match the key
        --input         input JSON object for the operation

    Partial search options:

        --superkeyname  super key in case of doing a partial search
        --superkeypart  part of --keyname in the --superkeyname as a
						[start:end] format

The `--input` parameter has a different purpose for each `--operation` values.

### Available operations

#### describe

Describe the parsed hierarchy of a Cobol copybook. You can use this to check
field lenghts and build queries. Groups have their lengths between `<>`.

    ./api.py \
        --operation="describe" \
        --copybook="/path/to/COPYBOOK.LIB"


```cobol
01 COPYBOOK <1005>
    03 REGNUMBER PIC 9(03)
    03 REGSEQUENCE PIC 9(03)
    03 REGTYPE PIC X(02)
    03 NAME <90>
        05 FIRSTNAME PIC X(45)
        05 LASTNAME PIC X(45)
    03 BIRTHDATE <8>
        05 BIRTHDATE-DAY PIC 9(02)
        05 BIRTHDATE-MONTH PIC 9(02)
        05 BIRTHDATE-YEAR PIC 9(04)
        ...
```

#### select

Supose *REGNUMBER* and *REGSEQUENCE* are the keys of a *DATAFILE.DAT* built
using *COPYBOOK.LIB*. To select a *REGNUMBER* equals to *123* and only some
fields:

    ./api.py \
        --operation="select" \
        --copybook="/path/to/COPYBOOK.LIB"
        --copybook="/path/to/DATAFILE.DAT"
        --datafilekeys="REGNUMBER,REGSEQUENCE" \
        --keyname="REGNUMBER" \
        --keyvalue="123" \
        --input="['REGNUMBER','NAME','FIRST NAME','BIRTHDATE']

```json
[
    {
        "REGNUMBER": "123",
        "NAME": "FIRST NAMELAST NAME",
        "FIRSTNAME": "FIRST NAME",
        "BIRTHDATE": "14021980"
    }
]
```

`--input` is optional. All fields would be returned if it is not provided.

##### Partial key searches

When you have a grouped key, it's possible to search its fields by providing
the field name on `--superkeyname` and the length of the subfield on
`--superkeypart`.

Suppose you have a grouped key field like this:

```cobol
01 COPYBOOK <1005>
    03 KEYFIELD
		03 FIRSTPART PIC 9(03)
		03 SECONDPART PIC 9(03)
		03 THIRDPART PIC 9(03)
```

You'll be able to return all the records that match only `FIRSTPART` by doing:

    ./api.py \
        --operation="select" \
        --copybook="/path/to/COPYBOOK.LIB"
        --copybook="/path/to/DATAFILE.DAT"
        --datafilekeys="REGNUMBER,REGSEQUENCE" \
        --superkeyname="KEYFIELD" \
		--superkeypart="[0:3]" \
		--keyname="FIRSTPART" \
        --keyvalue="123"

#### update

On updates the `--input` argument is a key-value JSON object for the values to
be updated on the record.

Following the examples above, to update the `FIRSTNAME` field for a record:

    ./api.py \
        --operation="update" \
        --copybook="/path/to/COPYBOOK.LIB"
        --copybook="/path/to/DATAFILE.DAT"
        --datafilekeys="REGNUMBER,REGSEQUENCE" \
        --keyname="REGNUMBER" \
        --keyvalue="123" \
        --input="{'FIRSTNAME': 'UPDATED FIRST NAME'}"

The operation is successfull if nothing gets returned.

#### insert

On inserts the `--input` argument is a key-value JSON object with the values
to be inserted. This object must include the datafile keys.

For example, to insert a new record:

    ./api.py \
        --operation="insert" \
        --copybook="/path/to/COPYBOOK.LIB"
        --copybook="/path/to/DATAFILE.DAT"
        --datafilekeys="REGNUMBER,REGSEQUENCE" \
        --input="{
			'REGNUMBER': '123',
			'REGSEQUENCE': '321',
			'REGTYPE': 'AB',
			'FIRSTNAME': 'FIRST NAME EXAMPLE',
			'LASTNAME': 'LAST NAME EXAMPLE',
		}"

The operation is successfull if nothing gets returned.
		
#### delete

It is similar to `select` without the `--input` argument. This will delete the
record with a "123" key:

    ./api.py \
        --operation="delete" \
        --copybook="/path/to/COPYBOOK.LIB"
        --copybook="/path/to/DATAFILE.DAT"
        --datafilekeys="REGNUMBER,REGSEQUENCE" \
        --keyname="REGNUMBER" \
        --keyvalue="123"

The operation is successfull if nothing gets returned.
		
# TCP Service

The `service.py` script can be used for a TCP service to perform the `api.py`
operations using JSON via a TCP socket:

    ./service.py [ start | stop | restart ] [ --debug ]

The arguments are the same in a key-value JSON object:

```json
{
    "operation": "select",
    "copybook": "/path/to/COPYBOOK.LIB",
    "datafile": "/path/to/DATAFILE.DAT",
    "datafilekeys": "REGNUMBER,REGSEQUENCE",
    "keyname": "REGNUMBER",
    "keyvalue": "123",
    "input": [
        "REGNUMBER",
        "FIRSTNAME"
    ]
}
```

Use `netcat` to test the service:

    echo "JSON" | netcat 192.168.1.21 2501

# Internationalization

The internationalization of error messages is done via gettext.

To generate the localization template:

    xgettext --language python api.py --output locales/cobolapi.pot

Merge the file with the existing one replacing `<language code>`:

    msgmerge --update --no-fuzzy-matching --backup off locales/<languge code>/LC_MESSAGES/cobolapi.po locales/cobolapi.pot

Generate the final localization file after editing `<language code>.po`:

    msgfmt locales/<language code>/LC_MESSAGES/cobolapi.po --output-file locales/<language code>/LC_MESSAGES/cobolapi.mo