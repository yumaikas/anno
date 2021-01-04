# Anno


Anno is a tool for scanning a directory laid out according to the Johnny decimal system.

It's written in Janet, so you'll need a janet install as of janet 1.13 or later.

Right now, it recognizes the following annotations to text files:

```
@todo[Description here | due: Date in YYYY-mm-dd format]
@appt[Description here | date: Date in YYYY-mm-dd format]
@til[Description here  | on: Date in YYYY-mm-dd format]
```

It uses the JD_FOLDER environment variable to know where the JD folder structure is.

Example usage output:

```
Supported subcommands:\n
    areas             - Prints out a list of the JD areas in $JD_FOLDER
    debug-dump        - Prints out a list of all recognized annotations and their data
    agenda            - Prints out @todos and @appts (appointments)
    review            - Prints out @tils 
    help <subcommand> - Shows help for a given subcommand
```

# Status

This is currently in alpha, with hopes for it to be useful for my personal use.
