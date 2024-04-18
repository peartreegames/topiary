# Localization

:::warning

Localization is very experimental, even moreso than the rest of topiary

:::

There are four steps to adding localization to topiary. Validate - Export - Compile - Run

## Validation

Each piece dialogue line and choice can automatically be given an ID which will be
appended to the string content with an `@` by using the [cli](./cli.md) `loc validate` command.

eg. `:Speaker: "Dialogue line content"@12345678-ABCDEFGH`

## Export

Once a file has been updated with all necessary IDs a CSV file can be exported.
Currently the name and path of the CSV *MUST* be the same of the tile with `.csv` appended.

eg. `hello.topi` > `hello.topi.csv`

Once the file is exported add the rest of the languages by adding a new column.
Each row will have the ID, the Speaker (or `CHOICE`), the raw text from topi, and the base language value.

## Compile

When compiling pass in the `--loc` flag to specify localization is being used.
This will append all the CSV files used for the module at the end of the bytecode.

:::note

This can obviously duplicate a lot of lines and data.
Another option would be to let the client handle keeping track of and
collating the CSVs for the module, and providing them to the VM.

However since we only load each Dialogue as needed we're mostly using
disk space and not much memory.

:::

## Run

When running topiary pass in the `--loc language-key` flag

eg `topi run ./examples/locale/locale.topi --loc fr`

