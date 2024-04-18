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


## Tips

When writing with localization in mind you have to remember that
languages don't work the same. Before going further if you're
new to localization (as I still am), I recommend reading this fantastic article by 
[multilingual.com](https://multilingual.com/articles/improving-translation-of-variables-in-interactive-games/).

Instead of encoding language pieces into string variables, Topiary opts for the
simplest most direct way, it doesn't localize strings, only dialogue Lines and Choices.
Full lines need to be rewritten instead of interpolating strings within strings.

```topi
// Instead of this
=== START {
    :Speaker: "I have {gold} coin{if gold == 1 "" else "s"}"@12345678-ABCEDEFGH
}

// You'll need to do this
=== START {
  if gold == 1 {
    :Speaker: "I have {gold} coin"@12345678-ABCEDEFGH
  } else {
    :Speaker: "I have {gold} coins"@ABCEDFGH-12345678
  }
}
```

That way we can have both lines localized appropriately.

```csv
"id","speaker","raw","en","zh-TW"
"12345678-ABCDEFGH","Speaker","I have {gold} coin","I have {0} coin","我有 {0} 元"
"ABCDEFGH-12345678","Speaker","I have {gold} coins","I have {0} coins","我有 {0} 元"
```

::: Note

This will increase the localized word count,
but with Translation Memory it shouldn't be much of a problem.

:::
