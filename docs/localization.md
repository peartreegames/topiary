# Localization

:::warning

Localization is very experimental, even moreso than the rest of topiary

:::

There are five steps to adding localization to topiary. Validate - Export - Compile - Generate - Run

## Validation

Each dialogue line and choice can automatically be given an ID which will be
appended to the string content with an `@` by using the [cli](./cli.md) `loc validate` command.

eg. `:Speaker: "Dialogue line content"@12345678-ABCDEFGH`

## Export

Once a file has been updated with all necessary IDs a CSV file can be exported.
The CLI `loc export` command allows you to specify any output path, though keeping it relative to your source is recommended.

eg. `topi loc export hello.topi -o hello.topi.csv`

Once the file is exported, add the rest of the languages by adding a new column.
Each row will have the ID, the Speaker (or `CHOICE`), the raw text from topi, and the base language value.

## Generate

Once the CSV is filled out you can then generate individual `.topil` files for each language. 
(See below for file format)

`topi loc generate ./examples/locale/hello.topi.csv --folder ./examples/locale/`

## Run

When running topiary with the cli, pass in the `--locale-key-file <path>` flag

eg `topi run ./examples/locale/hello.topi --locale-key-file ./examples/locale/hello.fr_FR.topil`

## Tips

When writing with localization in mind you have to remember that
languages don't work the same. Before going further if you're
new to localization (as I still am), I recommend reading this fantastic article by
[multilingual.com](https://multilingual.com/articles/improving-translation-of-variables-in-interactive-games/).

Instead of encoding language pieces into string variables, Topiary opts for the 
simplest, most direct way; it doesn't localize strings, only dialogue Lines and Choices.
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
but with modern software like translation memory
and repetition analysis, it hopefully shouldn't be much of a problem.

:::

## File Format (.topil)

| Name        | Type    | Description    |
|-------------|---------|----------------|
| Entry Count | u32     | Count          |
| Index Table | Entry[] | Entry Mappings |
| Blob        | u8[]    | String data    |

### Entry

| Name   | Type   | Description   |
|--------|--------|---------------|
| UUID   | u8[17] | ID            |
| Offset | u32    | Blob Offset   |
| Length | u32    | String Length |