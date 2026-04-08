# Localization

There are five steps to adding localization to topiary: Stamp - Export - Translate - Generate - Run.

Only dialogue lines and choices are localized. Topiary does not localize
individual strings, variables, or expressions. This is by design — translators
work best with complete sentences, and fragment-based localization
(stitching translated pieces together) produces broken grammar across languages.

## Stamp

Each dialogue line and choice needs a unique `@ID` for the localization pipeline to track it.
Use the formatter to stamp all unstamped lines automatically:

```bash
topi fmt --stamp story.topi        # format and stamp
topi fmt --stamp-only story.topi   # stamp without reformatting
```

This appends an `@ID` to any dialogue or choice that doesn't already have one:

```topi
:Speaker: "Dialogue line content"@8R955KPX-2WI5R816
~ "A choice the player can make"@JTCCIIS7-NHTNWTBL
```

You can check for missing IDs without modifying the file:

```bash
topi loc validate story.topi
```

## Export

Once all lines are stamped, export a CSV file for translators:

```bash
topi loc export story.topi -l en -o story.topi.csv
```

The CSV contains one row per localizable line:

```csv
"id","speaker","raw","en"
"8R955KPX-2WI5R816","Speaker","Dialogue line content","Dialogue line content"
"JTCCIIS7-NHTNWTBL","CHOICE","A choice the player can make","A choice the player can make"
```

Add translation columns to the CSV for each target language. The `raw` column shows the
original source text with named variables (e.g. `{gold}`) for context — translators
work with the language column where variables appear as `{0}`, `{1}`, etc.

Re-exporting after code changes merges new/removed lines while preserving existing translations:

```bash
topi loc export story.topi -l en -o story.topi.csv  # merges with existing CSV
```

## Generate

Compile the translated CSV into binary `.topil` files for runtime:

```bash
topi loc generate story.topi.csv --folder locales/           # all languages
topi loc generate story.topi.csv --folder locales/ -k fr_FR  # single language
```

## Run

Pass a `.topil` file when running:

```bash
topi run story.topi --locale-key-file locales/story.fr_FR.topil
```

For game engine integration, use `vm.setLocale(path)` or `vm.setLocaleFromBuffer(key, buffer)`
to load translations at runtime.

## Writing for Localization

When writing with localization in mind, remember that languages don't
work the same way. If you're new to localization, this article by
[multilingual.com](https://multilingual.com/articles/improving-translation-of-variables-in-interactive-games/)
is a good introduction.

### Why only lines and choices?

Topiary localizes complete dialogue lines and choices — not individual strings or variables.
This means translators always see full sentences, which produces better translations.
A translator can restructure `"You found {0} on the beach"` into
`"Sur la plage, vous avez trouvé {0}"` naturally. Translators can reorder
`{0}`, `{1}` placeholders freely to match target language grammar.

Variables and expressions interpolated into dialogue (like `{gold}` or `{playerName}`)
are resolved at runtime and inserted into the translated template. Player-provided
values (names, inputs) work automatically since they're already in the player's language.

### Use descriptive variable names

Variable names from your source code appear in the CSV `raw` column as context
for translators. A translator seeing `{x}` has no idea what it represents, but
`{goldCount}` or `{islandName}` tells them immediately. This matters because
the same word can translate differently depending on context — for example,
"table" in English could be furniture or a data grid, and many languages use
entirely different words for each.

```topi
// Avoid — translators won't know what {n} or {s} mean
:Speaker: "You found {n} near the {s}"@12345678-ABCDEFGH

// Prefer — the raw column gives translators context
:Speaker: "You found {coinCount} near the {locationName}"@12345678-ABCDEFGH
```

### Plurals and grammatical variants

Don't try to build grammar with string interpolation. Instead, write
separate lines for each variant and let translators handle each one naturally:

```topi
// Don't do this — translators can't fix the grammar
:Speaker: "I have {gold} coin{if gold == 1 "" else "s"}"@12345678-ABCDEFGH

// Do this — each line is independently translatable
if gold == 1 {
    :Speaker: "I have {gold} coin"@12345678-ABCDEFGH
} else {
    :Speaker: "I have {gold} coins"@ABCDEFGH-12345678
}
```

The CSV gives translators complete sentences to work with:

```csv
"id","speaker","raw","en","zh-TW"
"12345678-ABCDEFGH","Speaker","I have {gold} coin","I have {0} coin","我有 {0} 元"
"ABCDEFGH-12345678","Speaker","I have {gold} coins","I have {0} coins","我有 {0} 元"
```

### Contextual or dynamic text

If a function returns text that changes based on state, don't interpolate it —
use `if` or `switch` so each variant is its own localizable line:

```topi
// Don't do this — the function's return value won't be localized
:: "{timeOfDayGreeting()}, how are you?"@12345678-ABCDEFGH

// Do this — each variant is a complete, translatable line
switch timeOfDay {
    Time.Morning: :: "Good morning, how are you?"@12345678-ABCDEFGH,
    Time.Afternoon: :: "Good afternoon, how are you?"@ABCDEFGH-12345678,
    Time.Evening: :: "Good evening, how are you?"@11111111-22222222
}
```

This is more verbose, but it produces better translations — in French,
"Good morning, how are you?" and "Good evening, how are you?" might use
entirely different phrasing, not just a different greeting word.

### Text variation (random, cycling, sequential)

For ambient text or narration that varies each time, use a counter variable
and a `switch` so each variant has its own `@ID`:

```topi
// cycle
var wind_idx = 0
=== CLIFFSIDE {
    switch wind_idx {
        0: :: "The wind howls through the rocks."@AAAAAAAA-BBBBBBBB,
        1: :: "A gust rattles the shutters."@CCCCCCCC-DDDDDDDD,
        2: :: "The wind shrieks like a wounded gull."@EEEEEEEE-FFFFFFFF
    }
    wind_idx = (wind_idx + 1) % 3
}
```

For random selection, use `rnd()`:

```topi
// random
var pick = rnd(0, 3)
switch pick {
    0: :: "Waves crash against the jetty."@11111111-22222222,
    1: :: "The sea churns below."@33333333-44444444,
    2: :: "Spray mists across the rocks."@55555555-66666666
}
```

For a sequence that plays in order and sticks on the last entry:

```topi
// sequence
var journal_idx = 0
switch journal_idx {
    0: :: "Day 12: The light must not go out."@AAAABBBB-CCCCDDDD,
    1: :: "Day 19: Someone has been in the radio room."@EEEEFFFF-GGGGHHHH,
    else: :: "Day 31: If you are reading this, I am already gone."@IIIIJJJJ-KKKKLLLL
}
if journal_idx < 2 journal_idx += 1
```

The `random()`, `cycle()`, `sequence()`, and `shuffle()` builtins are useful
for non-dialogue values (game logic, selecting numbers or objects), but for
player-visible text, prefer the `switch` pattern so each line is localizable.

::: Note

These patterns increase localized word count, but modern translation tools
with translation memory and repetition analysis handle this well — similar
lines are flagged as fuzzy matches and translate quickly.

:::

### Speaker names

Speaker identifiers (`:Mirren:`, `:Detective:`) are not localized by Topiary.
They are passed to the host game engine as-is. The game engine's own
localization system should handle display name translation — the speaker
identifier is a lookup key, not player-facing text.

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