# CLI

The CLI tool is a very simple runner that can be used as a starting point. 
It will compile a file, and run your story in the terminal. 
It is recommended to start here to see how you can use topiary in your projects.

To run the CLI:

```shell
topi - command line topiary processor
Usage:
   topi version                  Print version
   topi run <file>               Run dialogue in terminal
       -a, --auto                    Automatically continue to the next line
       -b, --bough <name>            Starting bough
       -c, --choices <i,j,...>       Take these choice indices in order, then prompt
       -j, --json                    JSON Lines transcript; stops at the first unscripted choice
       -k, --locale-key-file <file>  Localization key file
       -l, --load <file>             Read save from file on start
       -r, --rewind                  Enable rewind/redo at choice prompts (debug)
       -s, --save <file>             Write save to file on end
           --seed <n>                Seed the random builtins
       -v, --verbose
   topi test <file> <count>      Run dialogue <count> times, selecting random choices
       -b, --bough <name>            Starting bough
       -j, --json                    JSON visit counts and never-visited names
       -q, --quiet                   Do not output visit tree on end
           --seed <n>                Seed choice selection and the random builtins
       -v, --verbose
   topi fmt <file>               Format source file
       -d, --dry                     Print to stdout instead of writing
       -i, --indent <n>              Spaces per indent level (default: 4)
       --stamp                       Also insert @UUID tokens for unstamped nodes
       --stamp-only                  Insert @UUID tokens without reformatting
       -v, --verbose
   topi compile <file>           Compile dialogue to bytecode
       -d, --dry                     Do not write to file on end
       -j, --json                    JSON Lines diagnostics
       -o, --output <file>           Write to file on end
       -t, --time                    Print compile phase timings
       -v, --verbose
   topi loc validate <file>      Check for missing localization ids
       -d, --dry                     Do not write to file on end
       -v, --verbose
   topi loc export <file>        Export dialogue localization to csv
       -d, --dry                     Do not write to file on end
       -l, --lang <lang>             Base language column name (default: en)
           --no-merge                Overwrite existing csv instead of merging
       -o, --output <file>           Write to file on end
       -v, --verbose
   topi loc generate <file>      Export dialogue csv to topil files
       -d, --dry                     Do not write to file on end
       -f, --folder <folder>         Folder to output files
       -k, --locale-key              Generate only a specific localization key
       -v, --verbose
```

While running a topi file you can press `enter` for the next line, and select a choice with `0-9` then `enter`.

For scripts and agents, `topi run <file> -b <bough> --json -c 1,0,2` plays a fixed path and prints one JSON object per line: `line`, `choices` (with the `selected` index, or `null` where it stopped), then a final `end` with `status` (`finished`, `awaiting_choice`, `invalid_choice`, `error`, `compile_error`), visit counts and mutable globals. Add `--seed <n>` to make `rnd`/`random` repeatable.
