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
       -k, --locale-key-file <file>  Localization key file
       -l, --load <file>             Read save from file on start
       -s, --save <file>             Write save to file on end
       -v, --verbose
   topi test <file> <count>      Run dialogue <count> times, selecting random choices
       -q, --quiet                   Do not output visit tree on end
       -v, --verbose
   topi compile <file>           Compile dialogue to bytecode
       -d, --dry                     Do not write to file on end
       -o, --output <file>           Write to file on end
       -v, --verbose
   topi loc validate <file>      Validate dialogue localization ids
       -d, --dry                     Do not write to file on end
       -v, --verbose
   topi loc export <file>        Export dialogue localization to csv
       -d, --dry                     Do not write to file on end
       -o, --output <file>           Write to file on end
       -v, --verbose
   topi loc generate <file>      Export dialogue csv to topil files
       -d, --dry                     Do not write to file on end
       -f, --folder <folder>         Folder to output files
       -k, --locale-key              Generate only a specific localization key
       -v, --verbose
```

While running a topi file you can press `enter` for the next line, and select a choice with `0-9` then `enter`
