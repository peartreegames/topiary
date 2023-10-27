# AutoTestRunner

The build step will also build a topi-auto cli tool.
With it you can run your story N times and print out the visit tree.
This is just a quick tester to make sure all paths are valid,
and that no path causes an error.

## Usage

`topi-auto [file] [count]`

So the below example will run the examples/story.topi 100 times

```
zig build
./zig-out/bin/topi-auto ./examples/story.topi 100`
```
