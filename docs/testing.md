# AutoTestRunner

The topi-cli tool also includes an AutoTestRunner.
With it, you can run your story N times and print out the visit tree.
This is just a quick tester to make sure all paths are valid,
and that no path causes an error.

## Usage

`topi-cli [file] --auto [count]`

So the below example will run the examples/story.topi 100 times

```
zig build
./zig-out/bin/topi-cli ./examples/story.topi --auto 100`
```
