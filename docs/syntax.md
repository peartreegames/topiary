# Syntax

## Dialogue

### Boughs

Sections of dialogue are called Boughs, and are denoted with `=== [Name] {}`. 
So `=== START {}` is a bough called `START` with an empty "body".

### Lines

Dialogue lines have three sections. `:[Speaker]: "[Content]" #[Tag]`

Speakers are optional and can be empty with `::`. ex: `:: "A godlike voice echoes from the heavens."`

Tags can are also optional and can be omitted or multiple can be separated each with their own hash (`#`).

### Jumps

Boughs can be started with jumps, denoted with `=> [Name]`.

So with that we can write a very simple program.

```topi
=== START {
    :John: "Hello Jane!" #greet
    :Jane: "Great to see you, John"
}

=> START
```

### Nesting

Boughs can be nested and jumped to with `.` like so:

```topi
=== START {
    === INNER {
        :Speaker: "Start here"
    }
}
=> START.INNER
```

### Forks

To create interesting and interactive stories we need to be able to divert the story depending on choices answered by the player. For that we use the keyword `fork` with each prong denoted with a tilde `~`.

Fork bodies can be either a jump or a block surrounded by braces.

```topi
=== START {
    fork {
        ~ "Easy route" {
            :John: "Nice and easy"
        }
        ~ "Hard route" => END
    }
}

=== END {
    :John: "This is hard"
}

=> START
```

Forks can also be named to revisit, useful if wanting to loop choices.
```topi
=== START {
    fork DIFFICULTY { 
        ~ "Easy route" {
            :John: "Maybe this is too easy..."
            => DIFFICULTY
        }
        ~ "Hard route" => END
    }
}
=== END {
    :John: "The hard way it is"
}
=> START
```

### Flow and Jump Back Ups

If the flow of the story hits a closing backet it will end. 
But sometimes we want to temporarily duck into a bough and then continue on.
We can do that by adding a caret `^` to a jump name `=> [NAME]^` (meaning jump then come back up).

```topi
=== NO_BACKUP {
    :John: "Hello Jane!" #greet
    => JUMP
    :John: "How've you been?" // This will never be executed.
}
=> NO_BACKUP

=== WITH_BACKUP {    
    :John: "Hello Jane!" #greet
    => JUMP^
    :John: "How've you been?" // This will be executed.
}
=> WITH_BACKUP

=== JUMP {
    :Jane: "Great to see you, John"
}
```

Backups can also be applied to forks with `fork^` on anonymous forks and `fork [NAME]^` on named forks.

```topi
=== START {
    :Jane: "Which way do you want to go?"
    fork^ {
        ~ "Easy route" {
            :John: "Nice and easy"
        }
        ~ "Hard route" {
            :John: "Nothing easy was ever worth doing."
        }
    }
    :Jane: "Good choice."
}
=> START
```


## Code

### Variables

Variables can either be mutable `var` or constant `const`. Constant variables cannot be reassigned.
```topi
var value = 0
value = 5 // okay

const constant = 0
constant = 5 // error
```

### Types

Topi supports the following types
 - `bool` - `true/false`
 - `string` - `"text"`
 - `number` - `1`,`1.1`,`-2.2`

### Collections
Topi has three builtin collection types. Lists, Sets, and Maps. Each type has four builtin methods attached as well. 
`add`, `remove`, `has`, `clear`, `count`

Lists are a collections of values in order.
```
var list = ["one", "two", "three"] // List["one", "two", "three"]
var emptyList = []

list.add("four") // List["one", "two", "three", "four"]
list.remove("one") // List["two", "three", "four"]
list.count() // 3
list.has("two") // true
list.clear() // List[]
```

Sets are a collection of unique values.

```
var set = {"one", "two", "one"} // Set{one, two}
const emptySet = {}

set.add("two") // Set{one, two}
set.count() // 2
set.remove("one") // Set{two}
set.has("one") // false
set.clear() // Set{}
```

Maps are a collection of key/value pairs.
```
const map = {"one": 1, "two": 2} // Map{one:1, two:2}
const emptyMap = {:}

map.add("one", 3) // Map{one:3, two:2} (add will replace values if already exists)
map.count() // 2
map.remove("one") // Map{two:2}
map.has('two") // true
map.clear() // Map{}
```

### Loops

#### While

While loops will execute so long as the condition is met. However there is an internal limit of 100,000 to catch infinite loops. This can be adjusted by setting `Topiary.MaxWhile = -1 // no limit`
```topi
var i = 0
while i < 10 {
    print(i)
    i += 1
}
```

#### For

For loops can be a range (inclusive), or a collection. The item the loop is on is declared within pipes (`|`) after the iterator
```topi
for 0..10 |i| {
    print(i) // 0,1,2,3,4,5,6,7,8,9,10
}
```

```topi
var list = ["one", "two", "three"]
for list |item| {
    print(item) // one, two, thee
}
```

```topi
var set = {"one", "two", "one"}
for set |item| {
    print(item) // one, two
}
```

```topi
var map = {"one":1.1, "two":1.2, "three":1.3}
for map |item| {
    print(item.key) // one, two, three
    print(item.value) // 1.1, 1.2, 1.3
}
```

### Control flow 

Code and dialogues can be conditional with `if/else`.
Braces are optional if only one line is used.
```topi
var value = 0
if (value < 10) value = 11
else if (value > 10) value = 5
else value = -5
print(value) // 11
```

Switch statements can also be used. 
Breaks are implied on the first case which matches. 
Multiple conditions can be matched with commas (`,`), 
and if using numbers Ranges can also be used.

Again braces are optional if only one line is used.
```topi
// basic switch
switch(""string"") {
    ""string"": print(""switch here"")
    ""number"": print(""switch not here"")
    else: print(""switch else not here"")
}

// range cases
switch(2) {
    -6..-1: print(""switch not here"")
    0..5: print(""switch here"")
    6..10: print(""switch not here"")
    else: print(""switch else not here"")
}

// multiple matches
switch(5) {
    2,3,4: print(""switch not here"")
    5,6,7: print(""switch here"")
    else: print(""switch else not here"")
}

switch(""else"") {
    ""string"": print(""switch not here"")
    ""number"": print(""switch not here"")
    else: print(""switch else here"")
```

### Inline Code

Code can be executed within a dialogue line with braces `{}`.
```topi
var greeting = "Howdy"
const value = 42
=== START {
    :John: "{greeting}, Jane!" // Howdy, Jane!
    greeting = "Hello"
    :Jane: "{greeting}, John. The password is {value}." // Hello, John. The password is 42.
}
=> START
```

### Functions

Functions are declared same as variables, with parameters between pipes (`|`)
Braces are optional if only one line is used.

**Functions cannot contain Boughs or Jumps, only logic.**

```topi
const sum = |x, y| return x + y

const fib = |n| {
  if (n < 2) return n
  return fib(n - 1) + fib(n - 2)
}
```

### Enums

Enums are pretty standard

```topi
enum Cardinal {
    North,
    East,
    South,
    West
}

var direction = Cardinal.North
```

### Classes

Classes are an encapsulation of named data. 
All fields must be given a default value.
Instances of classes are created with the `new` keyword.
Any field not initialized will use the default value.


```topi
struct Person {
    age = 25,
    name = ""
}

var john = new Person {
    name = "John Doe"
}

print(john) // person{name = "John Doe", age = 25}
```

Classes can also have functions as fields, 
references to its own fields can be achieved with `self`

```topi
class Person {
    age = 0,
    firstName = "",
    lastName = "",
    fullName = || return self.firstName + " " + self.lastName,
    increaseAge = |amount| {
        self.age += amount
    }
}

var john = new Person {
    age = 25,
    firstName = "John",
    lastName = "Doe"
}

print(john.fullName()) // "John Doe"
john.increaseAge(2)
print(john.age) // 27
```
