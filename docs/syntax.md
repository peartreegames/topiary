# Syntax

## Dialogue 

### Boughs

Sections of dialogue are called Boughs, and are denoted with `=== [Name] {}`. 
So `=== START {}` is a bough called `START` with an empty "body".

### Lines

Lines have three sections. `:[Speaker]: "[Content]" #[Tag]`

Speakers are optional and can be empty with `::`. ex: `:: "A godlike voice echoes from the heavens."`

Tags can are also optional and can be omitted or multiple can be separated each with their own hash (`#`).

### Jumps

Boughs can be started with jumps, denoted with `=> [Name]`. The first jump must be passed in to the `start` function,
or if using the CLI the second parameter of the run command.

```topi
# topi run file.topi START

=== START {
    :John: "Hello Jane!" #greet
    :Jane: "Great to see you, John"
}
```

#### Automatic Jump Starts

As a convenience, if you don't pass in a jump to the CLI the first bough in the file will be used. 

```topi
# topi run file.topi // START will run in the above example
```

Be careful, if you include other files the "first" bough will be the first in the other file.

### Nesting

Boughs can be nested and jumped to with `.` like so:

```topi
# topi run file.topi START.INNER

=== START {
    === INNER {
        :Speaker: "Start here"
    }
}
```

### Forks

To create interesting and interactive stories we need to be able to divert the story depending on choices answered by the player. 
For that we use the keyword `fork` with each prong denoted with a tilde `~`.

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
```

Choices can also be unique with `~*`, this means once they are visited, 
they won't be added to the choice list again.


```topi
=== START {
    fork DIFFICULTY { 
        // can only be chosen once
        ~* "Easy route" {
            :John: "Maybe this is too easy..."
            => DIFFICULTY
        }
        ~ "Hard route" => END
    }
}
=== END {
    :John: "The hard way it is"
}
```

Choices can also be named to get the visit count in the story, 
but all choices will have a `visit_count` field regardless of being named or not, 
to be used in your runner code.

```topi
=== START {
    fork DIFFICULTY {
        ~ EASY "Easy route" {
            :John: "Maybe this is too easy..."
            => DIFFICULTY
        }
        ~ HARD "Hard route" => END
    }
}
=== END {
    :John: "The hard way it is"
    print(START) // 1
    print(START.DIFFICULTY.EASY) // everytime EASY was chosen
    print(START.DIFFICULTY.HARD) // everytime HARD was chosen
}
```

Lastly you can also tag choices. 
This is useful for things like displaying a choice in a different way to signify various outcomes or
that it will end a particular conversation or not

```topi
=== START {
    fork DIFFICULTY {
        ~ "Easy route" #repeat {
            :John: "Maybe this is too easy..."
            => DIFFICULTY
        }
        ~ "Hard route" #final => {}
    }
}
```

### Visits

All boughs, forks, and choices have visit counts. 
If a choice is not in a named fork its path is the fork index within the bough preceeded by an underscore `_0`, `_1`, etc
(but this is not recommended, best to give it a name).

```topi
=== START {
    // Not Recommended
    fork^ {
        // START._0.ONE
        ~ ONE "Pick one" {}
        // START._0.TWO
        ~ TWO "Pick two" {}
    }
    // Not Recommended
    fork^ {
        // START._1.ONE
        ~ ONE "Pick one" {}
        // START._1.TWO
        ~ TWO "Pick two" {}
    }
    // Recommended if choice visit count is needed
    fork NAMED {
        // START.NAMED.ONE
        ~ ONE "Pick one" {}
        // START.NAMED.TWO
        ~ TWO "Pick two" {}
    }
}
```

Visit paths can be found within scopes and don't need the full path written out.

```topi
=== START {
    :speaker: "Starting"
    fork NAMED {
        ~ ONE "Answer one" {
            :speaker: "You chose one"
            print("ONE={ONE}")
            print("TWO={NAMED.TWO}")
        }
        ~ TWO "Answer two" {
            :speaker: "You chose two"
            if TWO > 1 :speaker: "You've already been here"
            print("ONE={NAMED.ONE}")
            print("TWO={TWO}")
        }
    }
}
```

### Flow, Fin, and Jump Back Ups

If the flow of the story hits a closing backet or `fin`, it will end. 
`fin` can be useful to break out of a bough early.

```topi
=== START {
    if true fin
    :: "Will not get here"
}
```

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
However named forks with backups should be used with caution.

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

 - `bool`: `true/false`
 - `string`: `"text"`
 - `number`: `1`,`1.1`,`-2.2`

### Collections
Topi has three builtin collection types. Lists, Sets, and Maps. Each type has five builtin methods attached as well. 
`add`, `remove`, `has`, `clear`, `count`

Lists are a collections of values in order.

```topi
var list = List{"one", "two", "three"} // List{"one", "two", "three"}
var emptyList = List{}

list.add("four") // List{"one", "two", "three", "four"}
list.remove("one") // List{"two", "three", "four"}
list.count() // 3
list.has("two") // true
list.clear() // List{}
```

Sets are a collection of unique values.

```topi
var set = Set{"one", "two", "one"} // Set{one, two}
const emptySet = Set{}

set.add("two") // Set{one, two}
set.count() // 2
set.remove("one") // Set{two}
set.has("one") // false
set.clear() // Set{}
```

Maps are a collection of key/value pairs.

```topi
const map = Map{"one": 1, "two": 2} // Map{one:1, two:2}
const emptyMap = Map{}

map.add("one", 3) // Map{one:3, two:2} (add will replace values if already exists)
map.count() // 2
map.remove("one") // Map{two:2}
map.has("two") // true
map.clear() // Map{}
```

### Loops

#### While

While loops will execute so long as the condition is met. 

```topi
var i = 0
while i < 10 {
    print(i)
    i += 1
}
```

#### For

For loops can be a range (inclusive), or a collection. 
The item the loop is on is declared within pipes (`|`) after the iterator

```topi
for 0..10 |i| {
    print(i) // 0,1,2,3,4,5,6,7,8,9,10
}
```

```topi
var list = List{"one", "two", "three"}
for list |item| {
    print(item) // one, two, thee
}
```

```topi
var set = Set{"one", "two", "one"}
for set |item| {
    print(item) // one, two
}
```

```topi
var map = Map{"one":1.1, "two":1.2, "three":1.3}
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
if value < 10 value = 11
else if value > 10 { value = 5 }
else value = -5
print(value) // 11
```

Switch statements can also be used.
 
Breaks are implied on the first case which matches. 
Multiple conditions can be matched with commas (`,`), 
and if using numbers Ranges (`n..m`) can also be used.

Again braces are optional if only one line is used.
else statements are optional, if no prong matches it will be skipped.

```topi
// basic switch
switch "string" {
    "string": print("switch here"),
    "number": print("switch not here"),
    else: print("switch else not here")
}

// range cases
switch 2 {
    -6..-1: print("switch not here"),
    0..5: print("switch here"),
    6..10: print("switch not here")
}

// multiple matches
switch 5 {
    2,3,4: print("switch not here"),
    5,6,7: print("switch here"),
    else: print("switch else not here")
}

switch "else" {
    "string": print("switch not here"),
    "number": print("switch not here"),
    else: print("switch else here")
```


### Strings and Inline Code

Code can be executed within a dialogue line with braces `{}`.

```topi
var greeting = "Howdy"
const value = 42
=== START {
    :John: "{greeting}, Jane!" // Howdy, Jane!
    greeting = "Hello"
    :Jane: "{greeting}, John. The password is {value}." // Hello, John. The password is 42.
}
```

Double quotation marks can be added by doubling them inside strings `"""Hello"", he said."` will output `"Hello", he said`

### Functions

Functions are declared `fn [NAME]` and parameters between pipes (`|`)
Braces are optional if only one line is used.

**Functions cannot contain Boughs only logic.**

```topi
fn sum |x, y| return x + y

fn fib |n| {
    if n < 2 return n
    return fib(n - 1) + fib(n - 2)
}
```

#### Return Void

If you want to return out of a function early you have to specify `return void` and not just `return` like most langauges.

```topi
fn early || {
    if true return void
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

Under the hood they are just index integers, which cannot be changed.
This does mean you can use comparative operators with enums.

```topi
var north = Cardinal.North
var south = Cardinal.South

print(north < south) // true
```

#### Sequences

Enum Sequences (`enumseq`) are special enums, they are the same except they cannot be changed to a previous value.
If attempted, topi will ignore the assignment and remain at the current value.

```topi
enumseq QuestGiver {
	None,
	LearnedOfQuestGiver,
	MetQuestGiver,
	AcceptedQuest,
	CompletedQuest,
	RecievedAward
}

var seq = QuestGiver.None
seq = QuestGiver.MetQuestGiver
seq = QuestGiver.LearnedOfQuestGiver // ignored
print(seq) // QuestGiver.MetQuestGiver
```

Sequences are useful in that all previous states are inferred from the current. 
If the player met the quest giver, they must have learned of them.
Same if they accepted the quest, they must have met them, and so on.
For more information you can watch this great talk by 
[Inkle's Jon Ingold](https://www.youtube.com/watch?v=HZft_U4Fc-U).

### Classes

Classes are an encapsulation of named data. 
All fields must be given a default value.
Instances of classes are created with the `new` keyword.
Any field not initialized will use the default value.


```topi
class Person {
    age = 25,
    name = ""
}

var john = new Person {
    name = "John Doe"
}

print(john) // Person{name = "John Doe", age = 25}
```

Classes can also have functions as fields, 
references to its own fields can be achieved with `self`

```topi
class Person {
    age = 0,
    firstName = "",
    lastName = "",
    
    fn fullName || return self.firstName + " " + self.lastName 
    
    fn increaseAge |amount| {
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

Fields can also be indexed as well with `[]` notation.
This can be useful for function calls from game applications.
As an example with the above class:

```topi
fn changeField |instance, fieldName, newValue| {
    instance[fieldName] = newValue
}

changeField(john, "firstName", "Johnny")
changeField(john, "age", 28)
print(john.fullName()) // "Johnny Doe"
```

## Multiple Files

Multiple files can be joined together to create a single story using `include "[PATH]"`.
All files are added into the global scope, meaning you can't have the same variable names in multiple files.
This is due to variables engine integration, which would not know how to distinguish between 'file1.variable' and 'file2.variable'.

Currently circular dependencies are not allowed.

```topi
// main.topi
include "./other.topi"
sum(1, 5)
=> START

// other.topi
fn sum |x, y| return x + y
=== START {
    :: "Hello" 
}
```

Note circular includes are not allowed.

**Not allowed**

```topi
// main.topi
include "./other.topi"

// other.topi
include "./main.topi"
```

## Code Execution and Jumps

When making nested jumps be aware that preceding code will be executed to ensure any declarations are created.

Consider the following

```topi
# topi run file.topi START.INNER
// Expected output :Speaker: "End test 2"
=== START {
    :Speaker: "Start conversation"
    var test = "test"
    :Speaker: "Continue"
    === INNER {
        test = "test 2"
        :Speaker: "End {test}"
    }
}
```

In this situation we need to create the `test` variable before it's set to `test 2`.
To ensure that happens when you jump to `START.INNER` first Topiary will jump to `START`
execute all code (while skipping dialogues and forks), then when it encounters another jump
or ends, it'll then jump to `INNER`.

*For this reason it's recommended that all code be placed at the top of all files and boughs.*
