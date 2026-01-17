# Syntax

## Dialogue 

### Boughs

Sections of dialogue are called Boughs, and are denoted with `=== [Name] {}`. 
So `=== START {}` is a bough called `START` with an empty "body".

### Lines

Lines have three sections. `:?[Speaker]: "[Content]" ?[#Tag]`

Speakers are optional and can be empty with `::`.
Tags are also optional and can be omitted or multiple can be separated each with their own hash (`#`).

```topi
:: "The scent of crushed lavender hangs heavy in the air."
:Detective: "A curious place for a murder, wouldn't you say?" #stern #suspicious
```


### Jumps

Boughs can be started with jumps, denoted with `=> [Name]`. The first jump must be passed in to the `start` function,
or if using the CLI the second parameter of the run command.

```topi
# topi run file.topi --bough GATEHOUSE

=== GATEHOUSE {
    :Gardener: "I was just pruning the hydrangeas, I swear!" #nervous
    :Detective: "With a blood-stained trowel, Mr. Higgins?"
}
```

### Nesting

Boughs can be nested and jumped to with `.` like so:

```topi
# topi run file.topi ESTATE.MAZE

=== ESTATE {
    === MAZE {
        :Detective: "Lost in the hedges... and so was the victim."
    }
}
```


### Forks

To create interesting and interactive stories 
we need to be able to divert the story depending on choices answered by the player. 
For that we use the keyword `fork` with each choice prong denoted with a tilde `~ ?[NAME] "[CONTENT]" ?[#TAG]`.

```topi
=== FOUNTAIN {
    :Detective: "The water is clouded. Should I reach in?"
    fork {
        ~ "Search the basin" {
            :: "Your fingers brush against a cold, silver key."
            inventory.add("Silver Key")
        }
        ~ "Examine the statue" #leave => STATUE_CLUE
    }
}
```

Forks can be named for looping interactions.

```topi
=== INTERROGATION {
    fork TOPICS { 
        ~ "Ask about the Alibi" {
            :Lady_Margery: "I was in the conservatory all evening!"
            => TOPICS
        }
        ~ "Ask about the Poison" {
            :Lady_Margery: "I use that for the slugs, nothing more."
            => TOPICS
        }
        ~ "Leave her be" #leave => GARDEN_PATH
    }
}
```

Choices can be unique with `~*`, disappearing once selected.

```topi
=== EVIDENCE_BAG {
    fork GATHER { 
        ~* "Take the Torn Letter" {
            :: "It's addressed to the Butler..."
            => GATHER
        }
        ~ "Finish searching" => GAZEBO
    }
}
```

### Visits

Use paths to check how many times a player has visited a location or made a choice.

```topi
=== ROSE_GARDEN {
    if ROSE_GARDEN > 5 {
        :Detective: "I've paced this path so many times."
    }
    :: "The roses are suspiciously red."
    === YELLOW_ROSE {
        :Detective: "But the yellow ones are the most fragrant."
    }
    => YELLOW_ROSE
}

=== GARDEN_PATH {
    :Detective: "What am I missing?"
    if ROSE_GARDEN.YELLOW_ROSE > 1 {
        :Detective: "I've already checked the yellow roses."
    }
    ...
}
```

### Flow, Fin, and Jump Back Ups

Sometimes we want to temporarily duck into a bough and then continue on.
We can do that by adding a caret `^` to a jump `=>^ [NAME]` (meaning jump then come back up).

```topi
=== CRIME_SCENE { 
        :Detective: "Let's see what the coroner has to say." 
        =>^ EXAMINE_BODY
        :Detective: "Just as I thought. Pruned to death."
    }
    
    === EXAMINE_BODY {
        :: "The victim is face down in the petunias."
        :Coroner: "Cause of death; stabbing by a pair of gardening shears."
    }
}
```

Backups can also be applied to forks with `fork^ ?[NAME]`.

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

A story will end when it hits a `fin` regardless of where it is 
or if it's within a jump back up.

```topi
=== GARAGE {
    :Driver: "I will not let you ruin everything I've worked so hard for, Detective."
    :Detective: "What do you mean?"
    :: "The Driver hits you in the back of the head with a wrench."
    =>^ ATTACKED
    :: "The Driver runs off as you wearily pick yourself up." 
}

=== ATTACKED {
    health -= 10
    if health < 0 fin // Will end the story here
}
```


## Code

### Variables

Variables can either be mutable `var` or constant `const`. Constant variables cannot be reassigned.

```topi
var cluesFound = 0
const victimName = "Lord Ashbury"

victimName = "Lord Ashbury the Great" // Error:
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
var suspectList = List{"The Butler", "The Maid", "The Cook"}
var emptyList = List{}

suspectList.add("The Driver") 
suspectList.remove("The Cook")
suspectList.count() // 3
suspectList.has("The Mechanic") // false
suspectList.clear()
```

Sets are a collection of unique values.

```topi
var clues = Set{"Bloody Shears", "Torn Letter", "Hidden Handkerchief"} // Unique items only
const emptySet = Set{}

clues.add("Torn Letter") // no change 
clues.count() // 3
clues.remove("Hidden Handkerchief")
clues.has("Hidden Handkerchief") // false
clues.clear()
```

Maps are a collection of key/value pairs.

```topi
const evidenceWeights = Map{"Shears": 10, "Letter": 5}
const emptyMap = Map{}

evidenceWeights.add("Handkerchief", 5) // (add will replace values if already exists)
evidenceWeights.count() // 3
evidenceWeights.remove("Letter")
evidenceWeights.has("Shears") // true
evidenceWeights.clear()
```

### Loops

#### While

`while [CONDITION] { }`
While loops will execute so long as the condition is met.

```topi
var intensity = 3
while intensity > 0 {
    :: "The thunder rolls..."
    intensity -= 1
}
```

#### For

`for [ITERATOR] | [ITEM] | { }`
For loops can be a range (inclusive), or a collection.

```topi

print("Clues:")
for 0..2 |i| {
    print("{clues[i]}") 
}

```

```topi
for suspectList |person| {
    print("Interrogating {person}...")
}
```

```topi
for evidenceWeights |evidence| {
    print(evidence.key) // "Shears", "Letter", "Handkerchief"
    print(evidence.value) // 10, 5, 5 
}
```

### Control flow 

Code and dialogues can be conditional with `if/else`.
Braces are optional if only one line is used, though readability is improved for some cases.

```topi
var suspicion = 0 
if gardener_has_trowel { suspicion = 10 } 
else if found_blood_stain { suspicion = 50 } 
else suspicion = 0

if suspicion > 40 :: "The gardener begins to sweat profusely."
```

Switch statements can also be used.
 
Breaks are implied on the first case which matches. 
Multiple conditions can be matched with commas (`,`), 
and if using numbers, Ranges (`n..m`) can also be used.

`else` statements are optional, if no prongs match, it will be skipped.

```topi
switch found_item { 
    "Trowel": :Detective: "This trowel matches the wound perfectly!", 
    "Letter": :Detective: "A confession, hidden in the roses?", 
    else: :Detective: "Nothing but dirt and petals here." 
}

switch suspicion { 
    0..10: print("Hardly a suspect."), 
    11..50: print("Keep an eye on them."), 
    51..90: print("Focus our efforts on them.") 
    91..100: print("Arrest them at once!") 
}

switch lady_margery_location { 
    "Kitchen", "Cellar", "Stable": :: "She has no alibi for the time of the murder.", 
    "Conservatory", "Parlor": :: "The staff confirm she was taking tea.", 
    else: :: "Her whereabouts remain a mystery." }
```

### Strings and Inline Code

Expressions can be evaluated 
within a dialogue or choice line with braces `{}`
as an interpolated string.

```topi
var weapon = "Poisoned Tea"
var location = "Library"
=== CONFRONTATION {
    :Detective: "It was you, with the {weapon + " in the " + location}!"
}
```

Double quotation marks can be added by 'escaping' them with backslash `/` `"/"Hello/", he said."` will output `"Hello", he said`

### Functions

`fn [NAME] | ?[PARAMETER,] | { }`
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

If you want to return out of a function early, you have to specify `return void` 
and not just `return` like most langauges.

```topi
fn early || {
    if true return void
}
```

### Enums

`enum [NAME] { [VALUE,] }` 

Used for static state values, 
while `enumseq` for state values that only move forward.

Sequences are useful in that all previous states are inferred from the current.
If the player is investigating, they must have discovered the arrived on the scene.
Same if they accused, they must have investigated, and so on.
For more information you can watch this great talk by [Inkle's Jon Ingold](https://www.youtube.com/watch?v=HZft_U4Fc-U).

```topi
enum Location { Library, Garden, Attic }

enumseq CaseStatus {
    Arrived,
    Investigation,
    Accusation,
    Solved
}

var location = Location.Library
location = Location.Garden
location = Location.Attic

var current_state = CaseStatus.Discovery
current_state = CaseStatus.Investigation
current_state = CaseStatus.Discovery // This assignment is ignored!
```

Under the hood both they are just index integers, which cannot be changed.
This does mean you can use comparative operators with enums.

```topi
print(Location.Library < Location.Attic) // true
print(Location.Library == 0) // true
```

### Classes

`class [NAME] { [FIELD,] [METHOD] }`
Classes are an encapsulation of named data. 
All fields must be given a default value.
Instances of classes are created with the `new` keyword.
Any field not initialized will use the default value.

Classes can also have function as methods,
references to its own fields can be achieved with `self`

```topi
class Suspect {
    name = "Unknown",
    suspicion_level = 0,

    fn implicate |amount| {
        self.suspicion_level += amount
    }
}

var butler = new Suspect { name = "Barnaby" }
butler.implicate(50)
print("I am {butler.suspicion_level}% sure {butler.name} is our culprit.")
```

Fields can also be indexed as well with `[]` notation.
This can be useful for function calls from game applications.
As an example with the above class:

```topi
fn changeField |instance, fieldName, newValue| {
    instance[fieldName] = newValue
}

changeField(butler, "name", "Barnaby Farnsworth")
changeField(butler, "suspicion_level", 0)
print("Knowing {butler.name} is a Farnsworth excludes him from the investigation.")
```

## Multiple Files

Multiple files can be joined together to create a single story using `include "[PATH]"`.
All files are added into the global scope, meaning you can't have the same variable names in multiple files.
This is due to variables engine integration, which would not know how to distinguish between 'file1.variable' and 'file2.variable'.

Currently, circular dependencies are not allowed.

```topi
// main.topi
include "./utils.topi"
sum(1, 5)

// utils.topi
fn sum |x, y| return x + y
```

Note circular includes are not allowed.

**Not allowed**

```topi
// main.topi
include "./garden.topi"

// other.topi
include "./dining_room.topi"
```

## Code Execution and Jumps

When jumping to nested boughs, Topiary ensures all parent variables are initialized first.

Consider the following

```topi
# topi run file.topi TEA_PARTY.SERVE

=== TEA_PARTY {
    var poison_present = true
    === SERVE {
        if poison_present :Detective: "I wouldn't drink that if I were you."
    }
}
```

In this situation we need to create the `poison_present` variable before it can be used in the `if` statement.
To ensure that happens when you jump to `TEA_PARTY.SERVE` first Topiary will jump to `TEA_PARTY`
execute all code (while skipping dialogues and forks), then when it encounters another jump
or ends, it'll then jump to `SERVE`.

*For this reason it's recommended that all code be placed at the top of all files and boughs.*
