# Tips & Tricks

This is a collection of patterns for *writing* in Topi. If you're new to
the language, start with [syntax.md](syntax.md) — that's the reference.
This doc is the recipe book that comes after.

It's aimed at writers more than engine integrators. Each section is a
named pattern with a small snippet you can copy and adapt.

## Organize boughs by conversation, not by step

Boughs share a single global namespace across every file in your project.
That means if `chapter1.topi` defines `=== START` and `chapter2.topi` also
defines `=== START`, the compiler can't tell them apart and you'll get a
collision.

The fix is to wrap each conversation, character, or location in an outer
bough. Sub-boughs get addressed by their full path, so they never collide
across files:

```topi
// Bad — any other file with === START will collide
=== START { ... }
=== ASK_ABOUT_KEY { ... }
=== LEAVE { ... }

// Good — addressable as PROFESSOR.START, PROFESSOR.ASK_ABOUT_KEY, etc.
=== PROFESSOR {
    === START { ... }
    === ASK_ABOUT_KEY { ... }
    === LEAVE { ... }
}

=== RESEARCH_ASSISTANT {
    === START { ... }            // does not collide with PROFESSOR.START
    === ASK_ABOUT_KEY { ... }
}
```

You jump in with `=> PROFESSOR.START` from anywhere. Code at the top of
`PROFESSOR` runs before the sub-bough body — see
[syntax.md §Code Execution and Jumps](syntax.md#code-execution-and-jumps).

## File organization

A pattern that works well, especially if you're not used to programming:

- One file per character, location, or scene. Keeps related dialogue
  together and keeps the per-file CSV small after `topi loc export`.
- An entry file (`story.topi`) that just `include`s the others and
  jumps to the opening bough.
- All `var`, `const`, `enum`, `class`, and `fn` declarations at the
  top of files and at the top of boughs. Topi runs parent code before
  jumping into a child bough, so any state your scene needs has to
  exist by then.

```
game/
  story.topi              // just `include`s and `=> OPENING`
  professor.topi
  professor.topi.csv
  research_assistant.topi
  research_assistant.topi.csv
  shared/
    intro.topi
    intro.topi.csv
```

Variables share the same global namespace boughs do, so prefix per-character
state (`professor_trust`, `professor_questioned`) or hang it off a class
instance (`professor.trust`).

## Hub-and-spoke

A "hub" bough offers a fork of choices that each duck into a sub-bough and
return. `fork^` (fork-and-back) and `=>^` (jump-and-back) are doing the
work here — they let the hub stay the source of truth for navigation
without duplicating wrapper text in every spoke.

```topi
=== KITCHEN {
    fork^ {
        ~ "Talk to Nora"  => KITCHEN.NORA
        ~ "Talk to Felix" => KITCHEN.FELIX
        ~ "Leave"         => HALLWAY
    }

    === NORA  { :Nora:  "What now?" }
    === FELIX { :Felix: "Yes?"      }
}
```

Mark a topic with `~*` instead of `~` to make it disappear after it's
chosen — useful when a choice represents asking a one-time question.

## Looping topic forks (the interrogation pattern)

When a character has a list of topics to ask about, give the `fork` a name
and jump back to it from each prong. Combined with `~*` for one-shot
topics, this is the workhorse pattern for any "ask about X / ask about Y"
UI.

```topi
=== INTERROGATE {
    fork TOPICS {
        ~* "Ask about the alibi" {
            :Suspect: "I was in the conservatory."
            => TOPICS
        }
        ~* "Ask about the poison" {
            :Suspect: "I use it for the slugs."
            => TOPICS
        }
        ~ "Leave" #leave => HALL
    }
}
```

The `~ "Leave"` prong has no `~*`, so it stays available every loop. See
[syntax.md §Forks](syntax.md#forks) for the full syntax.

## Visit counts (and a gotcha)

Every bough tracks how many times it's been visited. You can read that
count by name from anywhere:

```topi
if LIBRARY > 5 :: "You've spent a lot of time in the library."
```

**Important:** a bough's own visit count increments *on entry*, before any
code in its body runs. So inside `=== GARDEN`, the value of `GARDEN` is
already `1` on the first visit — never `0`. Use `== 1` for first-time
text, not `== 0`:

```topi
=== GARDEN {
    if GARDEN == 1 :: "First time you've been here."
    else if GARDEN < 4 :: "You've been here before."
    else :: "You know this garden well."

    // Other boughs are still 0 if never visited — that check works fine
    if LIBRARY == 0 :: "You wonder what's in the library."
}
```

Visit counts are tracked by full path, so `KITCHEN.NORA` and
`LIBRARY.NORA` count separately.

## Text variation (random, cycling, sequential)

For ambient narration that varies each time the player passes through —
wind sounds, waves, idle background lines — use a counter variable plus a
`switch` so each variant is its own dialogue line. After stamping, every
variant gets its own `@ID` and translators see them as separate strings.

Cycle through in order:

```topi
var wind_idx = 0
=== CLIFFSIDE {
    switch wind_idx {
        0: :: "The wind howls through the rocks.",
        1: :: "A gust rattles the shutters.",
        2: :: "The wind shrieks like a wounded gull."
    }
    wind_idx = (wind_idx + 1) % 3
}
```

Pick at random:

```topi
switch rnd(0, 3) {
    0: :: "Waves crash against the jetty.",
    1: :: "The sea churns below.",
    2: :: "Spray mists across the rocks."
}
```

Play in order, then stick on the last entry forever:

```topi
var journal_idx = 0
switch journal_idx {
    0: :: "Day 12: The light must not go out.",
    1: :: "Day 19: Someone has been in the radio room.",
    else: :: "Day 31: If you are reading this, I am already gone."
}
if journal_idx < 2 journal_idx += 1
```

The `random()`, `cycle()`, `sequence()`, and `shuffle()` builtins exist
too, but they pick from a `List` of *values*, not from separate dialogue
lines. They're great for non-dialogue (picking a number, an enum, an
object) but won't get you per-line `@ID`s. For player-visible text, prefer
the `switch` form. See [localization.md](localization.md) for the full
reasoning.

## Knowledge propagation (who knows what)

For stories where information spreads between characters as the player
shares it, give each NPC a `knows` set and gate dialogue on it. Choices
appear when the player knows something the NPC doesn't.

```topi
class NPC {
    name = "Unknown",
    trust = 50,
    knows = Set{},
}

var known = Set{}                          // what the player has learned
var nora  = new NPC { name = "Nora"  }
var felix = new NPC { name = "Felix" }

=== NORA {
    fork TOPICS {
        if known.has("the_letter") and !nora.knows.has("the_letter") {
            ~ "Tell Nora about the letter" {
                :Nora: "Let me see it."
                nora.knows.add("the_letter")
            }
        }
        ~ "Step away" => ROOM
    }
}
```

This scales: every new piece of evidence is one `Set` entry plus one
guarded prong, not a new branch in a tree. See `examples/estate.topi` for
a fully built-out version with three NPCs and ~10 facts.

## Two prongs are better than one conditional choice

Topi *will* let you interpolate an `{if ...}` directly into a choice's
text:

```topi
~ "{if known.has("name") "Visit the gardener" else "Knock on the door"}" {
    => GARDENER
}
```

This works at runtime, but the stamper only puts one `@ID` on the outer
choice. Both text variants share that ID, so translators only see one of
them. For anything player-visible, write two prongs guarded by `if`/`else`
instead — each one becomes its own stamped line:

```topi
fork {
    if known.has("name") {
        ~ "Visit the gardener" => GARDENER
    } else {
        ~ "Knock on the door"  => GARDENER
    }
}
```

The interpolated form is fine for non-localized text (debug prompts, dev
tooling). For localized projects, default to two prongs.

## `=>^` as a "subroutine" for prose

`=>^` (jump-and-back) lets you factor a chunk of repeated dialogue into
its own bough and call it from anywhere. Treat it like a function for
prose: `EXAMINE_INVENTORY`, `WEATHER_INTRO`, `RECAP_THE_CASE`.

```topi
=== EXAMINE_BODY {
    :: "The victim is face down in the petunias."
    :Coroner: "Pruned to death."
}

=== CRIME_SCENE {
    =>^ EXAMINE_BODY
    :Detective: "Just as I thought."
}

=== RETURN_TO_SCENE {
    =>^ EXAMINE_BODY
    :Detective: "Same as last time, then."
}
```

Pairs well with the hub pattern when several spokes need the same setup
or recap.

## Scoring and threshold endings

For "the ending depends on what the player accomplished" stories,
accumulate evidence (or trust, or knowledge) into a `Map` and `switch` on
the total at the climax.

```topi
var evidence = Map{}

fn totalEvidence || {
    var total = 0
    for evidence |e| total += e.value
    return total
}

// During play, stash weighted clues
evidence.add("cut_cable", 3)
evidence.add("hidden_message", 3)

=== CONFRONTATION {
    switch totalEvidence() {
        0..5: => ENDING_INCONCLUSIVE,
        6..12: => ENDING_PARTIAL,
        else: => ENDING_FULL_TRUTH
    }
}
```

`Map` keeps each clue addressable by name, and weighting them lets a few
big finds outweigh many small ones. See `examples/story.topi` for a real
build-out.

## Ending the story with `fin`

`fin` ends the story immediately, from anywhere — including from inside a
`=>^` jump-back. That makes it good for shared "you died" or "game over"
boughs that several scenes can call into:

```topi
=== ATTACKED {
    health -= 1
    if health <= 0 fin
}

=== ALLEY  { =>^ ATTACKED  :: "You stagger on." }
=== ROOFTOP { =>^ ATTACKED :: "You catch your breath." }
```

If `health` drops to zero, the story ends at the `fin` regardless of
which scene was running.
