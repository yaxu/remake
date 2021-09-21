% TidalCycles - alternate timelines

The TidalCycles (or Tidal for short) live coding environment has been developed since around 2009, going through a number of rewrites of its core representation. Rather than having fixed goals, it's development has been guided by use, where it's developers have also been users, motivated by the open aim to make music for their audiences, friends, or just forthemselves. The software engineering process can therefore be seen as a long-form improvisation, with insights into the nature of Tidal only gained through the process of writing it, and those insights feeding back to guide the next steps of development.

This brings up the notion that key insights will have been missed, that would have lead to very different software. Indeed participants at beginners workshops have often asked questions without good answers, because they made deficiencies or missing features in the software clear. It is well known that a beginners mind is able to see much that an expert has become blind to. Running workshops are an excellent way to find new development ideas, but the present paper explores a different technique - the rewrite.

I (Alex) have 're-written' Tidal before, at least largely re-writing the inner representation of Tidal patterns and refactoring its library of combinators. This involved working in a fresh source folder, but copy-and-pasting a large part of the code from old to new, re-appraising as I went. By focussing on the representation, and taking advantages of insights gained since the last rework, generally this involved deleting more code than I wrote. Certainly the type definitions and supporting code has become significantly shorter and clearer through the process of these rewrites.

This time, during the summer 2021, I wanted to try working completely from scratch, rewriting Tidal without reference to the original code. This is sometimes done for 'clean room' re-implementations when changing the license for a multi-authored work. That isn't the motivation this time, I'm keeping the same GPL 3.0 license and crediting all contributors to Tidal as before. The motivation here was just out of curiosity - how different would it turn out? Would it be better or worse? A report on this ongoing experiment follows.

## Live stream

I began the rewrite as a two-hour live stream, to see how much of Tidal I could write in that time.[^ An archive of the stream is available here: https://youtu.be/F2-evGtBnqQ ] 

* Differences of terminology in secondary notation
* Differences of implementation
  * Pure
  * Applicative

## Continued work

Emerging motivations
* Straightforward refactoring - text type, clearer definitions
* Formalising mininotation
* Weaving mininotation into functional transformation
* Escaping Haskell syntax

## What if?

* 2d area instead of 1d span
