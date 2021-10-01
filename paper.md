% TidalCycles - alternate timelines

The TidalCycles (or Tidal for short) live coding environment has been developed since around 2009, going through a number of rewrites of its core representation. Rather than having fixed goals, its development has been guided by use, where its developers have also been users, motivated by the open aim to make music for their audiences, friends, or just for themselves. The software engineering process can therefore be seen as a long-form improvisation, with insights into the nature of Tidal only gained through the process of writing it, and those insights feeding back to guide the next steps of development.

This brings up the notion that key insights will have been missed, that would have lead to very different software. Indeed participants at beginners workshops have often asked questions without good answers, because they made deficiencies or missing features in the software clear. It is well known that a beginners mind is able to see much that an expert has become blind to. Running workshops are an excellent way to find new development ideas, but the present paper explores a different technique - the rewrite.

I have 're-written' Tidal before, at least largely re-writing the inner representation of Tidal patterns and refactoring its library of combinators. This involved working in a fresh source folder, but copy-and-pasting a large part of the code, function-by-function from old to new, re-appraising and rewriting as I went. By focussing on the representation, and taking advantages of insights gained since the last rework, generally this involved deleting more code than I wrote. Certainly the type definitions and supporting code has become significantly shorter and clearer through the process of these rewrites.

This time, during the summer 2021, I wanted to try working completely from scratch, rewriting Tidal without reference to the original code. In free/open source development this is sometimes done for 'clean room' re-implementations when changing the license for a multi-authored work. That isn't the motivation this time, I am keeping the same GPL 3.0 license and crediting all contributors to Tidal as before. The motivation here was just out of curiosity - how different would it turn out? Would it be better or worse? Will this become the next version of Tidal, and if not how will any new insights feed back into mainline Tidal development? A report on this ongoing experiment follows.

## The first two hours

I began the rewrite as a two-hour live stream, to see how much of Tidal I could write in that time.[^ An archive of the live stream is available here: https://youtu.be/F2-evGtBnqQ ] I wasn't sure how interactive or interesting the stream would be, but in the event, I happily talked continuously throughout, with an eye on feedback in the live chat. Surprisingly for me, this felt more engaged than the many times I have streamed musical live coding performances. It felt good to respond to questions and read encouraging messages, while sharing quite an intensive experience of writing the foundations of a system from scratch.

Although this was what you might call "night science", done out of curiousity rather than to respond to a clear research question, this approach has some relation to the "think aloud" and "talk aloud" protocols, useability research techniques which have been applied in the Psychology of Programming field to investigate programming languages as user interfaces. From my experience it seems possible to 'think aloud' while writing a significant part of a representation for live coding, during a relatively short period of time. The design of Tidal's innards has  taken place over a decade, but this session could be viewed as replaying a condensed version of the thinking behind its design over a mere two hours.  This points to a potentially useful research programme, where several people are invited to attempt a similar process of thinking aloud while remaking a core part of their system. There has been little in the way of comparative research into the thinking that goes into live coding language design, and this approach could be a fruitful approach to take. However I only offer this thought for future work, and in-depth analysis of the live stream itself is out of context for this paper.

The outcome of the stream[^ See the following link for the code resulting from the two hour live stream: https://github.com/yaxu/remake/commit/8cee36417438e82778b2e0085a2dd897609b8593] was 114 code lines[^ Calculated with the *cloc* utitlity: https://github.com/AlDanial/cloc], roughly two lines per minute, although 37 of these lines were largely redundant type definitions.  At the time of writing, additional work tidying and expanding on this work has roughly trebled this line count since, and it is this version that I will compare with the mainline Tidal codebase.[^ The state of the repository at the time of writing: https://github.com/yaxu/remake/tree/a088f49683f3034881292f20a90d39abc21bdc5f]

## Comparing the 'remake' with mainline Tidal


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
