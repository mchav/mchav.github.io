---
layout: post
title: An introduction to program synthesis
---

## Introduction
This post kicks off a hands-on series about program synthesis—the art of generating small programs from examples. We’ll build a tiny, [FlashFill-style](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/12/popl11-synthesis.pdf) synthesiser that learns to turn strings like “Joshua Nkomo” into “J. Nkomo” from input/output pairs. We'll see how to define a tiny string-manipulation language, write an interpreter, and search the space of programs to find one that solves our toy problem.

The accompanying code for this post is [on Github](https://github.com/mchav/synthesis) and runs in under a second for most examples.

## Why synthesis matters
### System 1 vs System 2 thinking

The book "Thinking Fast and Slow" by Daniel Kahneman differentiates between two kinds of thinking: system 1 vs system 2 thinking.

System 1 thinking is characterised by fast, intuitive judgments. It's the thinking that you do when you're not fully aware that you're thinking. It is built up through repetition and experience. How we express language is an example of System 1 at work. In most situations, we don't consciously think of what words to say. They stream out of us, dressing the thoughts we intend to communicate. System 1 thinking is the realm of the familiar. It's not always right, but it gets you into a comfortable ballpark.

System 2 thinking, on the other hand, is slow and deliberate. It's speaking in front of a crowd for the first time. It's taking an exam for a class. It's how we think when a context is outside (or at the bounds) of what we are used to. System 2 solves hard problems, but is time-consuming and taxing.

Human cognition is a constant dance between the two. Talking to a loved one is easy until the conversation topic is something sensitive. Writing a proof is hard until you discover the "trick" that could lead you to an answer. The two systems also interact in very strange ways. Ever had a eureka moment in the shower? Ever gotten a joke while driving, days after it was told? Problem-solving often means alternating between these two systems of thought.

### How machines "think"
An analogous dichotomy can be applied to our approaches to artificial intelligence.

Machine learning (broadly defined as using statistics to learn patterns from data) is system 1 thinking. It is the statistical "intuition" gained from gorging on mounds of data. It's easy to invalidate a lot of statistical approaches as memorisation (a lot of people will say LLMs are just a fancy autocomplete), but building the right statistical models is as difficult as building the right intuitions to solve problems. There has been a lot of ingenuity in this space over the last few decades.

But it's not the entire story.

Large language models hallucinate because the sort of thinking they do is prone to being wrong when things are difficult. This is a feature, not a bug.

Until the advent of deep learning in the early 2010s, artificial intelligence was dominated by systems that tried to recreate system 2 thinking. These are "slow" algorithms that take time to discover the "right" answer. Program synthesis is one such approach. It tries to search through the space of instructions until it discovers one that solves a specified problem.

Synthesis approaches are hard to scale or expand. Again, this is a feature, not a bug.

A growing number of researchers believe that getting to artificial general intelligence involves marrying the two approaches (what's now referred to as neuro-symbolic AI). There have been some promising achievements in the field recently:

* DeepMind's Alpha geometry uses a combination of machine learning and symbolic methods to solve complex geometry problems ([Aleph0 has a really good video on this](https://www.youtube.com/watch?v=4NlrfOl0l8U)).
* Open AI's o3 model, according to Francois Chollet, is [a form of deep learning guided program search](https://arcprize.org/blog/oai-o3-pub-breakthrough).

So, as we stand on the cusp of a future where synthesis looks as though it will make a resurgence in some incarnation, I thought it apt to write a series of practical blog posts on what it is.

## What is program synthesis

We'll work from a definition offered in Armando Solar-Lezama's [MIT class on the subject](https://people.csail.mit.edu/asolar/SynthesisCourse/index.htm).

_Program Synthesis correspond to a class of techniques that are able to generate a program from a collection of artifacts that establish semantic and syntactic requirements for the generated code._

Program synthesis asks: given a specification of what I want, can a computer search the space of small programs and discover one that fits my needs?

### Searching and Specification

#### Specifying the problem
Program synthesis lives at the intersection of what you want (the specification) and how you look for it (the search).

A specification is any collection of clues that narrows down the set of acceptable programs. Those clues can be semantic (what the output must mean) or syntactic (what the code must look like).

Synthesis problems are usually specified through:
* Input–Output Examples (I/O): The friendliest form. "When input is Joshua Nkomo, output must be J. Nkomo."
* Constraints / Properties (pre/postconditions):
Rules like "output must contain a full-stop," "output length ≤ max(len(input), len(target))," or "index must be within bounds." These semantic checks prune huge regions of bad candidates even before running full tests.
* Types and Grammars (the DSL itself):
By choosing a small, typed vocabulary (our string blocks), you ban ill-formed programs outright. This is a syntactic specification baked into the language design.
* Sketches (templates with holes): "I’m pretty sure it’s Concat(Upper(Head(x)), ???)—fill in the ???." You constrain structure and let the tool finish the details.
* Negative examples: "For input Steve, don’t output S. with a trailing space." Prevents sneaky overfits.
* Resource hints / preferences (ranking): “Prefer shorter programs,” “penalize Substring with two dynamic indices,” etc. These are soft constraints used to rank ties.

In our mini Flash-Fill program, the specification will be I/O examples.

#### Searching the space
At is core, synthesis is a search problem. Several families of search exist;

1. Enumerative search
2. Constraint-guided search
3. Heuristic / ML-guided search (nice upgrade later)

We'll focus on the latter two is subsequent posts. For this post we'll a use simple, yet powerful, type of enumerative search. 


## Building Our Synthesiser

_Follow along as you read the code on [Github](https://github.com/mchav/synthesis/blob/main/src/Enumerative.hs)_

### Starting from the bottom

Here's the key insight: if we have a language of simple operations (head, tail, concat, etc.), we can systematically combine them to build complex programs. It's like having LEGO blocks - start with basic pieces and build increasingly complex structures until one solves the problem.

Our algorithm:
1. Start with the simplest possible program (just return the input unchanged)
2. Generate all programs one step more complex
3. Test each against the examples
4. If none work, generate programs two steps more complex
5. Repeat until success or timeout

This is called "bottom-up enumerative search" - we enumerate programs from simplest to most complex.

We'll write a synthesiser that generates programs that can solve problems of the following nature:

|       Input   | Output      | 
| ------------- | ----------- |
|  Joshua Nkomo | J. Nkomo    |
|    Steve Biko | S. Biko     |
| Jomo Kenyatta | J. Kenyatta |


### The building blocks

We usually start by defining our building blocks when we do synthesis. This is usually called a domain-specific language (DSL). First, we define a small "language" of basic operations that programs can use:

* Head — keep the first character (“Joshua” → “J”)
* Tail — drop the first character (“ Nkomo” → “Nkomo” after trimming a leading space)
* Lower / Upper — change case
* Concat — glue two strings together
* Substring start end x — slice a range from x
* Find needle haystack — index of needle inside haystack
* Constants like ".", " "
* A placeholder input value

That’s enough to express lots of “data cleaning” tricks.

Given our mini-language, we can express our intended program as follows:

```haskell
firstInitial = Upper(Head(x))

-- Find the occurrence of the space, then drop it.
lastName = Tail( Substring( Find(" ", x), End, x ) )

Concat (
 Concat ( firstInitial, ". "),
 lastName
)

-- Or written densely.
Concat(
 Concat( Upper(Head(x)), ". " ),
 Tail( Substring( Find(" ", x), End, x ) )
)
```

Our challenge is to see if we can come up with a similar program using synthesis.

First, we have to define our language programmatically:

```haskell
data Program a where
 Concat :: Program String -> Program String -> Program String
 Substring :: Program Int -> Program Int -> Program String -> Program String
 Tail :: Program String -> Program String
 Head :: Program String -> Program String
 SValue :: String -> Program String
 Variable :: Program String
 Find :: Program String -> Program String -> Program Int
 Start :: Program Int
 End :: Program Int
```
`Program a` means “a little recipe (an expression tree) that, when we run it, produces a value of type a.” In our code, there are only two kinds of results: `Program String` and `Program Int`.

The `:: Program String` or `:: Program Int` parts tell us what kind of result each operation produces:

* `Program String` = produces text
* `Program Int` = produces a number (usually a position)

The arrows (->) show what inputs each operation needs. For example:

* `Tail :: Program String -> Program String` means "Tail needs a string-producing program as input and produces a string-producing program as output"

This structure is compositional. We can define longer programs in terms of these basic elements as we did in the solution above. The synthesis community typically uses functional programming languages because they express these relationships more clearly.

The same structure in Python would look something like this:

```python
class Program:
    """Base class for all program nodes (string- or int-producing)."""
    pass

@dataclass(frozen=True)
class Concat(Program):
 left: Program   # expects a string-producing Program
 right: Program  # expects a string-producing Program

@dataclass(frozen=True)
class Substring(Program):
 start: Program  # int-producing Program (index or special End)
 end: Program    # int-producing Program (index or special End)
 s: Program      # string-producing Program

@dataclass(frozen=True)
class Tail(Program):
 s: Program  # string-producing Program

@dataclass(frozen=True)
class Head(Program):
 s: Program  # string-producing Program

@dataclass(frozen=True)
class SValue(Program):
 value: str  # constant string

@dataclass(frozen=True)
class Variable(Program):
    """The input string."""
    pass
```

The compositionality is lost in the verbosity so we stick to Haskell for demonstration.

It is easy to view DSLs as a limitation. Why define a small language instead of generating any arbitrary code in a language like Python or Java? Doesn't a DSL mean that our technique inherently lacks power and expressivity?

While a DSL can be viewed as a constraint, its contribution is far more profound than a simple restriction. The design of the DSL is deeply intertwined with the design of the search algorithm. A well-designed DSL exposes a set of operators and a program structure that are amenable to efficient search and, crucially, aggressive pruning of the search space. DSLs are how we embed domain knowledge into program synthesis.

The development of FlashFill provides a canonical example of this principle. The synthesiser's DSL was not chosen arbitrarily; it was carefully engineered to enable an efficient top-down, divide-and-conquer search algorithm.

If a DSL is too small, we risk not being able to express the target transformation. If it's too big, we drown in candidates and overfit to examples. We want a sweet spot: just expressive enough to cover the problem class, yet constrained enough that searches can complete in our lifetime.

The interpreter for our DSL can also be defined recursively:

```haskell
-- | This is the leaf of our tree - string wrapped as a program
-- or rather a program that returns a string constant.
interpret (SValue v) = v
-- | Our string operations
interpret (Tail v) = drop 1 (interpret v)
interpret (Head v) = take 1 (interpret v)
interpret (Concat l r) = interpret l ++ interpret r
interpret (Substring start end g) = ...
```

Think of each `Program` as a tree node: `SValue "foo"` is a leaf; `Tail v`, `Head v` have one child; `Concat l r` has two children; `Substring start end g` has three. `interpret` walks that tree. Each line says, "if the node looks like this shape, do this." Haskell lets us write one equation per shape of input; the one that matches is used.

Now we can define our also search strategy programtically. To recap:

1. **Start small**. Begin with the identity function and the input x. Optionally seed a few helpful constants (like " " or ".") and any characters shared across all inputs/outputs.
2. **Grow candidates**. Repeatedly build bigger programs by:
    * Wrapping existing ones with Head, Tail, etc.
    * Concatenating any two existing ones.
    * Making substrings using Find, Start, and End.
3. **Interpret and test**. Run each candidate on the provided examples; keep only those that don’t explode in size and (eventually) those that match all examples.
4. **Iterate by depth**. Increase the “size limit” and repeat until we succeed or give up.

```haskell
search ::
 -- | Examples
 [(String, String)] ->
 -- | Variables of interest.
 [Program String] ->
 -- | Existing Programs
 [Program String -> Program String] ->
 -- | Search depth
    Int ->
 -- | Maybe is an optional type meaning we might end with no program found.
    Maybe (Program String -> Program String)
search examples variables programs d =
    if d == 0 then Nothing  --| Give up if we've gone deep enough
    else
        case findFirst ps of
 Just p -> Just p
 Nothing -> search examples variables (generatePrograms variables programs) (d - 1)
  where
 findFirst [] = Nothing
 findFirst (p : ps')
 | satisfiesExamples examples p = Just p
 | otherwise = findFirst ps'
```

_The `where` section defines some variable bindings, and he core of the logic is in the `if` statement._

The entire system is recursive:
- Generation 0 produces Generation 1
- Generation 1 produces Generation 2
- Each generation builds on all previous work

Notice, specifically, that the input to the new search is all the generated programs from the current generation, and the other inputs (except for the depth parameter) are left unchanged.

`search examples variables (generatePrograms variables programs) (d - 1)`

### Generate Programs Systematically

The `generatePrograms` function in our code employs four distinct strategies simultaneously, each exploring a different dimension of the program space:

#### Strategy 1: Simple Transformations
```
[ comp transform p | p <- existingPrograms, transform <- [Tail, Head, Lower, Upper] ]
```

This strategy takes every existing program and applies simple one-step transformations. If we have a program `p` that produces "HELLO", this generates:
- `Tail(p)` → "ELLO" (remove first character)
- `Head(p)` → "H" (keep only first character) 

#### Strategy 2: Concatenation Combinations
```haskell
[ \v -> Concat (p v) (q v) | p <- existingPrograms, q <- existingPrograms ]
```

This creates programs that combine the outputs of two existing programs. If we have:
- Program `p`: extracts first word
- Program `q`: extracts last word

This generates `Concat(p, q)`: first word + last word

**Example Evolution**:
- Generation 1: `p = identity` (returns input as-is)
- Generation 2: `Concat(p, p)` (doubles the input: "hi" → "hihi")
- Generation 3: `Concat(Concat(p, p), p)` (triples: "hi" → "hihihi")

**Combinatorial Explosion**: With just 10 existing programs, this strategy alone generates 10 × 10 = 100 new programs! This is something we must contain to make this approach useful.

#### Strategy 3 & 4: Variable-Based Transformations

These are the most sophisticated strategies. They use discovered "variables" (important strings found in the examples) to create complex transformations.

```
[ comp t p | v <- vars, p <- existingPrograms, t <- transformsWithVar v ]
[ comp p t | v <- vars, p <- existingPrograms, t <- transformsWithVar v ]
```

Let's say the system discovered "@" as an important variable. It generates programs like:

**Pre-processing with variable** (Strategy 3):
1. `Concat("@", existing_program)` - Prepend @ to result
2. `Substring(0, Find("@", input), existing_program)` - Everything before @
3. `Substring(Find("@", input), End, existing_program)` - Everything after @

**Post-processing with variable** (Strategy 4):
1. `existing_program` then `Concat` with "@"
2. `existing_program` then extract until "@"
3. `existing_program` then extract after "@"

#### A Concrete Generation Example

Let's trace through generating a program for email domain extraction:

##### Initial State (Generation 0)
- Programs: `[identity]` (just returns input)
- Variables: `["@", "."]` (discovered from examples)

##### Generation 1
The system generates programs like:
- `Tail(identity)` → removes first char
- `Head(identity)` → keeps first char
- `Concat(identity, identity)` → doubles the input
- `Concat("@", identity)` → prepends @
- `Concat(identity, "@")` → appends @
- `Substring(Find("@", identity), End, identity)` → **everything after @**

If the solution required more complexity, Generation 2 would create even more programs:

- `Tail(Substring(Find("@", identity), End, identity))`
- `Concat("domain: ", Substring(Find("@", identity), End, identity))`
- `Tail(Substring(Find("@", identity), End, identity))`

The final function looks like this:

```haskell
-- | Generate the next “ring” of candidate programs from:
--   * vars: string-valued building blocks discovered from examples (e.g., " ", ".", "@", etc.)
--   * existingPrograms: functions that turn an input Program String into a Program String
--   The result is a list of new transformers, ordered (smallest AST first) by a simple size metric.
generatePrograms
 :: [Program String]                      -- ^ vars (constants/placeholders from examples)
 -> [Program String -> Program String]    -- ^ existing candidate transformers
 -> [Program String -> Program String]    -- ^ newly generated transformers (sorted by size)
generatePrograms vars existingPrograms =
  -- Sort candidates so we explore simpler/smaller programs first.
  -- We estimate size by applying each transformer to a dummy leaf (SValue "")
  -- and measuring the generated AST with gSize.
  L.sortBy
 (\p q -> gSize (p (SValue "")) `compare` gSize (q (SValue "")))

    -- 1) Unary wrappers: for every existing transformer p,
    --    wrap it once with a simple string op (Tail/Head/Lower/Upper).
    --    `comp transform p` means “apply p, then apply transform”.
 ( [ comp transform p
 | p <- existingPrograms
 , transform <- [Tail, Head, Lower, Upper]
 ]

    -- 2) All ordered concatenations of existing transformers:
    --    build a function that, given an input v, concatenates (p v) and (q v).
    --    This grows breadth by combining previously discovered behaviours.
 ++ [ \v -> Concat (p v) (q v)
 | p <- existingPrograms
 , q <- existingPrograms
 ]

    -- 3) Variable-aware transforms (pre-compose):
    --    for each discovered var v (a Program String), and each existing p,
    --    create transforms t that *mention* v (prepend/append/slice around v),
    --    then do `t . p` via `comp t p`.
 ++ [ comp t p
 | v <- vars
 , p <- existingPrograms
 , t <- transformsWithVar v
 ]

    -- 4) Variable-aware transforms (post-compose):
    --    symmetric to (3), but apply p first and then t (`p . t` via `comp p t`),
    --    so we explore both “before p” and “after p” placements of the same motif.
 ++ [ comp p t
 | v <- vars
 , p <- existingPrograms
 , t <- transformsWithVar v
 ]
 )
  where
 -- | Small library of transforms parameterised by a discovered variable `v`.
 --   Each lambda expects a string-producing Program `v'` (usually the current input),
 --   and returns a new string-producing Program that uses `v` in a useful way.
 transformsWithVar v =
 [ \v' -> Concat v v'                           -- prepend v
 , \v' -> Concat v' v                           -- append v
 , \v' -> Substring Start (Find v v') v'        -- prefix of v' up to first v
 , \v' -> Substring Start (Find v' v) v         -- prefix of v up to first v'
 , \v' -> Substring (Find v v') End v'          -- suffix of v' after first v
 , \v' -> Substring (Find v' v) End v           -- suffix of v after first v'
 ]

-- | Helper intuition (defined elsewhere in our code):
-- comp :: (Program String -> Program String)
--      -> (Program String -> Program String)
--      -> (Program String -> Program String)
-- comp = (.)
--
-- gSize :: Program String -> Int
-- gSize = ... -- counts AST nodes to bias search toward simpler programs
```

Our approach is like evolution - each generation inherits and combines traits from its ancestors, occasionally producing exactly what we need. Given enough time and memory, it will find all relevant programs expressible in the language.

The key caveat here, though, is "enough time and memory." We'd like to find these programs in our lifetime. How do we tame the growth of our generation function?

Our program, as it exists, runs fine for small transformations, e.g. `[("Joshua", "J"), ("Steve", "S")]`, but for our target problem, my computer runs out of memory and kills the program (even after running for close to an hour).

We need to be smarter about how we search the space/

## The Exponential Growth Problem

Exponential growth is the supervillain of the synthesis story. Looking at the code above, we can see why! We generate A LOT of programs at each step.

Let's calculate the growth rate:

**Generation 0**: 1 program (identity)
**Generation 1**: ~30 programs (depending on variables discovered)
**Generation 2**: ~900 programs (30 existing × 30 combinations)
**Generation 3**: ~27,000 programs
**Generation 4**: ~810,000 programs

The sophistication of an enumerative synthesiser is largely measured by its ability to intelligently prune the search space. Pruning strategies exist on a spectrum, from reactive deduplication of redundant programs to proactive elimination of entire branches of the search tree. L

Let's rewrite our search to include some extra pruning and deduplication steps as examples.

```haskell
search examples variables programs d
 | d == 0 = Nothing
 | otherwise =
        case findFirst ps of
 Just p -> Just p
 Nothing -> search examples variables (generatePrograms variables ps) (d - 1)
  where
 inputs = map fst examples  -- | separate the inputs from the outputs.
 ps = prune examples $ deduplicate inputs programs  -- | Now define a new set of deduplicated, pruned programs.
 findFirst [] = Nothing
 findFirst (p : ps')
 | satisfiesExamples examples p = Just p
 | otherwise = findFirst ps'
```

We haven't defined these functions yet. We'll go over each of them, but we must keep in mind how they fit into our overall program.

### Deduplication
Continuing a search for duplicate programs is a waste of time. Pruning a single redundant sub-program results in exponential savings, as the synthesiser is spared from exploring the exponentially many larger programs that could have been constructed using that redundant component.

But what does it mean for two programs to be equal?

We'll apply a sort of duck test to the programs. If, given the same inputs, they produce the same outputs, then we consider them to be the same program. For example, `2 * x` and `x + x` would pass this equality duck test. They produce the same output for every real number. This is called a type of equality, observational equivalence. It is a practical approximation of true semantic equivalence. Instead of performing a costly formal proof to determine if two programs are identical for all possible inputs, the synthesiser uses the given examples as a proxy.

```haskell
deduplicate :: [String] -> [Program String -> Program String] -> [Program String -> Program String]
deduplicate inputs = go []
  where
 go _ [] = []
 go seen (x : xs)
 | any (equivalent inputs x) seen = go seen xs
 | otherwise = x : go (x : seen) xs

-- | Checks if two programs generate the same outputs given all the same inputs.
equivalent :: [String] -> (Program String -> Program String) -> (Program String -> Program String) -> Bool
equivalent inputs p1 p2 = all ((\i -> interpret (p1 i) == interpret (p2 i)) . SValue) inputs
```

_Our deduplication function is written for simplicity, not performance. The repo contains a more efficient deduplication function._

This function pays dividends as the search grows because more programs are more likely to be similar as we search deeper. Especially if we have invertible functions in our DSL. Including both `+` and `-` in a program over integers could lead to programs like `x + 1 - 1 + 1 - 1` (instead of just `x`).

### Pruning

A lot of ingenuity and domain knowledge go into pruning enumerative searches. Chess engines like Stockfish (a search-based chess engine) use pruning to zero in on the board positions that are more likely to win. In our case, we can employ a simple pruning mechanism:  if the output of the program is bigger than both the input and output, we might be straying away from a solution.

We must be careful how we prune since we could discard a part of the tree that would have been useful to explore. If a prune rule could remove a real solution, make it a ranking/bias (lower priority), not a hard filter. A lot of papers that suggest pruning search spaces in some way will provide elaborate proofs of correctness about their pruning strategy.

We can safely prune programs whose output is bigger than the input or output. For string related problems long output usually means we are straying away from the solution.

```haskell
-- | For this problem we prune based on length.
prune :: [(String, String)] -> [Program String -> Program String] -> [Program String -> Program String]
prune examples ps = pruned
    where
        pruned = filter (\p -> all (\(i, o) -> length (interpret (p (SValue i))) <= max (length i) (length o)) examples) ps
```

### Speed up
After both deduplication and pruning, the program takes under a second to synthesise a function that solves our problem.

```haskell
main :: IO ()
main = do
    let examples = [ ("Jomo Kenyatta", "J. Kenyatta"), ("Steve Biko", "S. Biko"), ("Joshua Nkomo", "J. Nkomo")]
    let variables = [SValue "."]
 print (search examples [SValue "."] 5)
```

```
$:~/code/synthesis$ time cabal run -O2
Right (\Variable -> Concat (Head (Head Variable)) (Concat (SValue ".") (Substring (Find (SValue " ") Variable) End Variable)))

real    0m0.539s
user    0m0.460s
sys 0m0.073s
```

Now running this function on some input it hasn't seen:

```
$ cabal repl
ghci> myFunction v = Concat (Head (Head (SValue v))) (Concat (SValue ".") (Substring (Find (SValue " ") (SValue v)) End (SValue v)))
ghci> interpret (f "Patrice Lumumba")
"P. Lumumba"
ghci> interpret (f "Jason Moyo")
"J. Moyo"
```

This generation strategy represents a balance between mathematical completeness and practical efficiency - systematically exploring an infinite space while remaining computationally tractable for real-world problems.

### Let's Trace Through the Example

Let's see how the synthesiser solves our name formatting problem:

**Input examples:**
```
("Joshua Nkomo", "J. Nkomo")
("Steve Biko", "S. Biko")
```

**Generation 1:** Start with `[Variable]`
- Test: `Variable` produces "Joshua Nkomo" ✗

**Generation 2:** Generate from `[Variable]`
```
Programs generated: [Head Variable, Tail Variable, Upper Variable, ...]
Testing: Head Variable -> "J" ✗
Testing: Tail Variable -> "oshua Nkomo" ✗
...
None match
```

**Generation 3:** Generate from Generation 2 programs
```
Programs generated include:
- Concat (Head Variable) (SValue ".")
 -> "J." (closer but not complete) ✗
- Upper (Head Variable)
 -> "J" ✗
- Substring (Find (SValue " ") Variable) End Variable
 -> " Nkomo" (found the last name with space) ✗
- ...
```

**Generation 4:** Now we're combining Generation 3 programs
```
Testing: Concat (Concat (Upper (Head Variable)) (SValue ".")) 
 (Tail (Substring (Find (SValue " ") Variable) End Variable))

Step by step on "Joshua Nkomo":
1. Head Variable -> "J"
2. Upper "J" -> "J"
3. Concat "J" "." -> "J."
4. Find " " in "Joshua Nkomo" -> position 6
5. Substring from 6 to End -> " Nkomo"
6. Tail " Nkomo" -> "Nkomo"
7. Concat "J." "Nkomo" -> "J. Nkomo" ✓

Checking "Steve Biko": -> "S. Biko" ✓

SOLUTION FOUND!
```

## The Synthesis Landscape: Deductive, Enumerative, and Hybrid Systems

The field of synthesis encompasses a diverse range of algorithmic philosophies, and the state-of-the-art often lies in combining their respective strengths.

- **Deductive Synthesis:** In contrast to the "generate-and-test" approach of enumeration, deductive synthesis is a more constructive approach. It treats synthesis as a theorem-proving task, where a program is derived from a logical specification through a series of deductive steps. This process is often highly efficient and produces programs that are correct by construction. However, pure deductive synthesis is typically applicable only to specific domains and requires specifications in formal logic, making it less general than enumerative search.

- **Hybrid Approaches:** Recognising the trade-offs between generality and efficiency, much of modern synthesis research focuses on hybrid systems that combine the best of both worlds.
  - **Divide-and-Conquer:** This strategy breaks a large, complex synthesis problem into smaller, more manageable sub-problems. For example, a synthesiser might separately enumerate small expressions that are correct on different subsets of the input examples, and then synthesise predicates to distinguish between these subsets, finally combining them into a single solution using a decision tree structure.
  - **Enumerative + Deductive/Deductive:** Another powerful hybrid model uses bottom-up enumeration to generate a library of small, useful program components. A more powerful, top-down deductive or propagation-based algorithm then uses this library of components to construct the final, larger program. This approach, exemplified by systems like  
 Duet and DryadSynth leverage the scalability of deductive methods for composing large programs while retaining the generality of enumeration for discovering the fundamental building blocks.

| Paradigm | Core Mechanism | Primary Strengths | Inherent Weaknesses | Example System(s) |
| --- | --- | --- | --- | --- |
| **Enumerative (Bottom-Up)** | Systematically build larger programs from smaller ones; prune via observational equivalence. | General, finds the smallest solutions. | Suffers from exponential state explosion (the "Scalability Wall"). | eusolver |
| --- | --- | --- | --- | --- |
| **Enumerative (Top-Down)** | Start with a "hole" and recursively fill it in; prune via abstract interpretation. | Can prune large, invalid search branches early. | Pruning effectiveness depends heavily on the quality of the abstraction. | Moito |
| --- | --- | --- | --- | --- |
| **Deductive** | Construct a program as a proof from a logical specification. | Highly efficient, formally correct by construction. | Limited to specific domains and grammar types. | SynQuid |
| --- | --- | --- | --- | --- |
| **Hybrid (Divide & Conquer)** | Combine enumerative/deductive methods to solve sub-problems and compose solutions. | Overcomes scalability limits of pure enumeration; more general than pure deduction. | Complexity in decomposition and composition logic. | Duet, DryadSynth |
| --- | --- | --- | --- | --- |
| **Neuro-Symbolic (LLM-Guided)** | Use a neural model to guide a symbolic search algorithm. | Leverages pattern recognition of LLMs to guide search intelligently. | Can be a black box; guidance may be unreliable; high computational cost. | AlphaGeometry, OpenAI o3 |
| --- | --- | --- | --- | --- |

## What's Next?

In the next post, we'll explore how to specify programs using more than just input-output examples, including partial specifications and logical constraints. We'll also look at how modern synthesis systems use machine learning to guide the search process, making them fast enough for interactive use.
