---
layout: post
title: "Context complexity: what is the Big-O of an agent API?"
---

TLDR;
* small, local models are a good instrument for measuring what a harness costs an agent.
* an API is only as good as its outputs' specificity, parsimony, and truthfulness.
* obvious code repair is cheaper done in your tools than elicited from the model through another retry, provided the tool discloses what it changed.

Earlier this year I saw a post comparing the [token efficiencies of different programming languages](https://news.ycombinator.com/item?id=46582728). The post itself wasn't a thorough treatment of the issue but it did create a pretty interesting discussion around what makes some languages better for LLMs than others. I've also seen assertions flying around online claiming flavours of the following:

* [a language with a good compiler is perfect for LLMs because the compiler acts as a correctness oracle of sorts](https://x.com/gdb/status/2007228511363444905)
* Python is the best programming language to use for AI since there are so many examples of it
* terse/information dense languages are better for LLMs since, compared to verbose languages, it takes fewer tokens to generate a program.

As someone who mostly writes code in an information-dense but obscure language with a good compiler, I didn't quite know how that combination would play out. It wasn't immediately obvious to me in what direction these factors would drive the cost and speed of solving tasks.

Generating programs is effectively a search problem. More specifically, it is a number of search tasks bundled into one. Before the advent of LLMs, automated code generation was the concern of a field of theory called program synthesis. Generating programs usually meant starting with the grammar of a language and cleverly searching the space of ALL POSSIBLE programs against a specification. Program synthesis methods didn't scale well because the space of all possible programs is extremely large (I have a quick primer on the topic [here](https://mchav.github.io/an-introduction-to-program-synthesis/)). The current LLM regime hides this search cost from a user by effectively "memorizing" a lot of tasks. If I ask an LLM to create a function that solves fizzbuzz, chances are it can generate one easily without consulting external sources. In this case, the cost of search is exactly equal into the cost of inference. The difficulty of making sure you can even pick the right program is hidden in a laborious and data-intensive training process. But if you ask an LLM to write fizzbuzz in a programming language invented last week, search rears its ugly head again. Now it can't do much without reaching for tools and is at the mercy of its context window. When working with novel tasks (new libraries or new concepts) the search problem has a number of measurable costs: how much information the model needs before it can proceed, how many facts it must hold at once, and how much irrelevant material it can absorb before it is steered in the wrong direction. I call the sum total of these costs "context complexity."

This blog post is a report of a set of experiments I ran to investigate how to better design tools to minimise context complexity. Recent work has started varying the surface too, restructuring tool documentation ([EasyTool](https://aclanthology.org/2025.naacl-long.44/)), injecting specification defects ([WildAGTEval](https://arxiv.org/abs/2601.00268)), manipulating tool context ([ToolScope](https://aclanthology.org/2026.acl-long.1573/)), and optimising docs from failed traces ([DocsChisel](https://arxiv.org/abs/2608.10037)). This post explores how to think of it as a growing quantity to optimize over long multiple tasks.

## Some modelling choices

To see anything I had to use small models. Frontier LLMs make for bad study subjects since they have both very large context windows and a lot of information baked into their weights. The models I tested on are local, small models with between 4 and 20 billion parameters.

The environment is a [Haskell notebook](https://github.com/DataHaskell/sabela) driven through a set of search and execution tools (called siza in the aforementioned repository) that do the following:

* list and read cells
* search for a function or module (type signatures, function bodies, etc)
* compile a candidate in a scratch session
* commit a cell that runs.

The experiments were done in a mangled frenzy when I had free time from work so I wasn't particularly disciplined about checkpointing. However, the results described in this post are a summary of 84 recorded live episodes and 150 benchmark runs, over four tasks (listed by increasing difficulty).

* Task 1: Plot a sine wave, where the points are trivial and the only difficulty is finding the drawing function.
* Task 2: Showing some summary statistics of a subset of columns for the iris dataset saved on disk as a Parquet file (there are few working Haskell Parquet readers).
* Task 3: Load the wine dataset and show summary statistics, deliberately underspecified, with no local file and no library named.
* Task 4: Use a relatively new effects library (rich and complex types) to do a simple task (add nubmers 1 through 10). The types of the functions are complicated so this task tested how well I could surface information to the model to get it unstuck.
* Task 5: Use hanalyze (simpler types) to do a slightly more complicated task. Do a linear regression on a dataset.

I designed the tool to minimise context use and instead they rely heavily on the LLM being able to reason about types and function names. As a control I used opencode's vanilla harness which provides some generic tools.

I ran these sessions with gemma4 and gpt-oss:20b to ensure that I wasn't just learning how to game a single model's shortcomings. The summary below picks relevant examples to explain a few principles I took away.

## 1) Types are as informative as examples

Task 4 was the best way to test how well a model could chase types. The prompt was:

```
Can you use bluefin's state effect to create a function that sums numbers from 0 to n and then call that function on n = 10?
```

The model is extremely unlikely to know what Bluefin even is since the library is new. Bluefin models effects with very expressive types. Read along with the function names, these can help inform what the final program should look like. The hypothesis here is that, a model should be able to reason about how to use the library given a combination of type information and compile time errors/nudges. When the model began the task, it vaguely knew (from hoarding information form the internet) that state effects, by convention, have `get`, `put` and `modify` primitives so it searched and got back the following results.

```
modify     :: (e < es) => State s e -> (s -> s) -> Eff es ()
get        :: (e < es) => State s e -> Eff es s
evalModify :: s -> (forall e. Modify s e -> Eff (e :& es) a) -> Eff es a
```

The model followed the types to:

```haskell
sum1to10 :: Eff es Int
sum1to10 = evalModify 0 $ \m -> do
  forM_ [1..10] $ \i -> modify m (+ i)
  get m

runPureEff sum1to10
```

The model learnt how to write this after a few hundred tokens of searching as opposed to grepping through the internet or some locally downloaded files. The signatures are alos a source of truth that keeps the model from "knowing" out of date information.

Given no tools (a raw opencode session with GPT-OSS:20B) the model either refused to use the library because it was unfamiliar, or filled its context with an internet search, which prevented it from completing the task.

## 2) Type checking is better than running the full program

On the iris task the model wanted to read a parquet file. It searched, found `parquet`, found a function called `readParquet` then checked its type with:

**tool call:** `check_type {"expr": "DataFrame.readParquetWithOpts"}`

```
readParquetWithOpts is defined in DataFrame (package dataframe)
  readParquetWithOpts :: ParquetReadOptions -> FilePath -> IO DataFrame
Package dataframe is not declared by this notebook. Add this as a cell's FIRST line,
then import the module:
-- cabal: build-depends: dataframe
No notebook cell imports DataFrame.
Add this import:
import DataFrame
```

For very expensive I/O, tracking the types in this way is a cheaper oracle than running a candidate function to prod at how it works. Each failed run where the model tries to learn the shape of the arguments could cost minutes. At worst, in an untyped language, it could put the reader into some undefined state crashes your computer. The types here provide an extra layer of safety and allow the model to explore with no side effects. The model can then go ahead and compose this function confidently with other parts of the program because it understands what call should look like.

## 3) Repair obvious mistakes so they don't eat up context

In the bluefin run the model's very first write was `import Bluefin`. It didn't really concern itself with where the package was installed and if bluefin even existed in the environment. Siza auto-fixed this case and sent back an advisory:

```
"note":"Applied GHC's suggested fix before committing: declared
        build-depends: bluefin ==0.7.0.1. The compile gate compiled this
        candidate in a disposable session and did not run it there."
```

The alternative was putting a missing-package diagnostic into the context then asking the model to retry. That side quest is a waste of context for something we know how to repair. Since models tend to fail in similar ways we can have a library of common repairs that we enact (inspired by [Project Jisaw](https://arxiv.org/pdf/2112.02969) out of Microsoft Research):

```
"partialRepair":{"applied":["declared build-depends: dataframe"],
  "note":"Nothing was committed. The fixes are in `compiledSource`, which is
          the text the compiler read, and the diagnostic is what remains after
          them. Each fix measurably reduced the errors, which is not the same
          as being right."}
```

## 4) Re-parse diagnostics and use them as a way to reprompt

A raw compiler diagnostic is a wall of text. Re-parsed, it can become the next prompt. Task 5 was a good exemplar for this. The compiler can, at times, be a more direct nudge to the right answer. When asked to do a regression the model hallucinated the name `train` for a function to train a regression model.

```haskell
main :: IO ()
main = do
  eDf <- loadCSV "examples/data/housing.csv"
  case eDf of
    Left err -> putStrLn $ "Error loading CSV: " ++ show err
    Right df -> do
      let maybeModel = train df ("median_income") ("median_house_value")
      case maybeModel of
        Nothing -> putStrLn "Could not train model"
        Just res -> -- use the model here
```

Rather than throwing a generic function not found error. We can intercept this error and use typed holes to repair the code. That is pass `let maybeModel = _ df ("median_income") ("median_house_value")` and get a list of acceptable function back. In this case, the compiler rightfully gave us `fitDataFrameLM` (one of two functions that fit this shape) to produce the program below.

```haskell
{-# LANGUAGE OverloadedStrings #-}
-- cabal: build-depends: hanalyze, hanalyze-frame, dataframe
import Hanalyze.DataIO.CSV (loadCSV)
import DataFrame (DataFrame, toText) -- maybe not needed
import Hanalyze (fitDataFrameLM, coefficients, rSquared)
import Data.Text (Text)
import Control.Monad (when)

do
  eDf <- loadCSV "examples/data/housing.csv"
  case eDf of
    Left err -> putStrLn $ "Error loading CSV: " ++ show err
    Right df -> do
      let maybeFit = fitDataFrameLM df ("median_income") ("median_house_value")
      case maybeFit of
        Nothing -> putStrLn "Could not fit model"
        Just res -> do
          putStrLn "Coefficients:"
          print $ coefficients res
          putStrLn "R-squared:"
          print $ rSquared res
```

The alternative would be to tell it the function doesn't exist and rely on it to keep hallucinating new names until it finds the right one. Or do an internet search side quest which ends up taking up all of its time.

The last, and most underrated, re-parsing is the model's own repetition. Small models loop when they are stuck and tend re-ask a question. We can "remind" the model that it has already done this without adding the same information back into the context.

```
{"query":"bluefin [limit=10] [mode=inventory]","state":"duplicate",
 "summary":"already held: bluefin (installed-not-loaded):
            -- cabal: build-depends: bluefin — provides `bluefin`. ..."}
```

Now, a duplicate costs a one-line summary instead of the full JSON payload.

## A start to computing context complexity

Suppose stage `i` of a task adds `u_i` bytes to the context. Those bytes are re-sent on every remaining stage, so the total input over `n` stages is:

```
    W(n)  =  sum over i of (n - i + 1) · u_i
```

If each stage adds about the same amount, the sum is quadratic in `n`: tool calls grow linearly while total bytes grow quadratically. This is why I think of context complexity as a growth rate. Two tool designs can cost about the same on a single question and grow at completely different rates over a long session, so measuring single prompts can't always separate a good design from a bad one.

## Counting context complexity

A initial swing at the problem suggests we should measure these four things:

* a fixed model, with fixed sampling settings, so runs are comparable.
* a workload of `n` stages scripted before the run. A bad tool causes extra turns, so if the session decides its own length then `n` is both the input and the outcome of the experiment.
* a check that the result is actually correct.
* a required success rate `q` so that a reject-everything model doesn't seem cntext efficient.

Then:

```
    CC(n)  =  the smallest budget b such that

              P[ the check passes before resource R exceeds b ]  >=  q
```

That is, the context complexity of a tool surface is the smallest budget that gets an agent through an `n`-stage workload reliably. I track the resource `R` as several numbers (tool calls, tool-result bytes, total input tokens, output tokens, wall clock) and only convert to money at the end. If you convert too early, the provider's pricing and caching policy leak into what should be a property of the tool design. Defining the measure as a budget also handles sessions that never succeed: you can't average "calls until success" when some runs never finish, but "how often do runs succeed within this budget" is always well defined.


To make the accounting concrete, here it is applied to two successful bluefin runs, both on gpt-oss:20b but after some improvements to the tooling's context management.

| | Aug 25 | Aug 2 |
|---|---|---|
| tool calls | 36 | 15|
| searching | 4 discover, 8 check_type, 6 read_source | 3 discover |
| scratch compiles (`try`) | 7 | 7 |
| tool-result bytes | 21K | 35K |
| cumulative input | ~340K chars | ~1.1M chars |


## Conclusion

As more and more coding becomes agentic, it'll become more and more important to think about how we engineer for better context usage. The trade off is similar to time and space complexity in algorithm analysis. An algorithm can use memory to save steps, or use steps to save memory. A tool surface has the same choice between context and computation. An "inefficient" design may hand a lot of information to the model to save having to do more thoughtful computation, letting the model search and retry its way to an answer. But this cost lingers for the entire session since LLMs are autoregressive (meaning they don't "remember" anything and instead they send the entire session text to the computer every time to get back the next set of tokens).

On the other hand an "efficient" design might spend compute in the tools themselves, using compute cycles on indexing, ordering, and mechanical code repair, so that less needs to enter the context at all, and the LLMs only do semantic repairs.

There are interesting engineering problems in both camps.
