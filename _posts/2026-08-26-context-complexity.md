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

As someone who mostly writes code in an information dense but obscure language with a good compiler, I didn't quite know how that combination would play out. It wasn't immediately obvious to me in what direction these factors would drive the cost and speed of solving tasks.

Generating programs is effectively a search problem. Rather, it is a number of search tasks bundled into one. Classical program synthesis makes this evident. Before the advent of LLMs, generating programs usually meant starting with the grammar of a language and cleverly searching the space of programs against a specification. Those methods didn't scale well because the space of all possible programs is extremely large (I have a quick primer on the topic [here](https://mchav.github.io/an-introduction-to-program-synthesis/)). The current LLM regime obscures this search by "memorizing" a lot of tasks. If I ask an LLM for a function that solves fizzbuzz, chances are it only needs to look into its weights. The cost of search collapses into the cost of inference. But ask it to write fizzbuzz in a programming language invented last week and the search rears its ugly head again. Now it can't do much without reaching for tools and is at the mercy of its context window. When working with novel tasks the search problem has a number of measurable costs: how much information the model needs before it can proceed, how many facts it must hold at once, and how much irrelevant material it can absorb before it is steered in the wrong direction. I call the sum total of these costs "context complexity."

This blog post is a report of a set of experiments I ran to investigate how to better design tools to minimise context complexity. Recent work has started varying the surface too, restructuring tool documentation ([EasyTool](https://aclanthology.org/2025.naacl-long.44/)), injecting specification defects ([WildAGTEval](https://arxiv.org/abs/2601.00268)), manipulating tool context ([ToolScope](https://aclanthology.org/2026.acl-long.1573/)), and optimising docs from failed traces ([DocsChisel](https://arxiv.org/abs/2608.10037)). This post explores how to think of it as a growing quantity to optimize over long multiple tasks.

## The setup

To see anything I had to use small models. Large models make for bad study subjects since they have both very large context windows and a lot of information baked into their weights. The models I tested on are local, small models with between 4 and 20 billion parameters.

The environment is a [Haskell notebook](https://github.com/DataHaskell/sabela) driven through a set of search and execution tools (called siza in the aforementioned repository) that do the following:

* list and read cells
* search for a function or module (type signatures, function bodies, etc)
* compile a candidate in a scratch session
* commit a cell that runs.

The experiments were done in a mangled frenzy when I had free time from work so I wasn't particularly disciplined about checkpointing. However, the results described in this post are a summary of 84 recorded live episodes and 150 benchmark runs, over four tasks (listed by increasing difficulty).

1) Plot a sine wave, where the points are trivial and the only difficulty is finding the drawing function.
2) Showing some summary statistics for the iris dataset saved on disk as a Parquet file (there are few working Haskell Parquet readers).
3) Load the wine dataset and show summary statistics, deliberately underspecified, with no local file and no library named.
4) Use a relatively new effects library to do a simple task. The types of the functions are complicated so this task tested how well I could surface information to the model to get it unstuck.

I designed the tool to minimise context use and instead they rely heavily on the LLM being able to reason about types and function names. As a control I used opencode's vanilla harness which provides some generic tools.

I ran these sessions with gemma4 and gpt-oss:20b to ensure that I wasn't just learning how to game a single model's shortcomings. The summary below picks relevant examples to explain a few principles I took away.

## 1) Types are as informative as examples

Task 4  was the best way to test how well a model could chase types. The model is extremely unlikely to know what Bluefin even is. The model vaguely knew that state effects have `get`, `put` and `modify` primitives so it searched and got back the following results.

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

It had never seen a bluefin program. The model learnt how to write this after a few hundred tokens of searching as opposed to grepping through the internet or some locally downloaded files. The signatures are alos a source of truth that keeps the model from "knowing" out of date information.

## 2) Type checking is better than running the full program

On the iris task the model wanted to read a parquet file. It searched, found `parquet`, found a function called `readParquet` then checked its type with:

**tool call:** `check_type {"expr": "DataFrame.readParquet"}`

```
readParquet is defined in DataFrame (package dataframe)
  readParquet :: FilePath -> IO DataFrame
Package dataframe is not declared by this notebook. Add this as a cell's FIRST line,
then import the module:
-- cabal: build-depends: dataframe
No notebook cell imports DataFrame.
Add this import:
import DataFrame
```

We anticipate the model using this function and embed the installation convention in the output so it doesn't have to consult the tool again to find out how to add packages.

The same principle governs the write path. Nothing is committed unless it compiles so the model doesn't get stuck iterating on broken code.

## 3) Repair obvious mistakes so they don't eat up context

In the bluefin run the model's very first write was `import Bluefin` on its own, with no dependency declared. The acknowledgement came back:

```
"note":"Applied GHC's suggested fix before committing: declared
        build-depends: bluefin ==0.7.0.1. The compile gate compiled this
        candidate in a disposable session and did not run it there."
```

The alternative was a missing-package diagnostic into the context, a retry, and both copies side quests clogging up the context. The repair meant only communiating the result in a single line. Casing and naming slips (models trip up on these a lot) are handled similiarly when the intent is clear. When only part of an error has an obvious fix, the harness applies just that fix and reports the rest:

```
"partialRepair":{"applied":["declared build-depends: dataframe"],
  "note":"Nothing was committed. The fixes are in `compiledSource`, which is
          the text the compiler read, and the diagnostic is what remains after
          them. Each fix measurably reduced the errors, which is not the same
          as being right."}
```

## 4) Re-parse diagnostics and use them as a way to reprompt

A raw compiler diagnostic is a wall of text. Re-parsed, it can become the next prompt. When the median candidate failed on a type mismatch, the harness took the offending argument, replaced it with a typed hole in a throwaway compile, and returned what compiler said could fill the hole:

```
"holeRewrite":{"of":"DF.col","with":"(_sabelaHole col)",
  "holeType":"DF.Expr a1 -> Text",
  "holeFits":[{"write":"DF.name",
               "type":"forall a. Show a => DF.Expr a -> Text"}, ...],
  "by":"the harness, in a throwaway compile; your cell was not edited
        and nothing was committed"}
```

Instead of "couldn't match expected type", the model reads "the thing in this position needs `DF.Expr a -> Text`, and `DF.name` has that type". The same machinery catches invented identifiers. When a model hallucinated `renderSVG`, the harness replied with: "renderSVG is not defined in the session or any imported module. With a typed hole in its place, the call site needs: [Char] -> SizeSpec V2 Integer -> t0 -> t". We prompt the model on what to do next rather than complain that it has invented a name we don't recognize. More improtantly we direct it to a cheap way to unblock itself.

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

The measurement I've settled on needs four ingredients:

* a fixed model, with fixed sampling settings, so runs are comparable.
* a workload of `n` stages scripted before the run. A bad tool causes extra turns, so if the session decides its own length then `n` is both the input and the outcome of the experiment.
* a check that the result is actually correct.
* a required success rate `q` so that a reject-everything model doesn't seem cntext efficient.

Then:

```
    CC(n)  =  the smallest budget b such that

              P[ the check passes before resource R exceeds b ]  >=  q
```

In words: the context complexity of a tool surface is the smallest budget that gets an agent through an `n`-stage workload reliably. I track the resource `R` as several numbers (tool calls, tool-result bytes, total input tokens, output tokens, wall clock) and only convert to money at the end. If you convert too early, the provider's pricing and caching policy leak into what should be a property of the tool design. Defining the measure as a budget also handles sessions that never succeed: you can't average "calls until success" when some runs never finish, but "how often do runs succeed within this budget" is always well defined.


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
