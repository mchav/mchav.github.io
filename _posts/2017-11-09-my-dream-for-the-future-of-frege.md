---
layout: post
title: My dream for the future of Frege
---

Almost all the time when I'm working on a project in a functional language such as Haskell or Frege and someone peers
over my shoulder to check what I'm doing, the conversation tends to begin with the person inquiring what language I'm
coding in. The follow up question is inevitably what the language is used for. I struggle to answer this question every time.
I could say it is a general purpose language that can be used for anything but that shies away from the question. If asked
what any other language is used for I'd likely give more definite answers. For Python I'd say it is big in the data science world and in academia because it's good for quick prototyping and has a large package ecosystem for both uses. For C++ I'd say that it's a good compromise between low-level programming and expressivity (compared to C that is). For Java I'd say it's used for a lot of commercial software and also for Android. There are similarly simple answers for languages like Kotlin, Julia and R. But for Frege/Haskell, I can never give as confident an answer. Part of it is because the language communities themselves recognise that there is no killer application or use-case one can point to that REQUIRES the use of Haskell/Frege over any other language. As such the package ecosystem hasn't evolved in a particular direction.

This post motivates what I think is a good use case for functional languages in the near to distant future.

Let me start with what inspired this.

I was writing a MapReduce job at work in Java. The code was verbose and took quite a long time to set up. I won't go into 
a detailed explanation of the [API](https://research.google.com/pubs/pub35650.html) I was working but roughly, writing
a mapreduce to do a simple data dump similar to that of the example of the paper linked above. Observe the following
piece of code (from the example in the paper):

```java
PCollection<String> words = lines.parallelDo(new DoFn<String,String>() {
  void process(String line, EmitFn<String> emitFn) {
    for (String word : splitIntoWords(line)) {
      emitFn.emit(word);
    }
  }
}, collectionOf(strings()));
```

What this function does, is traverse the dataset line by line, separate the line by whitespace, then use a given `EmitFn` 
(Emit Function) to place the data back into the collection. The function uses the `EmitFn` argument to express the
non-deterministic nature of a `DoFn` (the function that traverses the data). That is, given a single line we could return
many items (in this case, many words) or we could return no words at all, (if the line was just white space). This is
the sort of [non-determinism modeled by lists](https://en.wikibooks.org/wiki/Haskell/Understanding_monads/List). Thus what
we really wanted was for our API to like this:

```java
PCollection<String> words = lines.parallelDo(new DoFn<String,String>() {
  List<String> process(String line) {
    return splitIntoWords(line);
  }
}, collectionOf(strings()));
```

Now the relationship between the input and the output is a bit clearer. For each word we create a list of words each of which
we place back into the `PCollection`. I took some time to make some ad-hoc Frege bindings of the library and the API looked
much cleaner.

```haskell
module Example where

import Flume
import Java.util.Regex

main = do
  FlumeFrege.init ()
  source <- TextIO.source "someFile"
  playLines <- Flume.read source
  let lines = splitLines playLines
  print lines
  FlumeFrege.done ()

{- words is a function that, given a string, returns a list of strings after splitting the input by whitespace -}
splitLines :: PCollection String -> PCollection String
splitLines playLines = parallelDo playLines words
```

`parallelDo` Now takes as input, the collection it is transforming as well as the function that drives the transformation.
The result was not only terser but easier to add other operations to. The ease with which one can create and modify data
pipelines given higher order functions as well as pure functions is potentially a big productivity winner. This is likely
why Scala + Spark experienced a spike in usage some years ago (and to my knowledge still do). Big data was a great niche.
If you were doing independent transformation on large datasets then the usual fears and arguments for not adopting a new
language didn't quite hold. You didn't need to worry much about integration with your current codebase or the tooling
ecosystem of the language. The task was easy to isolate. But in general, functional languages did not come to the
forefront as one would have expected. The world continued unperturbed.

The age of big data turned into the age of machine learning. We had good tooling for collecting and aggregating data, now
we needed a way to understand it. Again there was talk about using functional language for machine learning models,
specifically [neural networks](http://colah.github.io/posts/2015-09-NN-Types-FP/). To verify that this was not all talk,
I put together a Frege Tensorflow API. I have a few examples (to be released) from the library that prove concretely
that functional languages would make for composable, clear as describe in the aforementioned article.
Functional languages, again, promised a simple intuitive design enforced by composition and purity. But the ecosystems [weren't quite prepared](https://news.ycombinator.com/item?id=14402378) for this new age. Again, a potentially good use case came and went.

My vision is for a functional language to become the go-to data science language in close competition with Python and R.

Currently, my vision sees Frege (or Eta) being that language.

I recently inquired about this same topic on [Reddit](https://www.reddit.com/r/haskell/comments/7brsuu/machine_learning_in_functional_programming/)
and a very insightful discussion sprang from it. It seems GHC Haskell is a long way away from being the language to carry
through with this vision.

Why a JVM-based language? You get Java interop which means the arguments about the ecosystem not being quite
ready aren't as potent. Quite a number of respectable data science and machine learning libraries already exist.
It would be great to develop on top of them with Frege in the way that Keras builds on TensorFlow. ND4j, deeplearning4j, MLlib, Tensorflow and Weka are all well maintaned packages in the Java data science ecosystem. One of the reasons they aren't more widely adopted, in my opinion is because of the Java isn't the language you think to go to if you want to iterate quickly, as most data scientists usually want to do.

There are arguments in the thread about Haskell's type system not being strong enough to encode simple linear algebra.
A potential solution would be to use Idris which, luckily, has a JVM backend as well.

Regardless, there has been a lot of groundwork done, and it will be easiest to build on top of it if efforts are focused
on creating a Frege that excels as a data science tool. I will continue work in this fashion to lay the groundwork
for this vision.
