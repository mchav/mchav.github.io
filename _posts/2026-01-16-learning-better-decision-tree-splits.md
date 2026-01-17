---
layout: post
title: Learning better decision tree splits: LLMs as Heuristics for Program Synthesis
---

A lot of tabular modeling gets easier the moment you stumble onto the right derived quantity. Not something mysterious or "deep." It's usually something you can name: a ratio that turns two raw columns into a rate; a difference that becomes a margin; a simple count that captures what a bunch of messy fields were hinting at.

If you’ve done this enough times, you start to recognize the repeating shapes: "per unit," "per time," "per person," "net," "remaining," "rate." These show up everywhere because they match how the world is measured.

The part I’ve been curious about is: how much of this can we automate without losing that "nameable quantity" feeling?

This post is about an experiment that worked surprisingly well for me. I treat feature engineering as a tiny program synthesis problem: enumerate candidate derived features as small arithmetic expressions over columns. Then I train a decision tree using rules built from those derived features. The twist is how I keep the feature generator from spiraling into unintelligible math: I use an LLM to prune out feature combinations that don't make sense.

The LLMs isn't the model making the modelling decisions. The LLM’s job is much smaller: look at a candidate arithmetic expression and answer one question—does this resemble a meaningful real-world quantity a human might compute?

That one constraint was enough to turn a messy tree into something that looks a lot more like human feature engineering.

[You can clone the repo and try it out yourself](https://github.com/mchav/dataframe/blob/main/app/README.md).

## Features and meaning

When people say feature engineering is “manual,” I think what they really mean is: the good features are the ones you can explain without apologizing.

If I write down clicks / impressions, I can call it “conversion rate.” If I write down price / sqft, I can call it “price per square foot.” If I write down revenue - costs, I can call it “profit.”

Even when those aren’t perfect, they’re at least coherent. They’re quantities that exist in the world.

Now compare that to something like income / zip_code. It might correlate with house price (New York incomes are higher and would be scaled down less because they have smaller valued zip codes compared to Texas, for example). It might even boost accuracy. But it’s not a quantity anyone would compute. It’s a red flag that the search procedure is exploiting quirks in the dataset instead of discovering "features."

This distinction—between "works statistically" and "is a coherent quantity" is exactly what I wanted to encode into the feature generator.

## The demo dataset

I used Kaggle’s Titanic dataset because it’s familiar and because there’s a well-known "starter pack" of feature engineering that people do. In particular, extracting a title from the passenger name ("Mr.", "Mrs.", etc.), deriving a cabin prefix (the leading letter), and doing something with ticket prefixes are all common moves.

I wanted this demo to feel honest: do the obvious parsing that many Titanic solutions do, then stop. Let the system take it from there.

Here’s the full pipeline I ran:

```haskell
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import qualified Data.Text as T
import qualified DataFrame as D
import qualified DataFrame.Functions as F

import Data.Char
import DataFrame ((|>))
import DataFrame.DecisionTree
import DataFrame.Functions ((.&&), (.==), (.=))

import System.Random

$(F.declareColumnsFromCsvFile "./data/titanic/train.csv")

main :: IO ()
main = do
    train <- D.readCsv "./data/titanic/train.csv"
    test <- D.readCsv "./data/titanic/test.csv"

    -- Apply the same transformations to training and test.
    let combined =
            (train <> test)
                |> D.deriveMany [ "Ticket" .= F.whenPresent (T.filter isAlpha) ticket
                                , "Name"   .= F.match "\\s*([A-Za-z]+)\\." name
                                , "Cabin"  .= F.whenPresent (T.take 1) cabin
                                ]
                |> D.renameMany [ (F.name name, "title")
                                , (F.name cabin, "cabin_prefix")
                                , (F.name pclass, "passenger_class")
                                , (F.name sibsp, "number_of_siblings_and_spouses")
                                , (F.name parch, "number_of_parents_and_children")
                                ]
    print combined

    let (train', validation) =
            D.take
                (D.nRows train)
                combined
                |> D.filterJust (F.name survived)
                |> D.randomSplit (mkStdGen 4232) 0.8
        -- Split the test out again.
        test' =
            D.drop
                (D.nRows train)
                combined

        model =
            fitDecisionTree
                ( defaultTreeConfig
                    { maxTreeDepth = 5
                    , minSamplesSplit = 25
                    , minLeafSize = 15
                    , synthConfig =
                        defaultSynthConfig
                            { complexityPenalty = 0
                            , maxExprDepth = 2
                            , disallowedCombinations =
                                [ (F.name age, F.name fare) ]
                            }
                    }
                )
                survived -- Label to predict
                ( train'
                    |> D.exclude [F.name passengerid]
                )

    print model

    putStrLn "Training accuracy: "
    print $
        computeAccuracy
            (train' |> D.derive (F.name prediction) model)

    putStrLn "Validation accuracy: "
    print $
        computeAccuracy
            ( validation
                |> D.derive (F.name prediction) model
            )

    let predictions = D.derive (F.name survived) model test'
    D.writeCsv
        "./predictions.csv"
        (predictions |> D.select [F.name passengerid, F.name survived])

prediction :: D.Expr Int
prediction = F.col @Int "prediction"

computeAccuracy :: D.DataFrame -> Double
computeAccuracy df =
    let
        tp =
            fromIntegral $ D.nRows (D.filterWhere (survived .== 1 .&& prediction .== 1) df)
        tn =
            fromIntegral $ D.nRows (D.filterWhere (survived .== 0 .&& prediction .== 0) df)
        fp =
            fromIntegral $ D.nRows (D.filterWhere (survived .== 0 .&& prediction .== 1) df)
        fn =
            fromIntegral $ D.nRows (D.filterWhere (survived .== 1 .&& prediction .== 0) df)
     in
        (tp + tn) / (tp + tn + fp + fn)
```


A couple things are doing quiet work here.

First: I’m renaming columns so the output reads like English. That matters more than it sounds. If the whole point is interpretability, the printed artifact shouldn’t feel like you’re decoding abbreviations.

Second: notice maxExprDepth = 2. That’s the “I want features I can still name” constraint. Depth 3 can be fun, but depth 2 is usually where you get ratios, differences, and simple interactions without the expression turning into algebra homework.

Third: I set complexityPenalty = 0 in this run on purpose. I wanted to isolate the effect of the LLM filter. In other words: "what happens if the only regularization pressure is semantic?"

## The feature generator

The mechanics are simple. I generate candidate numeric expressions from the columns. Things like `a + b`, `a / b`, `a - b`, `a * b`, plus the raw columns themselves. Then I turn each numeric expression into candidate rules by picking thresholds at a small grid of percentiles.

At that point, tree learning is as boring as you want it to be: pick the rule that best improves impurity, split, recurse, stop at depth/min-sample constraints. For classification trees, using something like Gini impurity is standard practice.

So far, this is just automated feature generation feeding a greedy tree. The entire problem is that the expression generator will happily propose nonsense.

## The problem

When you enumerate expressions naively, you’ll get candidates like Fare + Age. Strange combinations of class codes and counts that are hard to defend. None of this is illegal. Some of it is even predictive. But it’s not the kind of feature engineering that's robust or you’d want to explain to someone else. It’s not the kind of artifact you’d want to ship into a risk-sensitive workflow.

This is a classic program synthesis story in miniature: enumerative search grows quickly, and most of the space isn’t interesting. Priors and pruning are the difference between "works" and "does anything you want."

In my case, the "interesting" region of the search space is: expressions that resemble real derived quantities. Which is a semantic judgment that is awkward to encode by hand. So I outsourced that judgment to a very constrained LLM call.

## The trick: use an LLM as a semantic regularizer

For each candidate numeric expression, I ask an LLM to output a single digit score from 0 to 10. The prompt includes a short rubric: addition and subtraction should combine like-with-like; multiplication and division can produce useful derived quantities; categorical codes should be treated with suspicion; conversion functions don’t matter.

Then I do the most unglamorous thing possible: I drop expressions that score below a threshold.

That’s it. I’m not asking the LLM to invent features. I’m not asking it to choose thresholds. I’m not asking it to "solve Titanic." I’m using it like a regularizer: a pressure that biases what the learner is allowed to consider.

This "LLM as guidance for search" framing is showing up more and more in synthesis-style work: use the model to bias exploration, propose edits, or filter candidates, but keep a separate objective that actually evaluates what works.

I like the mental model: the LLM is the bouncer.

The before/after that made me confident this was worth writing down I ran the exact same pipeline twice.

In the first run, I didn’t apply the semantic filter. I just let the candidate generator dump its expression space into the tree learner and let impurity gain decide.

This is the "without LLM" tree I got (translated from the DSL):

```haskell
  ifThenElse
    (eq (col @Maybe Text "title") (lit (Just "Mr.")))
    
    THEN:
    ├─ ifThenElse
    │    (or
    │      (geq (toDouble (col @Int "passenger_class")) (lit (2.0)))
    │      (eq (col @Maybe Text "cabin_prefix") (lit (Nothing)))
    │    )
    │    
    │    THEN:
    │    ├─ lit (0)
    │    
    │    ELSE:
    │    └─ ifThenElse
    │         (lt
    │           (ifThenElse
    │             (eq (toDouble (col @Int "number_of_siblings_and_spouses")) (lit (0.0)))
    │             THEN: divide
    │                     (toDouble (col @Int "passenger_class"))
    │                     (toDouble (col @Int "number_of_siblings_and_spouses"))
    │             ELSE: lit (0.0)
    │           )
    │           (lit (1.0))
    │         )
    │         
    │         THEN: lit (0)
    │         ELSE: lit (1)
    
    ELSE:
    └─ ifThenElse
         (lt (toDouble (col @Int "passenger_class")) (lit (3.0)))
         
         THEN:
         ├─ lit (1)
         
         ELSE:
         └─ ifThenElse
              (gt
                (mult
                  (toDouble (col @Int "number_of_parents_and_children"))
                  (toDouble (col @Int "number_of_siblings_and_spouses"))
                )
                (lit (1.0))
              )
              
              THEN:
              ├─ lit (0)
              
              ELSE:
              └─ ifThenElse
                   (and
                     (eq (col @Maybe Text "Embarked") (lit (Just "S")))
                     (geq (col @Maybe Double "Age") (lit (Just 14.650000000000002)))
                   )
                   
                   THEN:
                   ├─ ifThenElse
                   │    (gt
                   │      (ifThenElse
                   │        (eq (toDouble (col @Int "number_of_siblings_and_spouses")) (lit (0.0)))
                   │        THEN: divide
                   │                (toDouble (col @Int "passenger_class"))
                   │                (toDouble (col @Int "number_of_siblings_and_spouses"))
                   │        ELSE: lit (0.0)
                   │      )
                   │      (lit (1.0))
                   │    )
                   │    THEN: lit (0)
                   │    ELSE: lit (1)
                   
                   ELSE:
                   └─ lit (1)
```

The DSL is in prefix/Polish notation. So `(eq (col @Maybe Text "title") (lit (Just "Mr.")))` means `title == "Mr."`. The `Maybe` part is just how Haskell deals with nulls.

Even if you don’t read every branch, you can feel the flavor. There are lots of conditions that look like dataset artifacts: dividing passenger class by number of siblings, multiplying number of parents with number of siblings. This is exactly what happens when the search is allowed to treat "anything correlated" as a legitimate feature.

The training accuracy and validation of this decision tree were 0.82.

What happens when we use the LLM?

Here’s the second tree:

```haskell
model =
  ifThenElse
    (and
      (lt (toDouble (col @Int "ticket_class")) (lit (3.0)))
      (eq (col @Text "Sex") (lit ("female")))
    )
    
    THEN:
    ├─ lit (1)
    
    ELSE:
    └─ ifThenElse
         (and
           (eq (col @Maybe Text "cabin_prefix") (lit (Nothing)))
           (eq (col @Maybe Text "title") (lit (Just "Mr.")))
         )
         
         THEN:
         ├─ lit (0)
         
         ELSE:
         └─ ifThenElse
              (geq
                (add
                  (toDouble (col @Int "number_of_parents_and_children_aboard"))
                  (toDouble (col @Int "number_of_siblings_and_spouses_aboard"))
                )
                (lit (3.0))
              )
              
              THEN:
              ├─ lit (0)
              
              ELSE:
              └─ ifThenElse
                   (or
                     (gt (toDouble (col @Int "ticket_class")) (lit (1.0)))
                     (geq (col @Maybe Double "Age") (lit (Just 14)))
                   )
                   
                   THEN:
                   ├─ lit (0)
                   
                   ELSE:
                   └─ lit (1)
```

The initial condition is already immediately readable: women in first or second class survived. Even deeper into the tree it remains readable - large faimiles didn't survive (encoded in the conditon: if number of parents and siblings is greater than 3 then 0).

The training and validation accuracy of this decision tree were both 0.83. Better than the first model!

More importantly the tree was simpler. The change wasn’t subtle. The tree got smaller and more coherent. It rediscovered features that are basically "human Titanic features" like family size, and simple interactions with sex. In other words: it started behaving like a feature engineering notebook, not like a correlation-miner.

This is the moment the "LLM as regularizer" framing really clicked for me. The learner didn’t change. The objective didn’t change. The only thing that changed was the hypothesis space: which derived quantities were allowed to exist as candidates.

And that was enough to nudge the output from "math that correlates" to "features you can name."

If you’re reading this and thinking “Titanic is toy-ish,” I agree. I used it because everyone already knows what reasonable features look like, which makes the interpretability comparison crisp. The real test is whether this behavior carries over to messier tabular problems where feature engineering actually hurts.

## Why I think this is a nice way to use LLMs

There are two reasons I like this pattern.

First, it doesn't throw the entire problem at the LLM hoping it gets it right. The LLM is asked to do semantic triage. That’s a smaller, more stable job.

Second, it treats interpretability as a first-class constraint. If your model is going to be read by humans, you don’t want to optimize only for accuracy and then bolt on interpretability later. You want the candidate language itself to be biased toward human-shaped quantities.

What I have right now is still a prototype, but the shape feels right: a small synthesis loop producing candidate features, a classic learner picking among them, and an LLM providing a soft semantic prior when you don’t have explicit unit metadata.

## Limitations (the kind you notice immediately)

The first limitation is determinism. If you want this to be reproducible, you’ll want caching: expression string → score. You’ll also want deterministic decoding, or at least stable settings.

The second limitation is schema context. The rubric can only be as good as the column names. If the dataset has columns like X1, X2, A, B, you shouldn’t expect semantic miracles. The filter can become much better if you pass minimal schema descriptions ("this is dollars," "this is years," "this is a categorical ID").

The third limitation is that "meaningful quantity" is subjective. There are domains where "Age <= Fare" might encode something real (it’s still weird, but you get the idea). This is why I think of the score as a regularizer rather than a hard rule: it’s a knob you tune based on how strict you want the feature language to be.

## What I want to try next

The next steps feel straightforward.

One is to distill the LLM into something cheaper. Once you have a cache of labeled expressions, you can train a small local classifier that approximates the semantic score and only calls the LLM for uncertain cases.

Another is to combine semantic regularization with structural regularization. In the demo run above I set the explicit complexity penalty to zero to isolate the LLM effect. In practice, you can (and probably should) use both: penalize expression size while also filtering out expressions that fail a semantic sanity check.

And the most exciting direction, for me, is to apply this to real tabular workflows where the "right derived quantities" aren’t obvious, but still exist: fraud, risk, forecasting, ops metrics, churn, all the places where humans already reason in rates, margins, and per-unit measures.

If nothing else, this experiment made one thing feel concrete: there’s a useful middle ground between "manual feature engineering forever" and "opaque feature learning." You can automate the search, and still keep the artifact readable, if you treat semantics as something the search needs to respect.
