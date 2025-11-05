---
layout: post
title: An introduction to program synthesis (Part II): Enumerative-bottom up search
---

## Introduction
This post kicks off the second part of a hands-on series about program synthesis. We'll apply the previously explored technique (an enumerative bottom-up search) to a slightly more realistic problem: automatically generating features for the [Iris dataset](https://archive.ics.uci.edu/dataset/53/iris).

## A recap
In the last post, we created a Flash-fill-style system that generates programs that transform strings given input-output examples as specifications. We defined a domain-specific language and searched the space of all possible programs to find one that produced all the inputs for our desired outputs.

This search technique (enumerative bottom-up search) is effectively a breadth-first search. The high-level algorithm is as follows:

```java
search(inputs, outputs) {
 programs = all_simple_programs()
    while (true) {
        for (program : programs) {
            if (satisfiesExamples(inputs, outputs, program)) {
                return program
 }
 }
 programs = deduplicate(inputs, expand(programs))
 }
    // no program was found
    return null
}
```


## The limits of this approach

### Combinatorial explosion
We tried to deal with the combinatorial explosion by deduplicating and keeping our DSL small. While effective for our toy problem, most real-world problems aggressively resist attempts to be so easily constrained. Interesting problems typically require expressive DSLs (binary and ternary functions, recursive functions, conditional logic, etc.), which make searching much more difficult. Deduplicating expressions also gets difficult as the DSL becomes more expressive.

We'll have to figure out a way to tame the algorithmic complexity of the problem without hamstringing our search.

### What is a correct program?
Checking if a program satisfies our examples works well when we can trust that the examples:
* are complete and representative of the problem
* correctly specify the underlying program we are trying to discover.

When dealing with data in the wild, we can't always guarantee that our inputs or outputs are correct. Our data is typically a noisy sub-sample of a larger distribution. Finding the exact program that satisfies a given set of examples might create an inflexible, complicated program that tries to get every single example right. In mathematical modelling, we call this overfitting: when a program fits the training examples but doesn't generalise well.

We're going to have to come up with approximate notions of "correctness" that can let us know when we've found a program that's good enough for the problem.

## Our problem

In a [previous post](https://mchav.github.io/iris-classification-in-haskell/), we trained a simple neural network to classify plants into three iris species. With some very light, hand-wavy feature engineering, we did well.


 variety   | precision |  recall  
------------|-----------|----------
 Setosa     | 1.0       | 1.0      
 Versicolor | 0.9677419 | 0.9375   
 Virginica  | 0.9459459 | 0.9722222

Our test set performance wasn't as good as we'd hoped.


 variety   | precision |  recall  
------------|-----------|----------
 Setosa     | 1.0       | 0.9285714
 Versicolor | 0.8888889 | 0.8888889
 Virginica  | 0.8666667 | 0.9285714

Our challenge for this post will be to engineer better features using symbolic regression.

Wait...symbolic regression? What happened to program synthesis? Well, symbolic regression is a kind of program synthesis in which the programs are mathematical expressions, and instead of finding the exact program, we find one that fits a dataset. The underlying algorithm is practically the same:

```java
search(inputs, outputs, depth) {
 formulas = all_simple_formulas()
    while (depth > 0) {
 forumlas = deduplicate(inputs, expand(formulas))
 }
    return pickBestFormula(inputs, outputs, formulas)
}
```

There are a couple of significant differences between this and our earlier synthesis algorithm.

* Our target is uncertain, so we have to constrain our search to only work until a certain depth.
* Because we are looking for a function that best approximates the relationship between the input and output, we have to include some logic to pick the best solution (for some definition of best).

This process should yield features we can directly plug into our neural network.

A prototype of this approach exists in the [dataframe library](https://github.com/mchav/dataframe), so this post will explain that code. I have simplified the code for this post.

## Why feature engineering?
Feature engineering is a more straightforward target than finding an exact program (we'd be building a fully fledged classifier). The result of feature engineering must have a strong relationship with the target variable. We'll see some tools for finding exact expressions later in this post.

## Our search space
The dataframe library defines an expression DSL that looks roughly like this:

```haskell
data Expr a where
 Col :: String -> Expr a  -- A reference to a column in the dataframe
 Lit :: a -> Expr a       -- A literal value
 UnaryOp :: (b -> a) -> Expr b -> Expr a
 BinaryOp :: (c -> b -> a) -> Expr c -> Expr b -> Expr a
```

This DSL is effectively the same expression language we defined in [rewriting dataframes for MicroHs](https://mchav.github.io/rewriting-dataframes-for-microhs/).

We can define some mathematical operations using this DSL.

```haskell
add :: (Num a) => Expr a -> Expr a -> Expr a
add = BinaryOp (+)

sub :: (Num a) => Expr a -> Expr a -> Expr a
sub = BinaryOp (-)

cos :: Num a => Expr a -> Expr a
cos = UnaryOp Prelude.cos

...
```

These will be our simple programs/formulas (from hereon, I'll use the two words interchangeably), which we use to seed and grow the search space.

What we call the `expand` function in the pseudo code corresponds to the `generatePrograms` function in the actual implementation. We define it as such:

```haskell
generatePrograms ::
 [Expr Double] ->
 [Expr Double] ->
 [Expr Double] ->
 [Expr Double]
generatePrograms vars constants [] = vars ++ constants
generatePrograms vars constants ps =
    let
 existingPrograms = ps ++ vars ++ constants
     in
 existingPrograms
 ++ [ transform p
 | p <- ps ++ vars
 , transform <-
 [ sqrt
 , abs
 , log . (+ Lit 1)
 , exp
 , sin
 , cos
 , relu
 , signum
 ]
 ]
 ++ [ pow i p
 | p <- existingPrograms
 , i <- [2 .. 6]
 ]
 ++ [ p + q
 | (i, p) <- zip [0 ..] existingPrograms
 , (j, q) <- zip [0 ..] existingPrograms
 , i >= j
 ]
 ++ [ p - q
 | (i, p) <- zip [0 ..] existingPrograms
 , (j, q) <- zip [0 ..] existingPrograms
 , i /= j
 ]
 ++ [ p * q
 | (i, p) <- zip [0 ..] existingPrograms
 , (j, q) <- zip [0 ..] existingPrograms
 , i >= j
 ]
 ++ [ p / q
 | p <- existingPrograms
 , q <- existingPrograms
 , p /= q
 ]
```

At each iteration (except the first), we take the list of existing formulas and compose them with simple programs. For example, when we invoke `generatePrograms [] [] [col "x"]` we get `sqrt(x), abs(x),..., pow 0 x, pow 2 x,...,x + x, x * x`.

We've expanded our initial "formula" into a set of prospective formulas. We already notice some duplicates here. `pow 2 x == x * x`. To save ourselves the trouble of an expansion branch, we should keep only one of these.

## Deduplication
Mathematics has many tools for simplifying complex structures into canonical forms. For example, `1/3`, `2/6`, and `4/12` can effectively be treated as `1/3`. Until now, we have relied on observational equality to deduplicate expressions. Such a deduplication would quickly catch a fair number of these deduplications, but it's still a relatively expensive notion of equality - especially as the number of formulas grows. We need a way to leverage our regular tools for mathematical equivalence so we don't always have to evaluate our prospective formulas on the dataframe.

Luckily, this is a solved problem. Symbolic regression implementations typically use equality graphs (e-graphs) to rewrite expressions in their simplest form. E-graphs are a valuable tool in compilers as well, since they can help optimise large expressions into simpler equivalents.

In this example, however, we won't use equality graphs. For pedagogical reasons, we'll implement our own custom rewrite rules.

We can do this in two ways:
* Bake reduction rules into the expressions of the DSL
* Normalise whole expressions into their canonical forms post-hoc.

```haskell

(+) :: Expr a -> Expr a -> Expr a
(+) (Lit x) (Lit y) = Lit (x + y)
(+) e1 e2
    -- Simple rule that says if we're adding x to itself then rewrite the expression as x * 2
 | e1 == e2 = UnaryOp ("mult " <> (T.pack . show) (Lit @a 2)) (* 2) e1
 | otherwise = BinaryOp "add" (+) e1 e2


normalize :: (Eq a, Ord a) => Expr a -> Expr a
normalize expr = case expr of
 Col name -> Col name
 Lit val -> Lit val
 If cond th el -> If (normalize cond) (normalize th) (normalize el)
 UnaryOp name f e -> UnaryOp name f (normalize e)
 BinaryOp name f e1 e2
 | isCommutative name ->
            let n1 = normalize e1
 n2 = normalize e2
             in if compareExpr n1 n2 == GT
                    then BinaryOp name f n2 n1 -- Swap to canonical order
                    else BinaryOp name f n1 n2
 | otherwise -> BinaryOp name f (normalize e1) (normalize e2)
```

Of course, these are straightforward optimisations. An e-graph would implement many smart rewrite rules, but we'll stick with these for now.

The deduplication function can thus be written as:

```haskell
deduplicate ::
    DataFrame ->
 [Expr Double] ->
 [(Expr Double, TypedColumn Double)]
deduplicate df = go S.empty . nubOrd . L.sortBy (\e1 e2 -> compare (eSize e1) (eSize e2)) . map normalize
  where
 go _ [] = []
 go seen (x : xs)
 | hasInvalid = go seen xs
 | S.member res seen = go seen xs
 | otherwise = (x, res) : go (S.insert res seen) xs
      where
 res = either throw id (interpret @Double df x)
        -- We need this since we allow functions that might return infinity or NaN
 hasInvalid = VU.any (\n -> isNaN n || isInfinite n) (toVector res)
```

## Preventing combinatorial explosion
Deduplication isn't enough. We need more creative tools to combat the combinatorial explosion as we explore the search space. Most of the ingenuity of enumerative search techniques lies here.

We have a couple of options here:
* Greedy: take the `n` best expressions each round (otherwise called a beam search)
* Divide-and-conquer: first train a neural network to use as an oracle to guide your search, then run tests against the neural network to attempt to deconstruct the function (this is called the AI Feynman algorithm)
* Evolution: randomly mutate and mix functions to better explore the space of programs (genetic programming)

For simplicity, we'll go with the greedy approach. Most symbolic regression tools use genetic programming, but that approach would be too powerful for our problem.

The greedy approach entails evaluating the expression on some data (which we've already done and returned from the `deduplicate` function), then picking the best `n` functions. We'll use simple Pearson's correlation to rank our formulas.

```haskell
pickTopN ::
    DataFrame ->
    TypedColumn Double ->
    BeamConfig ->
 [(Expr Double, TypedColumn a)] ->
 [Expr Double]
pickTopN _ _ _ [] = []
pickTopN df (TColumn col) cfg ps =
    let
 l = toVector @Double col
 ordered =
            Prelude.take
 (beamLength cfg) -- 4. Take only the first n
                L.sortBy
 ((flip compare) `on` snd) -- 3. Sort functions by descending objective.
 (map
 (\(e, res) -> (e, getLossFunction (lossFunction cfg) l (asDoubleVector res)))  -- 2. Compute objective
 ps -- 1. programs
 )
     in map fst ordered
```

Now we have everything we need to define our beam search:

```haskell
beamSearch ::
    DataFrame ->
 -- | Parameters of the beam search.
    BeamConfig ->
 -- | example labels
    TypedColumn Double ->
 -- | Constants
 [Expr Double] ->
 -- | Conditions
 [Expr Bool] ->
 -- | Programs
 [Expr Double] ->
    Maybe (Expr Double)
beamSearch df cfg labels constants conds programs
 | searchDepth cfg == 0 = case ps of
        [] -> Nothing
 (x : _) -> Just x
 | otherwise =
 beamSearch
 df
 (cfg{searchDepth = searchDepth cfg - 1})
 labels
 constants
 (generatePrograms vars constants ps)
  where
 vars = map col names
 ps = pickTopN df labels cfg $ deduplicate df programs
 names = D.columnNames df
```

## Retraining iris

We create a training split to use for our feature engineering. Doing feature engineering on a training set avoids leaking test-time information.

```
import qualified DataFrame.Hasktorch as DHT
import qualified System.Random as SysRand
import DataFrame.Functions ((.==))

let (training, _) = D.randomSplit (SysRand.mkStdGen 12345) 0.7 df
```

We then create a separate feature for each class by setting its instances to 1 and everything else to 0.

```haskell
labelWith :: Text -> DataFrame -> DataFrame
labelWith species = training |> D.derive "variety" (F.ifThenElse (variety .== species) (F.lit @Double 1) 0)

versicolor = F.synthesizeFeatureExpr "variety" F.defaultBeamConfig (labelWith "Versicolor")
virginica = F.synthesizeFeatureExpr "variety" F.defaultBeamConfig (labelWith "Virginica")
setosa = F.synthesizeFeatureExpr "variety" F.defaultBeamConfig (labelWith "Setosa")
```

Running our synthesizeFeatureExpr function gives us the following features:
* virginica: `(col @Double "petal.length") * (col @Double "petal.width")`
* versicolor: `cos (col @Double "petal.length")`
* setosa: `1 / (col @Double "petal.length")`

Let's look through each of these features one by one.

### Virginica
This feature calculates the approximate petal area. Virginica irises have the largest petals, so this product yields the highest values for this species. For example, if virginica has petals ~6cm long and ~2cm wide, this gives ~12, while smaller-petaled species produce smaller values.

### Versicolor

This feature applies a cosine transformation to petal length. This is the most clever/unusual one. Since:

* Setosa has short petals (~1.0-1.9 cm)
* Versicolor has medium petals (~3.0-5.1 cm)
* Virginica has long petals (~4.5-6.9 cm)

The cosine function's oscillating nature means that versicolor's petal lengths (around 3-5 radians) fall into a specific range of the cosine curve that differs from the other species. This creates separation even though versicolor is "in the middle" on raw measurements.

This suggests that the search was trying to model some non-linearity which could be approximated by `cos`.

### Setosa
This feature creates an inverse relationship with petal length. Since setosa has the shortest petals (~1-2 cm), dividing 1 by small numbers yields large values (e.g., 1/1.5 ≈ 0.66). Longer-petaled species get much smaller values (e.g., 1/6.0 ≈ 0.16).

## Initial results
What happens when we feed these features into the neural network? Well, we get better at the training set:

 variety   | precision |  recall  
-----------|-----------|----------
Setosa     | 1.0       | 1.0      
Versicolor | 0.969697  | 1.0      
Virginica  | 1.0       | 0.9722222

But the test set is a mixed bag.

 variety   | precision |  recall    
-----------|-----------|----------
Setosa     | 1.0       | 1.0      
Versicolor | 1.0       | 0.8333333
Virginica  | 0.8235294 | 1.0


We do better at classifying setosas, but we still have trouble differentiating between versicolor and virginica. The `cos` must have overfit to the non-linear pattern in the training set but failed to generalise. How can we better model the separation between versicolor and virginica?

Let's throw some conditionals into our DSL and see how far that gets us.

## Conditionals are your frenemy
Conditionals model many valuable programs. Armed with conditionals, we would be able to express almost any function. That's a good thing, but it also means we can even express the meaningless functions that arbitrarily overfit our data. Conditionals also mean our search space blows up. Take a simple if-statement that always checks `<=` in the condition. `if p <= q then r else s`. Computing this for all existing programs does O(n<sup>4</sup>) at each tree level. This brings our search to a grinding halt at just the second level, even with a small dataset and feature space.

How can we make conditionals tractable? Well, we can run a beam search over just the conditionals, combining them to form conditions that are strongly related to the target variable. Because we want to capture interesting non-linearities, conditionals are evaluated, converted to 0 or 1, then ranked by mutual information rather than Pearson correlation.

```haskell
generateConditions ::
    TypedColumn Double -> [Expr Bool] -> [Expr Double] -> DataFrame -> [Expr Bool]
generateConditions labels conds ps df =
    let
 newConds =
 [ p .<= q
 | p <- ps
 , q <- ps
 , p /= q
 ]
 ++ [ DataFrame.Functions.not p
 | p <- conds
 ]
 expandedConds =
 conds
 ++ newConds
 ++ [p .&& q | p <- newConds, q <- conds, p /= q]
 ++ [p .|| q | p <- newConds, q <- conds, p /= q]
     in
 pickTopN 10 df labels (deduplicate df expandedConds)
```

Now, our program supports two expansions—one for conditionals and another for programs.

```haskell
generatePrograms _ _ vars' constants [] = vars' ++ constants
generatePrograms includeConds conds vars constants ps =
    let
 existingPrograms = ps ++ vars ++ constants
     in
 ...
 ++ ( if includeConds
                    then
 [ DataFrame.Functions.min p q
 | (i, p) <- zip [0 ..] existingPrograms
 , (j, q) <- zip [0 ..] existingPrograms
 , Prelude.not (isLiteral p && isLiteral q)
 , p /= q
 , i > j
 ]
 ++ [ DataFrame.Functions.max p q
 | (i, p) <- zip [0 ..] existingPrograms
 , (j, q) <- zip [0 ..] existingPrograms
 , Prelude.not (isLiteral p && isLiteral q)
 , p /= q
 , i > j
 ]
 ++ [ ifThenElse cond r s
 | cond <- conds
 , r <- existingPrograms
 , s <- existingPrograms
 , r /= s
 ]
                    else []
 )
```

### Generated features
Running our synthesizeFeatureExpr function with conditionals enabled gives us the following features:
* virginica: `ifThenElse (or (leq (col @Double "petal.length") (col @Double "sepal.width")) (leq (lit (1.8)) (col @Double "petal.width"))) (lit (7.7)) (col @Double "petal.length")`
* versicolor: `ifThenElse (or (leq (lit (5.1)) (col @Double "petal.length")) (leq (lit (1.8)) (col @Double "petal.width"))) (lit 1) (lit 0)`
* setosa: `ifThenElse (leq (col @Double "sepal.width") (col @Double "petal.length")) (lit 0) (lit 1)`


### Virginica
This feature creates a derived feature that:

* Returns a constant value of 7.7 when either the petal is unusually short compared to sepal width, OR the petal width is ≥ 1.8 (a wide petal)
* Otherwise returns the actual petal length
* This likely captures virginica characteristics (larger petals)

### Versicolor
This feature is a binary indicator that flags:

* Flowers with petal length ≥ 5.1 OR petal width ≥ 1.8
* Returns 1 (true) or 0 (false)
* This targets medium-sized flowers typical of versicolor

### Setosa
This feature is a binary indicator that returns 1 when:

* Sepal width <= petal length
* Setosas typically have short petals relative to sepal width, so this would return 1 for setosas and 0 for others.

### Final results
With these 3 features, we get almost everything right on the training set.

 variety   | precision |  recall  
-----------|-----------|----------
Setosa     | 1.0       | 1.0      
Versicolor | 0.9411765 | 1.0      
Virginica  | 1.0       | 0.9444444

This is a clear improvement over both the previous versions.

We did it! We used interpretable, automatically generated features to improve our neural network!

## The symbolic regression landscape

Many battle-tested tools employ the techniques described here. [PySr](https://github.com/MilesCranmer/PySR) is the most mature in the Julia/Python ecosystem. Haskell has [SRTree](github.com/folivetti/srtree#readme) (which we plan to use eventually in the dataframe library).

Wanna learn more about bottom-up synthesis? MIT offers an introduction to program synthesis, with a problem set that roughly covers what we've done in these last two posts. In fact, I encourage you to audit the class to keep abreast with what's happening in the neuro-symbolic world.

## What's next?
Conditionals were a powerful tool, but posed a big challenge for us. How do we use them without blowing up the search space? What if we could tell the search the shape of the final program (that it's a conditional with 2 or 3 terms, and it returns 1 or 0)? We would only search for the hole we need to fill, not worry about others searching everything else.

Top-down enumerative search will be the subject of our next post in the series.
