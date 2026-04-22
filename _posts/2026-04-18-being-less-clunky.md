---
layout: post
title: "Pandas feels clunky coming from R. What about Haskell?"
---

Some years ago I came across an issue in the [Frames repo](https://github.com/acowley/Frames/issues/185) that mentioned a blog post titled "[Why pandas feels clunky when coming from R.](https://www.sumsar.net/blog/pandas-feels-clunky-when-coming-from-r/)" The article showed a side-by-side of simple data exploration in R and compared the code to Pandas. At the time, the author concluded that Pandas was "clunkier" than R. The author operationalises the definition of clunkiness but I think it's really more of a you-know-it-when-you-see-it thing. You can feel if an API is making you drift further away from your task and making you think more about the tool and its idiosyncracies.

So let's give the example a spin and see if Haskell feels clunky compared to R.

NB: Polars isn't clunky and you should probably use that instead of Pandas but we compare to Pandas here because the original author does.

## Reading the data

According to the article, we've been given a table of purchases from various countries, where the customer could have received a discount. We want to answer some questions about the data but we do so incrementally. The blog post showcases the ease of writing and changing business logic with dataframes.

So we start by looking at the total amount gathered from sales.

```R
library(tidyverse)

purchases <- read_csv("purchases.csv")
purchases |> head()
```

With dataframe (in a cabal script) things look roughly similar give or take a few lines of code.

```haskell
#!/usr/bin/env cabal
{- cabal:
  build-depends: base >= 4, dataframe
-}
module Main where

import qualified DataFrame as D

main :: IO ()
main = do
    df <- D.readCsv "./data/purchases.csv"
    print (D.take 10 df)
```

Pandas looks pretty similar:

```python 
import pandas as pd

purchases = pd.read_csv("purchases.csv")
purchases.head()
```

And finally in Frames we have:

```haskell

{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Frames
import qualified Data.Foldable as F

tableTypes "Purchases" "purchases.csv"

loadBenchmarks :: IO (Frame Purchases)
loadBenchmarks = inCoreAoS (readTable "purchases.csv")

main :: IO ()
main = do
  ms <- loadBenchmarks
  print (take 10 (F.toList ms))
```

Everyone is doing well so far! We can read the CSV and show the first few rows.

## Summing by amount

Now, let's see how much we made in total sales.

```R
purchases$amount |> sum()
```

R expresses the logic pretty directly. As does pandas:

```python
purchases["amount"].sum()
```

For dataframe we would add the following expression:

```haskell
print (D.sum (F.col @Int "amount") df)
```

That's some clunk creeping in. We not only had to get the column name right, we had to remember its type. We can pull out some machinery to make it less clunky.

```haskell
#!/usr/bin/env cabal
{- cabal:
  build-depends: base >= 4, dataframe
-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified DataFrame as D
import qualified DataFrame.Functions as F

$(F.declareColumnsFromCsvFile "./data/purchases.csv")

main :: IO ()
main = do
    df <- D.readCsv "./data/purchases.csv"
    print (D.take 10 df)
    print (D.sum amount df)
```

So we traded off fragility for complexity. For this small example it doesn't buy us much but in the next part it will.

Frames already had `TemplateHaskell` enabled so we just have to add:

```haskell
import Lens.Micro.Extras
import Control.Foldl (fold, sum)
import Prelude hiding (sum)

-- All the old code.
    print $ fold sum (view amount <$> ms)
```

This is still pretty readable if you understand FP. In fact it's a great declarative programming model that shows you what a sum is. For some problems this is great. But as we tack on complexity this will come back to bite us.

## Grouping by country

So apparently they wanted the amounts by country. Now you have to go back and change the code. R still looks stunning:

```R
purchases |>
  group_by(country) |>
  summarize(total = sum(amount))
```

This has a great SQL-ish API. Python is similar but starts to be a little clunky since it requires you to think about indices:

```python
(purchases
  .groupby("country")
  .agg(total=("amount", "sum"))
  .reset_index()
)
```

dataframe's TemplateHaskell setup now pays off.

```haskell
print $  purchases
      |> D.groupBy [F.name country]
      |> D.aggregate ["total" .= F.sum amount]
```

`groupBy` takes a list of strings. We can recover the column's string name with `F.name`. We also introduce the expression DSL that we use to calculate the sum (instead of the top-level `D.sum` function). There's a little complexity but now we get a SQL-ish API that doesn't leak that much implementation detail.

Frames starts to bring out the artillery.

```haskell
{-# OPTIONS_GHC -Wall            #-}

{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Main (main) where

import           Frames
import qualified Data.Foldable as F
import qualified Data.List as L
import           Lens.Micro.Extras

import           Control.Foldl (fold, sum)
import qualified Control.Foldl as Foldl
import           Prelude hiding (sum)

import qualified Frames.MapReduce as FMR
import qualified Frames.Folds as FF

tableTypes "Purchases" "purchases.csv"

loadBenchmarks :: IO (Frame Purchases)
loadBenchmarks = inCoreAoS (readTable "purchases.csv")

unpack :: FMR.Unpack Purchases Purchases
unpack = FMR.unpackFilterOnField @Country (const True)

assign :: FMR.Assign (Record '[Country]) Purchases (Record '[Amount, Discount])
assign = FMR.splitOnKeys @'[Country]

reduce :: FMR.Reduce (Record '[Country])
                     (Record '[Amount, Discount])
                     (Frame Purchases)
reduce = FMR.foldAndAddKey $ (FF.foldAllConstrained @Num @'[Amount, Discount]) sum

mrFold :: FMR.Fold Purchases (Frame Purchases)
mrFold = FMR.concatFold $ FMR.mapReduceFold unpack assign reduce

rhead :: Show a => Frame a -> IO ()
rhead  = \ms -> mapM_ print (((take 6) . F.toList) ms)

main :: IO ()
main = do
  ms <- loadBenchmarks
  rhead ms
  print $ fold sum (view amount <$> ms)
  let result = FMR.fold mrFold ms
  putStrLn $ (L.intercalate "\n" $ fmap show $ fold Foldl.list result)
```

At the points at which computation happens it's pretty declarative but simple logic is now overwhelmed by boilerplate. Refactoring to answer a pretty easy follow up question cost us a lot in code but we get compile-time safety. At this stage, how much complexity you're willing to stomach is a matter of person taste and familiarity.

## Deducting the discount
Frames is now clunky so we'll stop reporting on it. We now want to get the sum of not just the amount but the amount minus the discount. Let's see how the other implementations handle the discount.

```R
# R
purchases |> 
  group_by(country) |> 
  summarize(total = sum(amount - discount))
```

The R change is pretty easy. Pandas gets a little weird:

```python
(purchases
  .groupby("country")
  .apply(lambda df: (df["amount"] - df["discount"]).sum())
  .reset_index()
  .rename(columns={0: "total"})
)
```

We now start leaking a few more implementation details. Column renaming and the difference between simple sums and expression sums.

Haskell still tracks R:

```haskell
print $  purchases
      |> D.groupBy [F.name country]
      |> D.aggregate ["total" .= F.sum (amount - discount)]
```

## Removing outliers

Let's now remove any rows whose amount is greater than the global median * 10.

```R
purchases |>
  filter(amount <= median(amount) * 10) |>
  group_by(country) |> 
  summarize(total = sum(amount - discount))
```

Pandas introduces a `query` function:

```python
(purchases
  .query("amount <= amount.median() * 10")
  .groupby("country")
  .apply(lambda df: (df["amount"] - df["discount"]).sum())
  .reset_index()
  .rename(columns={0: "total"})
)
```

dataframe looks fairly similar too:

```haskell
print $  purchases
      |> D.filterWhere (F.toDouble amount .<= F.median amount * 10)
      |> D.groupBy [F.name country]
      |> D.aggregate ["total" .= F.sum (amount - discount)]
```

`amount` is an Int and median is a double so we pay a small conversion cost but that's okay.

Things look good on all fronts. The refactor wasn't hard.

## Using the median within each country

R just drops the comparison to after the groupBy.

```R
purchases |>
  group_by(country) |>
  filter(amount <= median(amount) * 10) |>
  summarize(total = sum(amount - discount))
```

Pandas struggles to handle this logic succintly:

```python
(purchases
  .groupby("country")
  .apply(lambda df: df[df["amount"] <= df["amount"].median() * 10])
  .reset_index(drop=True)
  .groupby("country")
  .apply(lambda df: (df["amount"] - df["discount"]).sum())
  .reset_index()
  .rename(columns={0: "total"})
)
```

The diff now includes a lot of extra noise and takes you away fro the core change.

Haskell keeps the diff minimal:

```haskell
print $  purchases
      |> D.filterWhere (F.toDouble amount .<= F.over [F.name country] (F.median amount * 10))
      |> D.groupBy [F.name country]
      |> D.aggregate ["total" .= F.sum (amount - discount)]
```

We use the `over` expression which does an intra-expression grouping. All the setup we did is now paying off. Our diffs are small and focused. Arguably, the R diff (changing the order of the groupby/filter) makes things a little more confusing because your reasoning is non-local. You need to read the previous line to understand what 

## What about type safety?

We lost some type safety by making the API less clunky. For exmaple, this gives us a runtime failure.

```haskell
print $  purchases
      |> D.exclude [F.name amount]
      |> D.filterWhere (F.toDouble amount .<= F.over [F.name country] (F.median amount * 10))
      |> D.groupBy [F.name country]
      |> D.aggregate ["total" .= F.sum (amount - discount)]
```

Filter will try and refer to an amount that doesn't exist then throw an exception. Frames would never let us do something like this. Can we get type safety back?

Yes we can.

We just have to swap out some imports and do a little more annotating.

```haskell
#!/usr/bin/env cabal
{- cabal:
  build-depends: base >= 4, dataframe, text
-}
{-# LANGUAGE DataKinds #-}

module Main where

import qualified DataFrame as D
import qualified DataFrame.Typed as DT

import Data.Text (Text)

main :: IO ()
main = do
    df <- D.readCsv "./data/purchases.csv"
    let tdf = DT.unsafeFreeze @'[ DT.Column "country" Text
                                , DT.Column "amount" Int
                                , DT.Column "discount" Int] df

    print $  tdf
          |> DT.filterWhere (DT.toDouble (DT.col @"amount") .<= DT.over @'["country"] (DT.median (DT.col @"amount" * 10))
          |> DT.groupBy @'["country"]
          |> DT.aggregate (DT.agg @"total" (DT.sum (DT.col @"amount" - DT.col @"discount") DT.aggNil))
```

We could use template Haskell to generate the type we freeze the dataframe into.

```haskell
-- this is the equivalent of writing:
-- type Purchases = '[ DT.Column "country" Text
--                   , DT.Column "amount" Int
--                   , DT.Column "discount" Int]
$(DT.deriveSchemaFromCsvFile "Purchases" "./data/purchases.csv")


-- Skip other code
    let tdf = DT.unsafeFreeze @Purchases df
```

We're only a little clunkier than before. We could in principle add more TemplateHaskell to get the column references similar to what we did to remove `F.col` before. Leaving us with:

```haskell
-- Some code above.
    print $  tdf
          |> DT.filterWhere (DT.toDouble amount .<= DT.over @'["country"] (DT.median (amount * 10)))
          |> DT.groupBy @'["country"]
          |> DT.aggregate (DT.agg @"total" (DT.sum (amount - discount) DT.aggNil))
```

## Up shot
Safety and usability are usually at tension. But you can use a compiled language (in this case Haskell) as ergonomically as you would a dynamic language if you design the right abstractions and keep them centered on user tasks.
