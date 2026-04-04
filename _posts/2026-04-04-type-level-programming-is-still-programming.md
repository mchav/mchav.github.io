---
layout: post
title: "Type-level programming is still programming"
---

I was showing a friend my typed dataframe API. The whole pitch was: look, you derive a schema from your data, and then the compiler catches column name typos, type mismatches, all the stuff that would otherwise blow up at runtime. I had a nice demo ready using the [Kaggle credit card fraud dataset](https://www.kaggle.com/datasets/mlg-ulb/creditcardfraud) (about 284,000 rows, 31 columns).

I loaded it up in GHCi and ran the untyped version first:

```haskell
dataframe> df <- D.readCsv "./data/creditcard.csv"
dataframe> :set +s
dataframe> :declareColumns df
time :: Expr Double
v1 :: Expr Double
v2 :: Expr Double
...
_class_ :: Expr Int
dataframe> df |> D.groupBy [F.name _class_]
         |> D.aggregate ["rand" .= F.mean v1 - F.mean v2]
         |> D.select ["rand"]
---------------------
        rand
---------------------
       Double
---------------------
-8.395726543461365
1.4528594901372989e-2

(0.92 secs, 4,287,695,984 bytes)
```

But suppose you had a runtime error in the last part of the pipeline?

```haskell
dataframe> df |> D.groupBy [F.name _class_]
         |> D.aggregate ["rand" .= F.mean v1 - F.mean v2]
         |> D.select ["ran"]

*** Exception: 

[ERROR] Column not found: ran for operation select
	Did you mean rand?
```

Now you've waiting the duration of the entire execution of the pipeline (allocations and all) to discover that there is a misnamed column. That's pretty wasteful.

So I ported the example to the typed dataframe and API and ran it. First the misnamed version:

The example did show that when you misname the column it fails at compile time.

```haskell
dataframe> :set -XDataKinds
dataframe> import qualified DataFrame.Typed as DT
dataframe> _ = (); DT.deriveSchema "CreditCard" df. -- Template Haskell trick for GHCi
dataframe> tdf = either (error . show) id (DataFrame.Typed.freezeWithError @CreditCard df)
dataframe> tdf |> DT.groupBy @'["Class"]
               |> DT.aggregate (DT.agg @"rand" (DT.mean (DT.col @"V1") - DT.mean (DT.col @"V2")) DT.aggNil)
               |> DT.select @'["ran"]
<interactive>:39:127: error: [GHC-64725]
    • Column 'ran' not found in schema
    • In the second argument of ‘(|>)’, namely ‘DT.select @'["ran"]’
      In the expression:
        tdf |> DT.groupBy @'["Class"]
          |>
            DT.aggregate
              (agg
                 @"rand" (DT.mean (DT.col @"V1") - DT.mean (DT.col @"V2")) aggNil)
          |> DT.select @'["ran"]
```

And then the happy path:

```haskell
dataframe> tdf |> DT.groupBy @'["Class"]
               |> DT.aggregate (DT.agg @"rand" (DT.mean (DT.col @"V1") - DT.mean (DT.col @"V2")) DT.aggNil)
               |> DT.select @'["rand"]
```

Nothing. The cursor just sat there. I waited a minute. Two minutes. Still nothing. My friend was polite about it and started conjecturing about what could be wrong. But the happy path just didn't finish and eventually OOM'd.

## Chasing ghosts

I spent a while convinced the problem was runtime. Both APIs generate the same expression tree under the hood. Both call the same compiled aggregation function. The typed wrappers are just newtypes. So I went looking for specialization failures, thinking GHCi wasn't generating efficient code for the polymorphic hot loops.

I added `SPECIALIZE` pragmas to `foldLinearGroups`, `mapColumn`, `zipWithColumns`, and every aggregation function in `Functions.hs`. I made the typed combinators delegate to the untyped compiled versions instead of defining their own closures. I specialized for `Double`, `Float`, `Int`, `Int8`, `Int16`, `Int32`, `Int64`. The whole works.

No change. Still hung.

At this point I wrote both pipelines into a compiled executable with `-O2` and profiling enabled. Both finished in 13 milliseconds. Identical. The runtime code was not the problem. Whatever was happening was specific to GHCi.

Then I did the thing I should have done from the start. I put `Debug.Trace` calls at every stage of the pipeline:

```haskell
aggregate tagg (TGD gdf) =
    trace "[typed.aggregate] START" $
    unsafeFreeze (DA.aggregate (taggToNamedExprs tagg) gdf)
```

```haskell
aggregate aggs gdf@(Grouped df ...) =
    trace "[aggregate] START" $
    ...
```

```haskell
eval ctx expr@(Binary ...) =
    trace "[eval] Binary" $
    ...
```

None of them fired. The program never reached runtime.

I tried a few more things after that. I thought maybe it was laziness, that evaluation was being deferred and the traces weren't forced. I added bang patterns, `seq` calls, explicit `evaluate` in IO. Nothing changed. The traces weren't printing because the code wasn't running. Period.

That's when I understood: the hang was happening during GHCi's type-checking of the expression at the prompt.

## The confirmation

To be sure, I ran the same typed pipeline against the housing dataset. 10 columns instead of 31.

```
[typed.aggregate] START
[typed.aggregate] exprs: [("rand",(sub (mean (col @Double "median_income"))
                                       (mean (col @Double "housing_median_age"))))]
[aggregate] START
[eval] Binary
[eval] MergeAgg FAST PATH col="median_income"
[eval] MergeAgg FAST PATH col="housing_median_age"
[eval] Binary: liftValue2
[aggregate] expr DONE
(0.55 secs, 410,398,240 bytes)
```

Everything worked. All the traces fired. Fast path, correct result. So the runtime code was fine. The problem scaled with the number of columns in the schema. 10 columns: instant. 31 columns: heat death of the universe.

## The actual bug

The typed API uses type families to compute result schemas. When you call `groupBy @'["Class"]` on a 31-column schema, the type checker needs to figure out which columns are grouping keys. That computation was done by a type family called `GroupKeyColumns` which itself uses a type-level `If` statement to check for column existence:

```haskell
type family GroupKeyColumns (keys :: [Symbol]) (cols :: [Type]) :: [Type] where
    GroupKeyColumns keys '[] = '[]
    GroupKeyColumns keys (Column n a ': rest) =
        If
            (IsElem n keys)
            (Column n a ': GroupKeyColumns keys rest)
            (GroupKeyColumns keys rest)
```

It's easy to think of type-level programming as some kind of compiler magic that either works or doesn't. But type families are a sort of programming language that must be reasoned about with the same tools we use to reason about regular programs. They take arguments, do recursion, and return results.

So let's trace through what `GroupKeyColumns '["Class"]` does on a 3-column schema, step by step. This is the `If` version:

```haskell
type family If (cond :: Bool) (t :: k) (f :: k) :: k where
    If 'True  t f = t
    If 'False t f = f
```

```
GroupKeyColumns '["Class"] '[Column "V1" Double, Column "V2" Double, Column "Class" Int]

-- Step 1: n = "V1". IsElem "V1" '["Class"] = 'False.
-- GHC needs to evaluate If 'False <then> <else>.
-- But first it normalizes both arguments:
--   then = Column "V1" Double ': GroupKeyColumns '["Class"] '[Column "V2" Double, Column "Class" Int]
--   else = GroupKeyColumns '["Class"] '[Column "V2" Double, Column "Class" Int]
-- Both branches trigger the recursion. Two copies of the same work.

-- Step 2 (from the "then" branch): n = "V2". Same story. Two more copies.
-- Step 2 (from the "else" branch): n = "V2". Same story. Two more copies.
-- We now have 4 pending evaluations of GroupKeyColumns on '[Column "Class" Int].

-- Step 3: each of those 4 evaluates n = "Class". IsElem = 'True.
-- Each produces two more recursive calls on '[].
-- 8 evaluations of GroupKeyColumns '["Class"] '[] = '[].
```

Three columns, eight leaf evaluations. The tree of work doubles at every level. For n columns, GHC does 2^n reductions.

With 10 columns (the housing dataset I tested against), that's 1,024 reductions. Imperceptible. With 31 columns (the Kaggle credit card dataset I demoed with), that's 2,147,483,648 reductions. That's why it hung.

The same analysis applies to any recursive type family that uses `If` with recursive calls in both branches. It's the type-level equivalent of writing a naive Fibonacci function without memoization. The recursion tree fans out because GHC doesn't share work between the two `If` arguments.

## The fix

I replaced `If` with a helper type family that pattern-matches directly on the boolean:

```haskell
type family GroupKeyColumns (keys :: [Symbol]) (cols :: [Type]) :: [Type] where
    GroupKeyColumns keys '[] = '[]
    GroupKeyColumns keys (Column n a ': rest) =
        GroupKeyColumnsHelper (IsElem n keys) n a keys rest

type family GroupKeyColumnsHelper (found :: Bool) (n :: Symbol) (a :: Type)
    (keys :: [Symbol]) (rest :: [Type]) :: [Type] where
    GroupKeyColumnsHelper 'True  n a keys rest =
        Column n a ': GroupKeyColumns keys rest
    GroupKeyColumnsHelper 'False n a keys rest =
        GroupKeyColumns keys rest
```

Now only the matching branch is ever constructed. The recursion is linear.

After the change the typed and untyped version run at roughly the same speed.

## Why the helper version works

Let's trace the helper version on the same 3-column schema:

```
GroupKeyColumns '["Class"] '[Column "V1" Double, Column "V2" Double, Column "Class" Int]

-- Step 1: n = "V1". IsElem "V1" '["Class"] = 'False.
-- GHC evaluates: GroupKeyColumnsHelper 'False "V1" Double '["Class"] '[Column "V2" Double, Column "Class" Int]
-- Matches the 'False equation. Result: GroupKeyColumns '["Class"] '[Column "V2" Double, Column "Class" Int]
-- Only one recursive call. The 'True branch was never constructed.

-- Step 2: n = "V2". IsElem "V2" '["Class"] = 'False.
-- Same thing. One recursive call.

-- Step 3: n = "Class". IsElem "Class" '["Class"] = 'True.
-- Matches the 'True equation. Result: Column "Class" Int ': GroupKeyColumns '["Class"] '[]
-- One recursive call.

-- Step 4: GroupKeyColumns '["Class"] '[] = '[]
```

Four steps total. Linear. Each step does constant work because only the matching equation is ever expanded. The other equation doesn't exist in the reduction trace at all.

This is a known footgun in type-level Haskell. But `If` is the natural thing to reach for when you want a conditional, and it works fine on small inputs. I tested everything against the 10-column housing dataset and it was fast. I didn't think to test against 31 columns until I was demoing it live.

## The rule of thumb

If you have a recursive type family and you're using `If` to select between branches, you have an exponential blowup waiting for a wide enough input. My fix was to introduce a helper type family with two equations and let GHC's pattern matching do the branching instead of `If`.

```haskell
-- Exponential: both branches are always reduced
type family Foo (xs :: [k]) :: [k] where
    Foo '[] = '[]
    Foo (x ': xs) = If (SomePredicate x) (x ': Foo xs) (Foo xs)

-- Linear: only the matching branch is constructed
type family Foo (xs :: [k]) :: [k] where
    Foo '[] = '[]
    Foo (x ': xs) = FooHelper (SomePredicate x) x xs

type family FooHelper (b :: Bool) (x :: k) (xs :: [k]) :: [k] where
    FooHelper 'True  x xs = x ': Foo xs
    FooHelper 'False x xs = Foo xs
```

It's more verbose but it's O(n) instead of O(2^n). For 10 columns you won't notice. For 31 columns you'll be sitting there wondering why your program stopped responding while your friend (who is convinced that Haskell is an impractical academic fascination) watches.
