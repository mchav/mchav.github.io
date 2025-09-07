---
layout: post
title: Rewriting dataframes for MicroHs
---

## My fondness for alternative Haskells

It's often said that the best way to learn a programming language (or programming in general) is to make things that you actively use. After I first learnt Haskell, I thought it natural to try and make something I'll use in my day-to-day life. An idea of what to make almost immediately sprang up in my head. Because I spent most of my college years travelling with the university's debate team, I thought it would be cool to make a simple countdown timer with large bold text and changing colours that I'd use to time my speeches. 

At that time (around 2015), it was incredibly difficult to write Haskell GUI applications, let alone mobile applications. The few examples that existed online were scant, "hello world" applications that didn't seem like they could do much.

I stumbled across [Frege](https://github.com/Frege/frege) (a dialect of Haskell for the JVM) and was warmly welcomed by the language's small community. With the help of the two primary maintainers, Ingo and Dierk, I wrote my first large project in a functional programming language: [froid - a library for using the Frege programming language in Android development](https://github.com/mchav/froid).

I had many ideas of the directions I wanted to take the library - most of which were all the rage at the time: STM, functional reactive programming, etc. But using anything in the Haskell ecosystem was difficult because most libraries were extremely GHC-specific.

Eventually, Frege's community fizzled out as the maintainers got busy and the general mid-2010s fervour around functional programming faded. All the other Haskell implementations lost traction slowly after (GHCJS and Eta).

But I still wish that there were a wider Haskell ecosystem that focused on use cases that GHC can't address. I still wish that people would write code that other Haskells could co-opt with little to no friction.

The release of [MicroHs](https://github.com/augustss/MicroHs/blob/master/doc/hs2024.pdf) was a strong reminder of that time. A new Haskell with much smaller binaries that came at a time when [people were showing concern about the size of the GHC ecosystem](https://discourse.haskell.org/t/haskell-tools-could-loose-some-weight/11159).

To be a good steward of the Haskell ecosystem at large, I figured it would be good to think through how my current personal effort, dataframe, could be decoupled from GHC.

## Our north star

### What is a dataframe?

A dataframe is a heterogeneous collection of columns and their labels. It is, at its core, a very loosely typed thing since types in this domain are evolving "guesses" of what the data should look like. This is very hand-wavy, but there is a more serious treatment of the topic in the [dataframe design document](https://docs.google.com/document/d/1oIX_OWzoTXFeN9q7ZRuDuP1mQaRSvu4RhT2Tnj8uV2c/edit?pli=1&tab=t.0).

The goal for this blog post will be to make an API that looks roughly like this (taken from the actual [`dataframe`](https://docs.google.com/document/d/1oIX_OWzoTXFeN9q7ZRuDuP1mQaRSvu4RhT2Tnj8uV2c/edit?pli=1&tab=t.0) implementation):

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified DataFrame as D
import qualified DataFrame.Functions as F

import DataFrame ((|>))

main :: IO ()
main = do
    let highs = [(24 :: Int), 20, 22, 23, 25, 26, 26]
    let lows = [(14 :: Int), 13, 13, 13, 14, 15, 15]
    let df = D.fromNamedColumns [
        ("Day", D.fromList (take (length highs) (cycle ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]))),
        ("High Temperature (Celcius)", D.fromList highs),
        ("Low Temperature (Celcius)", D.fromList lows)]

    -- Typed column references
    let high = F.col "High Temperature (Celcius)" :: D.Expr Int
    let low = F.col "Low Temperature (Celcius)" :: D.Expr Int

    -- Some expressions.
    let hotDays = df
        & D.filterWhere (high `F.>=` D.lit (25 :: Int))
        & D.derive "total" (high + low) 
        & D.derive "year" (F.lit (2025 :: Int)) 
    print hotDays
```

The output should look roughly like this:

|        row | Day        | High Temp… | Low Tempe… | total      | year       |
| ---------: | :--------- | ---------: | ---------: | ---------: | ---------: |
|          4 | Friday     |         25 |         14 |         39 |       2025 |
|          5 | Saturday   |         26 |         15 |         41 |       2025 |
|          6 | Sunday     |         26 |         15 |         41 |       2025 |

### Our constraints
This will be a harder challenge than just using MicroHs (which already has some extensions enabled by default). What if we wanted to implement this without any extensions (well, except for one, which we'll see later)? The current dataframe implementation makes liberal use of type families, GADTs and reflection to make the codebase easier to navigate. The features aren't strictly necessary, however. If we make some sacrifices, we can create a base-only implementation of dataframes.

## Implementation
### The core data types

As we said above, a dataframe is a heterogeneous collection of columns and their labels. We can create a data type directly from this definition:

```haskell
data DataFrame = DataFrame { columns :: [(String, Column)]}
```

That's it. Anything tacked onto that definition is a refinement for performance or usability. For this post we'll stick with this simple definition.

But what is a column?

In the main library, we allow columns to be of any type with a Show, Ord, and Eq instance. Since such flexibility would, in the future, compel us to use more complicated tactics to create functions, we'll start with a simple formulation of a column. Our columns will be one of `Int`, `String`, or `Double`. Again, the data type comes straight from the definition (with a small addition):

```haskell
data Column = CInt [(Int,Int)]
            | CDouble [(Int, Double)]
            | CString [(Int, String)]
```

List are not a great choice for our column. Ideally, we would use vectors, arrays, or any other contiguous-memory data structure, but none of the options currently compile with microhs, so lists with indexes will do for now.

Now we can make a constructor for our dataframe:

```haskell
fromNamedColumns :: [(String, Column)] -> DataFrame
fromNamedColumns = DataFrame
```

For columns, we use a type class which allows us to have a single function dispatched by each type in our universe.

```haskell
class ToColumn a where
  toColumn :: [(Int, a)] -> Column

instance ToColumn Int    where toColumn = CInt
instance ToColumn Double where toColumn = CDouble
instance ToColumn [Char] where toColumn = CString
instance ToColumn Bool where toColumn = CBool
```

We have to include the `FlexibleInstances` extension here, otherwise the compiler complains that we can only use simple types in instance declarations.

Our constructor for columns can therefore be written as follows:

```haskell
fromList :: ToColumn a => [a] -> Column
fromList xs = toColumn (zip [0..] xs)
```

We now have the first part of our program covered:

```haskell
main :: IO ()
main = do
  let highs = [(24 :: Int), 20, 22, 23, 25, 26, 26]
  let lows = [(14 :: Int), 13, 13, 13, 14, 15, 15]
  let df = D.fromNamedColumns [
                ("Day", D.fromList (take (length highs) (cycle ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]))),
                ("High Temperature (Celcius)", D.fromList highs),
                ("Low Temperature (Celcius)", D.fromList lows)]
  pure ()
```

To see the dataframe we define a small helper module for pretty printing.

```haskell
module DataFrame.PrettyPrint (
    renderMarkdownTable
) where

import qualified Data.List as L
import           Data.Maybe
import           DataFrame.Core
import           DataFrame.Column

colMap :: Column -> [(Int, String)]
colMap (CInt xs)    = [ (i, show v) | (i, v) <- xs ]
colMap (CDouble xs) = [ (i, show v) | (i, v) <- xs ]
colMap (CString xs) = [ (i, v)      | (i, v) <- xs ]

escapeCell :: String -> String
escapeCell = concatMap (\c -> case c of
  '|' -> "\\|"
  '\n' -> "<br>"
 _    -> [c])

data Align = LeftA | RightA

colAlign :: Column -> Align
colAlign (CString _) = LeftA
colAlign _           = RightA

fit :: Int -> String -> String
fit w s =
  let t = escapeCell s
  in if length t <= w then t else take (max 1 (w-1)) t ++ "…"

pad :: Align -> Int -> String -> String
pad LeftA  w s = s ++ replicate (max 0 (w - length s)) ' '
pad RightA w s = replicate (max 0 (w - length s)) ' ' ++ s

alignMarker :: Align -> Int -> String
alignMarker LeftA  w = ":" ++ replicate (max 0 (w-1)) '-'
alignMarker RightA w = replicate (max 0 (w-1)) '-' ++ ":"

renderMarkdownTable :: Maybe Int -> DataFrame -> String
renderMarkdownTable mW (DataFrame cols) =
  let names   = map fst cols
 maps    = map (colMap . snd) cols
 aligns  = map (colAlign . snd) cols
 allIdx  = L.sort . L.nub $ concatMap (map fst) maps

 autoW =
        let headerParts = "row" : names
 cellLens    = concatMap (map (length . escapeCell) . (map snd)) maps
 headerLens  = map length headerParts
 rowLens     = map (length . show) allIdx
        in maximum (1 : (cellLens ++ headerLens ++ rowLens))

 w = max 3 $ maybe autoW id mW

 fmtHeaderCell a s = pad a w (fit w s)
 fmtCell a s       = pad a w (fit w s)

 header =
        let first = fmtHeaderCell RightA "row"
 rest  = zipWith (\nm _ -> fmtHeaderCell LeftA nm) names aligns
        in "| " ++ L.intercalate " | " (first : rest) ++ " |\n"

 sep =
        let first = alignMarker RightA w
 rest  = map (`alignMarker` w) aligns
        in "| " ++ L.intercalate " | " (first : rest) ++ " |\n"

 row i =
        let first = fmtCell RightA (show i)
 rest  = zipWith3
 (\m a _ -> fmtCell a (fromMaybe "" (lookup i m)))
 maps aligns names
        in "| " ++ L.intercalate " | " (first : rest) ++ " |\n"
  in header ++ sep ++ concatMap row allIdx
```

The `renderMarkdownTable` function takes an optional width argument and truncates all columns to that width.

### Expressions

This is the difficult part. We want to be able to express binary and unary operations for our dataframe.

We have the extra constraint that our operations can only work within our type universe (`Int`, `String`, and `Double`). In GHC we'd typically pull out GADTs for this and define the expression as follows:

```haskell
data Expr a where
 Col :: String -> Expr a  -- | Column references
 Lit :: a -> Expr a       -- | Literals
 UnaryOp :: (ToColumn b) => (b -> a) -> Expr b -> Expr a
 BinaryOp :: (ToColumn c, ToColumn b) => (c -> b -> a) -> Expr c -> Expr b -> Expr a
```

But since we don't have that at our disposal (NB: MicroHs has GADTs, but remember we're making this problem strictly more difficult), we have to enumerate our type universe. Since we only have three types, this is still fine.


```haskell
data Expr a =
 Col String
 | Lit a
 | UnaryIntOp (Int -> a) (Expr Int)
 | UnaryDoubleOp (Double -> a) (Expr Double)
 | UnaryStringOp (String -> a) (Expr String)
 | UnaryBoolOp (Bool -> a) (Expr Bool)

    -- Binary Ops
 | BinaryIntToIntOp (Int -> Int -> a) (Expr Int) (Expr Int)
 | BinaryIntToDoubleOp (Int -> Double -> a) (Expr Int) (Expr Double)
 | BinaryIntToStringOp (Int -> String -> a) (Expr Int) (Expr String)
 | BinaryIntToBoolOp (Int -> Bool -> a) (Expr Int) (Expr Bool)

 | BinaryDoubleToIntOp (Double -> Int -> a) (Expr Double) (Expr Int)
 | BinaryDoubleToDoubleOp (Double -> Double -> a) (Expr Double) (Expr Double)
 | BinaryDoubleToStringOp (Double -> String -> a) (Expr Double) (Expr String)
 | BinaryDoubleToBoolOp (Double -> Bool -> a) (Expr Double) (Expr Bool)
    
 | BinaryStringToIntOp (String -> Int -> a) (Expr String) (Expr Int)
 | BinaryStringToDoubleOp (String -> Double -> a) (Expr String) (Expr Double)
 | BinaryStringToStringOp (String -> String -> a) (Expr String) (Expr String)
 | BinaryStringToBoolOp (String -> Bool -> a) (Expr String) (Expr Bool)

 | BinaryBoolToIntOp (Bool -> Int -> a) (Expr Bool) (Expr Int)
 | BinaryBoolToDoubleOp (Bool -> Double -> a) (Expr Bool) (Expr Double)
 | BinaryBoolToStringOp (Bool -> String -> a) (Expr Bool) (Expr String)
 | BinaryBoolToBoolOp (Bool -> Bool -> a) (Expr Bool) (Expr Bool)
```

To interpret our expressions, we need to create a function that takes in an expression and a dataframe.

The `interpret` function should return a column that is the result of applying our expression to the dataframe. Because our expression tree is verbose, the interpret function will be verbose as well.

```haskell
interpret :: (ToColumn a) => Expr a -> DataFrame -> Column
interpret (Col name) df = case lookup name (columns df) of
 Nothing -> fromList ([] :: [[Char]])
 Just c  -> c
interpret (Lit value) df = let
 ixs = case (columns df) of
 ((_, (CInt xs)): rest)    -> map fst xs
 ((_, (CDouble xs)): rest) -> map fst xs
 ((_, (CString xs)): rest) -> map fst xs
 ((_, (CBool xs)): rest)   -> map fst xs
            [] -> ([] :: [Int])
    in toColumn (zip ixs (replicate (length ixs) value))
interpret (UnaryIntOp f e) df = case interpret e df of
 CInt xs -> toColumn (map (\(i, v) -> (i, f v)) xs)
 _       -> error "Type mismatch"
interpret (UnaryDoubleOp f e) df = case interpret e df of
 CDouble xs -> toColumn (map (\(i, v) -> (i, f v)) xs)
 _       -> error "Type mismatch"
interpret (UnaryStringOp f e) df = case interpret e df of
 CString xs -> toColumn (map (\(i, v) -> (i, f v)) xs)
 _       -> error "Type mismatch"
interpret (UnaryBoolOp f e) df = case interpret e df of
 CBool xs -> toColumn (map (\(i, v) -> (i, f v)) xs)
 _       -> error "Type mismatch"
interpret (BinaryIntToIntOp f l r) df = case (interpret l df, interpret r df) of
    -- Assumes indices are the same.
    -- TODO: We could line these up.
 (CInt xs, CInt ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
 _ -> error "Type mismatch"
interpret (BinaryIntToDoubleOp f l r) df = case (interpret l df, interpret r df) of
 (CInt xs, CDouble ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
 _ -> error "Type mismatch"
interpret (BinaryIntToStringOp f l r) df = case (interpret l df, interpret r df) of
 (CInt xs, CString ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
 _ -> error "Type mismatch"
interpret (BinaryIntToBoolOp f l r) df = case (interpret l df, interpret r df) of
 (CInt xs, CBool ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
 _ -> error "Type mismatch"
-- | Double
interpret (BinaryDoubleToIntOp f l r) df = case (interpret l df, interpret r df) of
 (CDouble xs, CInt ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
 _ -> error "Type mismatch"
interpret (BinaryDoubleToDoubleOp f l r) df = case (interpret l df, interpret r df) of
 (CDouble xs, CDouble ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
 _ -> error "Type mismatch"
interpret (BinaryDoubleToStringOp f l r) df = case (interpret l df, interpret r df) of
 (CDouble xs, CString ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
 _ -> error "Type mismatch"
interpret (BinaryDoubleToBoolOp f l r) df = case (interpret l df, interpret r df) of
 (CDouble xs, CBool ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
 _ -> error "Type mismatch"
-- | String
interpret (BinaryStringToIntOp f l r) df = case (interpret l df, interpret r df) of
 (CString xs, CInt ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
 _ -> error "Type mismatch"
interpret (BinaryStringToDoubleOp f l r) df = case (interpret l df, interpret r df) of
 (CString xs, CDouble ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
 _ -> error "Type mismatch"
interpret (BinaryStringToStringOp f l r) df = case (interpret l df, interpret r df) of
 (CString xs, CString ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
 _ -> error "Type mismatch"
interpret (BinaryStringToBoolOp f l r) df = case (interpret l df, interpret r df) of
 (CString xs, CBool ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
 _ -> error "Type mismatch"
-- | Bool
interpret (BinaryBoolToIntOp f l r) df = case (interpret l df, interpret r df) of
 (CBool xs, CInt ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
 _ -> error "Type mismatch"
interpret (BinaryBoolToDoubleOp f l r) df = case (interpret l df, interpret r df) of
 (CBool xs, CDouble ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
 _ -> error "Type mismatch"
interpret (BinaryBoolToStringOp f l r) df = case (interpret l df, interpret r df) of
 (CBool xs, CString ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
 _ -> error "Type mismatch"
interpret (BinaryBoolToBoolOp f l r) df = case (interpret l df, interpret r df) of
 (CBool xs, CBool ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
 _ -> error "Type mismatch"
```

Functions must also be laboriously defined in this way. We define type classes of operations that dispatch our operations to the right node of our expression tree. We'll start with addition, equality and the greater-than-or-equal-to operation. The approach can easily be extended to other functions.

```haskell
-- | Each function must have a type class that specialises it to the
-- | right type.

class ExprEq a where
    eq :: Expr a -> Expr a -> Expr Bool

instance ExprEq Int where
 eq l r = BinaryIntToIntOp (==) l r

instance ExprEq Double where
 eq l r = BinaryDoubleToDoubleOp (==) l r

instance ExprEq [Char] where
 eq l r = BinaryStringToStringOp (==) l r

instance ExprEq Bool where
 eq l r = BinaryBoolToBoolOp (==) l r


class ExprGeq a where
    geq :: Expr a -> Expr a -> Expr Bool

instance ExprGeq Int where
 geq l r = BinaryIntToIntOp (>=) l r

instance ExprGeq Double where
 geq l r = BinaryDoubleToDoubleOp (>=) l r

instance ExprGeq [Char] where
 geq l r = BinaryStringToStringOp (>=) l r

instance ExprGeq Bool where
 geq l r = BinaryBoolToBoolOp (>=) l r

-- For addition
class ExprAdd a where
    add :: Expr a -> Expr a -> Expr a

instance ExprAdd Int where
 add l r = BinaryIntToIntOp (+) l r

instance ExprAdd Double where
 add l r = BinaryDoubleToDoubleOp (+) l r

instance ExprAdd [Char] where
 add l r = error "Cannot add strings"

instance ExprAdd Bool where
 add l r = error "Cannot add bools"

-- | Num instance
instance (ToColumn a, Num a, ExprAdd a) => Num (Expr a) where
 (+) = add
```

Now we are ready to define our work-horses: `filterWhere` and `derive`. `filterWhere` takes the indices of the column returned by a boolean expression and selects them from all the other columns. `derive` takes an expression, evaluates it to a column, then adds it to the dataframe.

```haskell
filterWhere :: Expr Bool -> DataFrame -> DataFrame
filterWhere expr df = case interpret expr df of
 (CBool xs) -> let
 ixs = map fst (filter snd xs)
        in fromNamedColumns $ map (\(i, v) -> (i, atIndicies ixs v)) (columns df)
 _          -> error "Should not be possible"

derive :: ToColumn a => String -> Expr a -> DataFrame -> DataFrame
derive name expr df = DataFrame ((columns df) ++ [(name, interpret expr df)])


-- | Small helper to select indices
atIndicies :: [Int] -> Column -> Column
atIndicies ixs (CInt xs) = CInt (filter ((`elem` ixs) . fst) xs)
atIndicies ixs (CDouble xs) = CDouble (filter ((`elem` ixs) . fst) xs)
atIndicies ixs (CString xs) = CString (filter ((`elem` ixs) . fst) xs)
```

We can now run the code we initially set out to run.

[The full code for this example is on Github](https://github.com/mchav/dataframe-mhs).

## Comparing MicroHs and GHC
Lennart's rule of thumb is that MicroHs has 100x smaller binaries but is 10x slower.

When the program above is compiled with ghc 9.4.5 this is somewhat true. The binary is 13MB (which is ~100X compared to MicroHs's 200KB). On my computer, it is only about 5 times slower.

On GHC 9.10.2 the binary size difference is less stark (1.3MB vs 200KB), but the speed gap is the same.

### Take away
Sticking to Haskell 2010 pushed me to separate the public API from the implementation details, so the same code runs on MicroHs and GHC. The experiment showed:

- **It’s doable.** A usable DataFrame core—construction, simple expressions, `filterWhere`, `derive`, and Markdown rendering—works fine without GADTs, type families, or reflection. You pay in verbosity, not viability.
- **Portability buys options.** A base-first design means the same front-end API can run on MicroHs for tiny CLIs or embedded contexts and on GHC for speed and ecosystem access.
- **Trade-offs are clear.** MicroHs binaries are ~100× smaller and ~5–10× slower for this workload; for many data-wrangling tasks that’s a great swap, and you can still keep a GHC backend for heavy lifting.
