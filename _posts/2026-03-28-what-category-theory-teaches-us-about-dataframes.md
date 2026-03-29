---
layout: post
title: "What Category Theory Teaches Us About DataFrames"
---

When I initially challenged myself with building a dataframe implementation, I began by looking at what already existed for inspiration. Spark was my initial reference. Then pandas. Then Polars, R's data.table, Julia's DataFrames.jl. Each one had its own take on the API. Early on, my main challenge was trying to figure out which operations I actually needed to implement to call what I had a "dataframe". The surface area felt enormous. Do I need `pivot` and `melt` as separate things? Is `apply` different from `map`? What about `transform`, `agg`, `applymap`, `pipe`? Some of these seemed like the same operation wearing different hats. Others seemed genuinely distinct. I didn't have a good framework for telling them apart.

I decided to do some reading around the thoery of dataframes. Since theory is in the business of trying to find unifying abstractions and dataframes were so ubiquitous I thought I'd find a wealth of material to build from. Sadly, I only found a handful of resource. The most useful was Petersohn et al.'s *Towards Scalable Dataframe Systems*. They had built a system called Modin which was a scalable/distributed drop-in replacement for pandas. To do that they needed to understand the actual structure underneath the API. So they studied it extensively. They analyzed 1 million Jupyter notebooks, cataloged how people use pandas, and proposed something I hadn't seen before: a *dataframe algebra*. A formal set of about 15 operators that can express what all 200+ pandas operations do.

That was the paper that gave me a foundation. Instead of copying APIs, I could implement the algebra and build the user-facing operations as compositions of those primitives.

I kept wondering, however, whether there was a level below that. Whether there was a smaller set of truly primitive operations that the 15 are built from. If I could find those, I'd have a real foundation: operations small enough to be obviously correct, expressive enough to build everything else.

But 15 operators is still a lot of surface area to get right. I kept wondering whether there was a level below that. A smaller set of truly primitive operations that the 15 are built from. If I could find those, I'd have a real foundation: operations small enough to be obviously correct, expressive enough to build everything else.

## Petersohn's dataframe algebra

It's worth spending some time on what Petersohn et al. actually did, because it frames everything that follows.

They started by defining what a dataframe *is*. Surprisingly, nobody had done this formally before. Their Definition 4.1 says a dataframe is a tuple *(A, R, C, D)*: an array of data *A*, row labels *R*, column labels *C*, and a vector of column domains *D*. This is more precise than "a table" because it captures things that make dataframes different from relational tables. Rows and columns are both ordered, both labeled, and treated symmetrically. You can transpose a dataframe. You can promote data values into column labels. These aren't things you can do with a SQL table.

Then they identified the operators. Here's their Table 1, condensed:

| Operator | Origin | What it does |
|---|---|---|
| SELECTION | Relational | Eliminate rows |
| PROJECTION | Relational | Eliminate columns |
| UNION | Relational | Combine two dataframes vertically |
| DIFFERENCE | Relational | Rows in one but not the other |
| CROSS PRODUCT / JOIN | Relational | Combine two dataframes by key |
| DROP DUPLICATES | Relational | Remove duplicate rows |
| GROUPBY | Relational | Group rows by column values |
| SORT | Relational | Reorder rows |
| RENAME | Relational | Rename columns |
| WINDOW | SQL | Sliding-window functions |
| TRANSPOSE | Dataframe | Swap rows and columns |
| MAP | Dataframe | Apply a function to every row |
| TOLABELS | Dataframe | Promote data to column/row labels |
| FROMLABELS | Dataframe | Demote labels back to data |

The "Origin" column matters. The first nine operators come from relational algebra and have direct analogs in SQL. WINDOW comes from SQL extensions. The last four (TRANSPOSE, MAP, TOLABELS, FROMLABELS) are unique to dataframes. They exist because dataframes treat rows and columns symmetrically and allow data to move between values and metadata. Relational databases can't do that.

Petersohn showed that over 85% of the pandas API can be rewritten as compositions of these operators. Operations like `fillna`, `isnull`, `str.upper`, and `cummax` are all special cases of MAP. Operations like `sort_values`, `set_index`, `reset_index`, `merge`, `groupby`, and `pivot` all map one-to-one onto operators in the algebra. That's a huge compression: 200+ ad hoc methods become 15 composable primitives.

But I kept looking at the relational operators in that table (PROJECTION, RENAME, GROUPBY, JOIN) and thinking: these feel related. They all change the *schema* of the dataframe. Is there a deeper relationship?

## Three shapes of schema change

Staring at Petersohn's table long enough, a pattern starts to emerge. Some operators change the *schema*, meaning which columns exist and what types they have. Others leave the schema alone and only affect the rows. And if you focus on the schema-changing ones, they fall into three groups.

**Restructuring.** You rearrange, subset, or relabel columns. The data stays the same; only the shape changes. In SQL terms: `SELECT name, salary FROM employees` produces a two-column result from a three-column table. In the dataframe library:

```haskell
select ["name", "salary"] df
-- 3-column schema → 2-column schema

rename "salary" "pay" df
-- Column name changes, data untouched

exclude ["department"] df
-- Drop a column
```

This covers Petersohn's PROJECTION and RENAME. The output schema is a function of the input schema. You can compute it without looking at any data.

**Merging.** You collapse rows that share a key, either into a summary or a collection. In SQL: `SELECT department, AVG(salary) FROM employees GROUP BY department`. In the library:

```haskell
aggregate
    [ mean (col @Double "salary") `as` "avg_salary"
    , count (col @Text "name") `as` "headcount"
    ]
    (groupBy ["department"] df)
-- Schema: name, department, salary
--       → department, avg_salary, headcount

-- Or keep all values without reducing:
aggregate
    [ collect (col @Double "salary") `as` "all_salaries" ]
    (groupBy ["department"] df)
-- Each department gets a list of all its salaries
```

Multiple rows map to the same key and get combined. This covers Petersohn's GROUPBY, and also parts of UNION and DROP DUPLICATES, which are also operations that identify or merge rows.

**Pairing.** You find rows in two tables that agree on a shared key and stitch them into a wider row. In SQL: `SELECT * FROM employees INNER JOIN departments USING (department)`. In the library:

```haskell
innerJoin ["department"] employees departments
-- Schema: (name, department, salary) + (department, budget)
--       → name, department, salary, budget
```

Shared keys appear once; unique columns from each side are concatenated. Left and outer joins are the same idea but with nullable columns where matches might be missing. This covers Petersohn's CROSS PRODUCT / JOIN.

So the relational core of Petersohn's algebra (PROJECTION, RENAME, GROUPBY, JOIN, UNION, DIFFERENCE, DROP DUPLICATES) maps onto three patterns: restructuring, merging, pairing. The schema-preserving operators (SELECTION, SORT, WINDOW) are orthogonal. They change *which rows* you see or *in what order*, but not the column structure. And the dataframe-specific operators (TRANSPOSE, MAP, TOLABELS, FROMLABELS) live outside the relational model entirely.

I had the three patterns. What I didn't have was a reason why it should be *these* three. Are restructuring, merging, and pairing truly fundamental, or did I just happen to group things this way? Could there be four patterns, or two? Is there a theory that predicts these three and explains why they compose?

That's the question my mentor Sam Stites pointed me toward when he suggested I read Fong and Spivak's *Seven Sketches in Compositionality*. The answer turns out to come from category theory. The version of category theory in Chapter 3 of that book is concrete and database-flavored. You don't need to know abstract math. You just need to think carefully about what a schema is and what it means to change one.

## Why these three

Let's start with a concrete example. Imagine you have two schemas:

```
Employees: name, department, salary
Departments: department, budget
```

There's a natural relationship between them. The `department` column in `Employees` references the `department` column in `Departments`. That's a foreign key. It's a mapping from one schema to the other: every employee row points at a department row.

Now, what can you *do* with that mapping? Fong and Spivak's Chapter 3 observes that there are exactly three things.

**You can restrict data to fit a different schema.** The schema `{name, salary}` is a subset of `Employees`. When you call `select ["name", "salary"]`, you're saying: I have data shaped like the full schema, but I only want the part that fits this smaller schema. The data doesn't change. You're just dropping the columns that aren't in the subset.

What makes this general is the idea of a *mapping between schemas*. For `select`, the mapping says "the column `name` in the small schema corresponds to `name` in the big schema, and `salary` corresponds to `salary`." That's an inclusion. For `rename "salary" "pay"`, the mapping says "`pay` in the new schema corresponds to `salary` in the old one." That's a relabeling. In both cases, there's a mapping that tells you how to translate between schemas, and the data follows.

Fong and Spivak call this **Delta (Δ)**. Given a mapping between schemas, Delta uses it to reshape the data. It never invents new data or combines rows. It only restructures what's already there.

**You can collapse data along the mapping by merging.** Multiple employees share a department. If you want data shaped like the `Departments` schema (one row per department) you have to decide what to do with all the employees that point at the same department. You could collect them into a list, sum their salaries, or count them. That's `groupBy ["department"]` followed by an aggregation.

Fong and Spivak call this **Sigma (Σ)**. Given a mapping where many source rows point at the same target, Sigma collects everything at each target. The `collect` function keeps all values as a list, which is raw Sigma. Using `sum` or `mean` composes the collection with a fold.

**You can combine data from both schemas by pairing.** Given data on *both* `Employees` and `Departments`, you can find rows that agree on `department` and stitch them into a wider row. That's `innerJoin ["department"] employees departments`. Each result row contains columns from both tables, matched on their shared key.

Fong and Spivak call this **Pi (Π)**. Their mnemonic is "pair and query data," with a footnote: "more commonly called 'join' by database programmers." Pi finds all tuples that satisfy the constraints imposed by the shared key.

So the three patterns from the previous section (restructuring, merging, pairing) have names: Δ, Σ, Π. But the names are the least interesting part. What's interesting is *why there are exactly three*.

The answer is about the structure of schema mappings. Fong and Spivak model schemas as *categories*, which is just a precise way of saying "a collection of things (tables, columns) with relationships between them (foreign keys) and a rule for following chains of relationships."

An *instance* of a schema is what you get when you assign actual data to it: a set of rows for each table, and for each foreign key, a function that maps a row to the row it references. So an instance of the `Employees`/`Departments` schema assigns rows `{Alice, Bob, Carol}` to `Employees`, rows `{Engineering, Sales}` to `Departments`, and a function that sends Alice → Engineering, Bob → Sales, Carol → Engineering. The only rule is consistency: if `Employees` references `Departments` which references `Location`, then looking up Alice's location directly has to give the same answer as looking up Alice's department and then that department's location. In category theory, an instance that satisfies this consistency rule is called a *functor*.

When you have a mapping between two schemas (also a functor), Fong and Spivak prove that it induces *exactly* these three data migration operations and no others. They're connected by a structure called an *adjoint triple*:

**Σ ⊣ Δ ⊣ Π**

Sigma is the most generous way to move data from the detailed schema to the coarser one: take everything and merge. Pi is the most conservative: only keep tuples that match on all shared attributes. Delta goes the other direction, restricting data without inventing or combining anything. The adjunction is a formal guarantee that these three compose: the output of any Δ step is a valid input for any Σ or Π step, and vice versa. That's the reason you can chain select into join into groupBy and the schemas just work out.

Here's the full picture, mapping Petersohn's operators to the categorical framework:

| Petersohn operator | Pattern | Category theory |
|---|---|---|
| PROJECTION | Restructuring | Delta |
| RENAME | Restructuring | Delta |
| GROUPBY | Merging | Sigma |
| UNION | Merging | Sigma |
| DIFFERENCE | Merging | Sigma |
| DROP DUPLICATES | Merging | Sigma |
| CROSS PRODUCT / JOIN | Pairing | Pi |
| SELECTION | (schema-preserving) | Natural transformation |
| SORT | (schema-preserving) | — |
| WINDOW | (schema-preserving) | — |
| TRANSPOSE | (dataframe-specific) | — |
| MAP | (dataframe-specific) | — |
| TOLABELS | (dataframe-specific) | — |
| FROMLABELS | (dataframe-specific) | — |

The first seven operators decompose into Δ, Σ, Π. The next three preserve the schema. They're morphisms between instances on the same schema, not migrations between schemas. The last four are outside the relational model entirely.

This is the compression: 200 pandas operators → 15 algebraic operators → 3 categorical patterns covering the relational core. The schema-preserving and dataframe-specific operators are important, but they're not where the schema complexity lives. It's the schema-changing operations (restructuring, merging, pairing) that need to compose correctly, and the adjoint triple is what makes that composition work.

## Designing an API around the three patterns

So what does this mean if you're actually building a dataframe library? The Δ/Σ/Π decomposition gives you a design principle: every schema-changing operation you expose should be expressible as one of the three patterns, and each pattern should have a clear rule for computing the output schema from the input schema.

Start with Delta. You need operations that reshape a schema without computing new data. That means:
- `select columns` → output schema is the subset of input columns you named
- `exclude columns` → output schema is the input minus the columns you named
- `rename old new` → output schema is the input with one column relabeled

These operations share a property: given the input schema and the operation's arguments, you can compute the output schema without looking at any data. That makes them cheap, predictable, and safe to reorder in an optimizer.

Then Sigma. You need operations that collapse rows by key. That means:
- `groupBy keys` followed by `aggregate [sum ..., mean ..., count ...]` → output schema is the key columns plus one new column per aggregation
- `groupBy keys` followed by `collect` → output schema is the key columns plus list-valued columns

The key insight from the categorical picture is that `collect` and `aggregate` are variations of the same pattern. Sigma collects everything at each key, and aggregation functions like `sum` or `mean` are an optional reduction on top.

Then Pi. You need operations that combine two schemas by shared key. That means:
- `innerJoin keys left right` → output schema is key columns (once) plus non-key columns from both sides
- `leftJoin keys left right` → same, but right-side non-key columns become nullable
- `fullOuterJoin keys left right` → same, but non-key columns from both sides become nullable

Each join variant is Pi with a different policy for missing matches. The schema rule is the same; only the nullability wrapping changes.

Finally, the schema-preserving operations (`filter`, `sort`, `take`, `sample`) sit outside the three patterns. Their output schema is always identical to their input schema. They're important, but they don't interact with schema composition, which is why they can be designed independently.

Once you have these pieces, a pipeline is a chain of Δ, Σ, and Π steps, and each step's output schema is a valid input for the next. In the dataframe library, Haskell's type system enforces this. Schemas are encoded at the type level, and the compiler checks every transition:

```haskell
type Employees =
    '[ Column "name" Text
     , Column "department" Text
     , Column "salary" Double
     ]

type Departments = '[ Column "department" Text, Column "budget" Double ]

result =
    employees
        & T.innerJoin @'["department"] departments  -- Π: schema grows
        & T.derive @"cost_ratio"                    -- grows by one
            (T.col @"salary" / T.col @"budget")
        & T.select @'["department", "cost_ratio"]   -- Δ: schema shrinks
        & T.groupBy @'["department"]
        & T.aggregate                               -- Σ: collapse
            ( T.agg @"avg_ratio" (T.mean (T.col @"cost_ratio"))
            $ T.aggNil
            )
```

Reference `"salary"` after `select` drops it? Compile error. Derive a column that already exists? Compile error. Join on a key missing from one table? Compile error. If the pipeline compiles, every schema transition is valid. But this idea isn't Haskell-specific. Any language with sufficiently expressive types could enforce the same rules. The three patterns tell you what the rules *are*. The type system is just the enforcement mechanism.

The patterns also help with optimization. If you know exactly what each operation does to the schema, you can reason about when it's safe to reorder steps. The library has a lazy evaluation mode where a pipeline is built up as a logical plan before being executed:

```haskell
optimize :: Int -> LogicalPlan -> PhysicalPlan
optimize batchSz =
    toPhysical batchSz
        . eliminateDeadColumns
        . pushPredicates
        . fuseFilters
```

Consecutive filters get fused into one. Filters get pushed past column operations toward the data source. Derived columns that are never referenced downstream get dropped before execution. These rewrites are safe because the operations obey algebraic laws that follow from the three patterns: restructuring and filtering commute when the filter doesn't touch restructured columns; conjunction of predicates is the same as filtering twice; a Δ step that drops a column can't affect a filter that doesn't reference it.

## Where this is going

The bigger picture I'm working toward is a canonical definition of the dataframe. Petersohn et al. made the best attempt I've seen with their data model and algebra. Category theory adds algebraic structure on top. It says the schema-changing operations decompose into three migration functors composed via adjunctions. Together, these give you a foundation: a data model that says what a dataframe *is*, an algebra that says what you can *do* with it, and a categorical framework that says why those operations *compose*.

That's what I wanted when I started the library. A small set of operations that compose into larger abstractions, with the compiler verifying every step. I think there's a lot more to explore here: the dataframe-specific operators (TRANSPOSE, TOLABELS, FROMLABELS) deserve their own algebraic treatment, and the interaction between schema-changing and schema-preserving operations could be made more precise. But the core (restructure, merge, pair) feels solid.

If any of this sounds interesting: Fong and Spivak's [*Seven Sketches in Compositionality*](https://arxiv.org/abs/1803.05316) is written for non-mathematicians and builds from first principles. Chapter 3 covers databases and the Δ/Σ/Π migration functors. Petersohn et al.'s [*Towards Scalable Dataframe Systems*](https://www2.eecs.berkeley.edu/Pubs/TechRpts/2020/EECS-2020-198.html) covers the algebra, the data model, and what people actually do in those 1 million notebooks. Both are worth your time.

The dataframe library is [on GitHub](https://github.com/mchav/dataframe). The typed API lives in `DataFrame.Typed`. I'd love to hear what you build with it.
