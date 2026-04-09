---
layout: post
title: "Grow and mow: interpretable models with boosting, symbolic regression and e-graphs"
---

This post is the convergence of two ideas that have been floating in my head for about a year. Can we learn messy stochastic models and use algorithmic/algebraic tools to rein in model complexity to make models interpretable?

A notebook with a visual TLDR is available [as an export of the marimo notebook](https://mchav.github.io/static/boosted_symbolic_regression.html). You can import the [raw notebook](https://gist.github.com/mchav/b3414b907c8e790d8d1ece791a2d73b9) into [molab](molab.marimo.io) to try it out yourself.

The first idea comes from a problem I kept running into at work.

## Boosting

I spent most of my career working in fraud detection. XGBoost (or similar techniques) dominate in this space. XGBoost approximates a function by training small decision trees to incrementally chip away at a residual. Each tree doesn't do a great job on its own, but they pick up each other's slack by collectively contributing towards the whole picture. There's an old parable about blind men touching different parts of an elephant. One feels the trunk and says it's a snake, another feels the leg and says it's a tree, a third feels the side and says it's a wall. Boosting works like this. Each tree touches a different part of the function and gets a partial answer. Their sum reaches the right conclusion. (There is a better treatment of boosting in general [here](https://xgboost.readthedocs.io/en/stable/tutorials/model.html).)

But similar to the parable, the trees end up with a lot of detail that's extraneous to the underlying function. Each individual tree tells you little about the final prediction. The ensemble is accurate but a little opaque. You can use game-theoretic techniques like [SHAP](https://shap.readthedocs.io/) to determine what each feature contributes to the output, and that gives you a coarse view of what the model is doing. But the most interpretable form a model can come in is a single tree. That's just nested if-statements. You wouldn't have to probe the model in creative ways. You could follow the lines of implication and see exactly why a decision was made.

I like this human-centric way of building models. The model is something to be understood, changed, questioned, inspected. There are practical benefits too. If you can translate a model into a single decision tree, you can translate it to SQL, to Go, to whatever your production environment uses. You're not locked into serving the model in the framework it was trained in.

So how do you go from an ensemble of hundreds of trees to a single tree?

Turns out that problem is NP-hard. A compact tree reconstructed from an ensemble is called a [born-again tree](https://arxiv.org/abs/2003.11132). The technique uses dynamic programming to regenerate a single tree that faithfully represents the ensemble. But it's computationally expensive, and it might give you a tree that tries to fit the noise of the ensemble. After all, learning is a messy process.

You could also distill the ensemble: train a new tree not against the original training data but against the ensemble's predictions. The student learns from the teacher. But even learning an optimal decision tree is NP-hard, so you fall back to greedy algorithms like CART. The process is lossy. You're not sure what detail it misses. It captures the main essence, but you've traded some accuracy for interpretability without a clear guarantee of what you kept and what you lost.

So we're kind of stuck. The ensemble is accurate but unreadable. Collapsing it exactly is intractable. Distilling it is lossy and hard to reason about.

Let's hold on to this thought and take a detour.

## Symbolic regression

Symbolic regression is a different approach to learning from data entirely. Instead of fitting parameters of a fixed model (like linear regression fitting coefficients), it searches over the space of mathematical expressions to discover the structure itself. The output isn't a weight matrix or a tree. It's an equation: $y = 2.5\,x^2 + 0.3\,x$, or $F = G\,m_1\,m_2 / r^2$.

Most symbolic regression methods use genetic programming. You generate a population of candidate expressions, evaluate them against the data, run tournaments where the best survive, crossover and mutate the survivors, and repeat. You are effectively trying to one-shot the learning problem: find the right expression structure and the right constants in a single search.

This works on small-ish datasets and is good at recovering the equation that describes the underlying data. Symbolic regression has been popular in the sciences because it promises testable hypotheses rather than a black box that produces just answers. It's the process Kepler went through trying to find laws of planetary motion, except automated. In a [podcast with Terrence Tao](https://www.dwarkeshpatel.com/p/terence-tao), Dwarkesh Patel described Kepler's process as being like a "high temperature LLM". The analogy captures the trial-and-error aspect but misses the heart of it. Symbolic methods are greatly improved by a combination of reasoning and search. [PySR](https://github.com/MilesCranmer/PySR), the most popular tool for this, runs multi-population evolutionary search and recovers exact physical laws from noisy data in seconds.

There's also recent work by Fabricio Olivetti de França on [eggp](https://github.com/folivetti/eggp), a symbolic regression system that uses e-graphs to prune the search space. We're already a few digressions deep, but the short version is that e-graphs are a data structure that stores many equivalent forms of an expression simultaneously, and after applying algebraic rewrite rules, gives you back the simplest one. Think of it as a compiler optimization pass for mathematical expressions.

## The convergence

So XGBoost has this property where an ensemble of trees is accurate but hard to collapse into something readable. And symbolic regression typically tries to one-shot learn a single equation from data. What if we combined the two approaches?

The idea is: use boosting to build an ensemble of small symbolic expressions (not trees, but tiny equations like `0.3*x^2` or `sin(x0)`), then collapse the ensemble algebraically into a single clean formula. Accept that learning is messy, but have techniques to consolidate it later.

This is similar to how studying works. You take a bunch of scattered notes during a lecture, full of redundancies and partial thoughts, and later consolidate them into a neater structure. Traditional symbolic regression tries to make the lecture notes and the final summary the same document. Boosting lets them be different. You scribble fast and clean up later.

It also reminds me of the wake-sleep algorithm from the [DreamCoder](https://arxiv.org/abs/2006.08381) paper on program synthesis. DreamCoder alternates between a "wake" phase (solve problems using your current library) and a "sleep" phase (compress solutions into reusable abstractions). The wake phase is messy and exploratory. The sleep phase consolidates. Symbolic boosting has the same rhythm: grow the ensemble (wake), simplify it (sleep), grow again, simplify again.


## How it works

Concretely, symbolic boosting works like this. Initialize the model as the mean of the target. The residuals are everything the mean doesn't explain. Then, each round:

1. Generate 50 random expression trees, each with at most 7 nodes. The grammar includes `+`, `-`, `*`, and squaring.
2. Fit each candidate's learnable parameters against the current residuals using L-BFGS-B (30 iterations, 2 random restarts).
3. Score each candidate by how much it reduces the residual variance.
4. Pick the winner. Shrink it by a learning rate (default 0.1) and add it to the ensemble.
5. Update residuals and go again.

After 30 rounds, the ensemble might look like:

```
0.83*x*y + (-0.90)*x*y + 1.18*y*(x+y) - 1.01*(y-0.51)^2 - 0.42*y - 0.76
```

That's the scribbled lecture notes. Redundant terms, partial cancellations, coefficients scattered everywhere. The true answer is `x*y`, buried under noise.

Now for the consolidation.

## Simplification: polynomial normal form

If you ignore transcendental functions (`sin`, `cos`, `exp`, `log`), the boosting ensemble is a polynomial. And polynomials have a canonical form where identical monomials are merged automatically.

Take the messy ensemble above:

$$-0.18\,y(x+y) + 1.18\,y(x+y) - 1.01(y-0.51)^2 - 0.42\,y - 0.76$$

Expand every product and collect like terms:

| Step | What happens |
|------|-------------|
| Expand products | $y(x+y) \to xy + y^2$ |
| Merge like terms | $-0.18\,xy + 1.18\,xy \to 1.0\,xy$ |
| Cancel near-zero | $y^2$ coefficient $\approx -0.01$, dropped |
| Collect constants | $-0.26 + (-0.76) \to -1.02$ |

Result: $xy + 0.77\,y - 1.02$. Three terms instead of seventeen.

The implementation is around 25 lines of Python using SymPy:

```python
TRANSCENDENTALS = (sympy.sin, sympy.cos, sympy.exp, sympy.log)

def poly_nf(expr, threshold=1e-9):
    atom_map, ctr = {}, [0]
    def replace_trans(e):
        if e.func in TRANSCENDENTALS:
            inner = poly_nf(e.args[0], threshold)
            ne = e.func(inner); k = str(ne)
            if k not in atom_map:
                s = sympy.Symbol(f"atom{ctr[0]}"); ctr[0] += 1
                atom_map[k] = (s, ne)
            return atom_map[k][0]
        return (e.func(*[replace_trans(a) for a in e.args])
                if e.args else e)
    replaced = replace_trans(expr)
    expanded = sympy.expand(replaced)
    terms = sympy.Add.make_args(expanded)
    if len(terms) > 200:
        return expr
    kept = [t for t in terms
            if abs(float(t.as_coeff_Mul()[0])) > threshold]
    result = sympy.Add(*kept) if kept else sympy.Float(0)
    for key, (s, orig) in atom_map.items():
        result = result.subs(s, orig)
    return result
```

First, replace every transcendental subexpression (`sin(...)`, `exp(...)`, etc.) with a fresh placeholder symbol. This draws a boundary: everything polynomial gets expanded and merged, everything transcendental stays opaque. Then `sympy.expand` distributes all products over sums. Like terms merge automatically. Drop anything with a near-zero coefficient.

The transcendental boundary is important. Without it, `sympy.expand` would try to expand through `sin(x + y)` and fail. With it, `sin(x + y)` becomes an atom that participates in polynomial arithmetic the same way a variable does. After simplification, the placeholders are substituted back.

The blowup guard (bail out if expansion produces more than 200 terms) prevents polynomial multiplication from cascading. Two 15-term polynomials multiplied together produce up to 225 monomials. Without the guard, nested products could blow up the representation.

## Simplification: magnitude pruning

Polynomial normal form is algebraic simplification. Magnitude pruning is statistical simplification.

After expansion, you might have a monomial like `0.003 * cos(y)` alongside a dominant `98.7 * x * y`. Magnitude pruning evaluates each monomial on the training data, measures its peak absolute contribution, and drops anything contributing less than 3% of the dominant term. The `cos(y)` correction is noise relative to the signal. Drop it.

```python
def mag_prune(expr, X_data, threshold=0.03):
    expanded = sympy.expand(expr)
    terms = list(sympy.Add.make_args(expanded))
    if len(terms) <= 1:
        return expr
    fsyms = sorted(expr.free_symbols, key=str)
    peaks = []
    for t in terms:
        try:
            fn = sympy.lambdify(fsyms, t, modules=["numpy"])
            vals = fn(*[X_data[:, int(str(s)[1:])] for s in fsyms])
            peaks.append(float(np.nanmax(np.abs(
                np.nan_to_num(vals, nan=0.0)))))
        except Exception:
            peaks.append(float("inf"))
    mx = max(peaks) if peaks else 1.0
    if mx == 0:
        return expr
    kept = [t for t, p in zip(terms, peaks) if p / mx >= threshold]
    return sympy.Add(*kept) if kept else expr
```

## Simplification: e-graph cleanup

Polynomial normal form and magnitude pruning handle polynomial redundancy and statistical insignificance. But there's a third kind of redundancy: algebraic identities involving transcendental functions.

If the ensemble contains `log(exp(x))`, polynomial normal form treats it as an opaque atom and leaves it alone. It doesn't know that `log(exp(x)) = x`. Similarly, `sqrt(x)^2 = x`, `x + 0 = x`, and `x * 1 = x` are identities that polynomial algebra can't simplify.

This is where e-graphs come back. The notebook uses [egglog](https://github.com/egraphs-good/egglog) to apply a conservative set of rewrite rules:

```python
erules = [
    rewrite(a + MathE(0.0)).to(a),        # x + 0 → x
    rewrite(a * MathE(1.0)).to(a),         # x * 1 → x
    rewrite(a * MathE(0.0)).to(MathE(0.0)),# x * 0 → 0
    rewrite(-(-a)).to(a),                  # double negation
    rewrite(Log(Exp(a))).to(a),            # log(exp(x)) → x
    rewrite(Exp(Log(a))).to(a),            # exp(log(x)) → x
    rewrite(Sqrt(a) ** MathE(2.0)).to(a),  # sqrt(x)^2 → x
    rewrite(a * a).to(a ** MathE(2.0)),    # x*x → x^2
]
```

Insert the expression, run saturation for 30 iterations, extract the smallest equivalent.

The rules are deliberately conservative. Distributivity (`a*(b+c) = a*b + a*c`) is not included because it causes e-graphs to explode. Polynomial normal form already handles distributivity structurally, so the e-graph only needs to handle what polynomials can't.

## The sawtooth

The boosting loop doesn't simplify every round. It simplifies every 5 rounds. The ensemble grows messy for 5 rounds, then collapses when the simplification pipeline fires. Then it grows messy again, then collapses again.

This creates a sawtooth pattern in expression complexity. The notebook visualizes this: the top plot shows complexity spiking up and crashing down at each collapse event, the bottom plot shows R-squared climbing steadily through the collapses. The collapses don't hurt accuracy because simplification doesn't change semantics. Merging `0.83*x*y + (-0.90)*x*y` into `-0.07*x*y` produces the exact same predictions on every input. The expression gets smaller, not different.

The rhythm is: grow, simplify, grow, simplify. Scribble, consolidate, scribble, consolidate.

## Where it works and where it doesn't

The notebook includes four target functions to try boosting against.

**`2.5*x^2 + 0.3*x`** (simple quadratic). A polynomial. Boosting nails it. The ensemble discovers `x^2` and `x` terms early, and polynomial normal form collapses them into the exact answer.

**`sin(x1) + cos(x2)`** (trigonometric). Also additive. Each trig function is a plausible weak learner on its own. Boosting finds the first term, the residuals look like `cos(x2)`, and it finds the second.

**`3.0*x1^2 + 0.5/x2`** (physics-inspired). The polynomial part is easy. The `0.5/x2` is a single division. If the random generator produces something like `param0 / x1`, parameter fitting finds the right coefficient.

**`x1 * exp(-x2^2) + 2.3`** (complex). This is the hard one. `exp(-x2^2)` is a deeply nested composition. No single 7-node weak learner can express `x1 * exp(-x2^2)`. The ensemble will approximate it with polynomial corrections, but it's unlikely to recover the exact structure.

This is the fundamental limitation. Some mathematical structures are not additive. `exp(-x^2)` has to be built as a single composite expression, not assembled from a sum of corrections. Boosting will converge to a good numerical approximation, but the formula it produces will be a polynomial approximation of the true equation, not the equation itself.

PySR's evolutionary approach doesn't have this bias. It searches over full expression trees, so it can discover `exp(-x^2)` as a single unit. The tradeoff: PySR needs more time and is more stochastic. Boosting is faster and more predictable, but structurally limited to additive decompositions.

The sweet spot is problems where the underlying equation is a sum of a few terms, each of moderate complexity. Coulomb's law (`q1*q2 / r^2`), kinetic energy (`0.5*m*v^2`), spring force (`k*x`). Additive or multiplicative structure that the polynomial representation can collapse.

## Try it yourself

The three tools that make this possible: [PySR](https://github.com/MilesCranmer/PySR) for evolutionary symbolic regression, [SymPy](https://www.sympy.org/) for symbolic algebra and the polynomial normal form, and [egglog](https://github.com/egraphs-good/egglog) for e-graph equality saturation.

Marimo notebooks are great for exploring this sort of approach since they make visualizing the outputs easy. We can leverage human intelligence to further audit and simplify models.

## Why this is exciting

Symbolic regression has always been hamstrung by scale. Finding ways to make training loops smaller and more efficient removes an important hurdle to its adoption. Finding different ways to create symbolic models that search for or approximate exact equations might be a chapter in the AI story where we actually think about how we can collaborate with machines to discover new things.
