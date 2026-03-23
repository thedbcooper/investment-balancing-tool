# Portfolio Rebalancing Tool

A Shiny app that calculates how to allocate new investment funds across a portfolio to bring allocations as close as possible to target weights — **without selling any assets**.

Based on: J. Bartroff, "Rebalance your portfolio without selling", arXiv:2305.12274.
This repository implements the algorithms from that paper; the implementation is original code. See the paper for theory and proofs:

> Bartroff, J. (2023). *Rebalance your portfolio without selling.* College Mathematics Journal. [arXiv:2305.12274](https://arxiv.org/abs/2305.12274)

---

## Why Not Differential Evolution?

**Differential evolution (DE)** is a stochastic numerical optimiser that was considered as an alternative approach. It would search over candidate allocations, iteratively minimising tracking error against target weights. While capable, it was rejected in favour of the closed-form solutions for the following reasons:

- **Exact optimality**: the ℓ₁ and ℓ₂ solutions are mathematically proven to be globally optimal. DE is heuristic and provides no such guarantee.
- **Dollar amounts**: this app works in dollar values, where fractional purchases are allowed (e.g. mutual funds, dollar-based brokers). The continuous closed-form solutions are directly applicable and more precise than DE, which is better suited to discrete whole-share problems.
- **Transparency**: each solution reduces to a single interpretable formula — a deflation factor α or threshold λ\*. DE is a black-box search with no intuitive explanation of the resulting allocation.
- **Speed and determinism**: the analytical solutions run instantly and always produce the same result. DE requires many objective evaluations and a fixed random seed for reproducibility.

DE would have an edge if whole-share constraints or per-trade fee schedules were required. Neither applies here.

---

## The Problem

You have a portfolio of $n$ assets with current values $x_1, \ldots, x_n$ and target proportions $p_1, \ldots, p_n$ (summing to 1). You want to invest a fixed additional amount $y > 0$ to get as close as possible to your targets, **without selling anything**.

The **naive adjustments** that would achieve exact targets if selling were allowed are:

$$\delta_i = p_i(x + y) - x_i$$

Since some $\delta_i$ may be negative (sell orders), we can't apply them directly.

---

## Two Optimal Solutions

The paper proves two closed-form solutions depending on how "as close as possible" is defined.

### ℓ₂ Solution — Minimises Sum of Squared Deviations (Theorem 1)

Find k*: the largest k such that sum_{i=1}^{k}(delta_i - delta_k) < y (with deltas sorted descending). Then:

    lambda* = (sum_{i=1}^{k*} delta_i - y) / k*
    y_i*    = (delta_i - lambda*)+

Only the k* assets with the **largest** naive adjustments receive funds. Best when you want to close the biggest gaps first.

### ℓ₁ Solution — Minimises Sum of Absolute Deviations (Theorem 2)

In the normal portfolio case y <= sum(delta_i+), a deflation factor is applied uniformly:

    alpha = y / sum(delta_i+)
    y_i*  = alpha * delta_i+

**All** underweight assets receive funds proportionally to how underweight they are. When y > sum(delta_i+) (surplus), the excess is distributed evenly across all assets.

### Key difference

|                        | ℓ₁                          | ℓ₂                              |
|------------------------|-----------------------------|---------------------------------|
| Assets receiving funds | All underweight             | Only top k* underweight         |
| Behaviour              | Spreads funds proportionally | Concentrates on largest gaps   |
| Optimises              | Sum of absolute deviations  | Sum of squared deviations       |

---

## Features

- **Input table** — edit assets, tickers, current values, and target % directly in the app
- **CSV upload** — load a portfolio from a CSV file
- **Both solutions** — ℓ₁ and ℓ₂ results with final allocations shown
- **Full rebalance reference** — naive buy/sell rebalance for comparison
- **Key scalars** — deflation factor α, threshold λ*, and minimum new funds needed to avoid any selling

---

## CSV Format

```csv
Asset,Ticker,Group,Current_Value,Target_Percent,Chosen Weight for Buy
Total US Stock Market Index,VTI,US Stocks,40739.2,70,
Total International Stock Index,VXUS,International Stocks,13062.5,20,
Total Bond Market,BND,Bonds,5019,10,100
Treasury ETF,VGIT,Bonds,2000,10,0
```

- `Target_Percent` values must sum to 100.
- `Group` is a label used to group assets (e.g. by asset class). Assets sharing a `Group` and `Target_Percent` compete for the same target allocation via the `Chosen Weight for Buy` column.
- `Chosen Weight for Buy` is optional. When multiple assets share a target group, this sets the relative weight for how new purchases within that group are split. Leave blank to use the default.

---

## Running the App Locally

Restore the project environment using `renv` before running the app.

```r
# 1. Install renv if you don't already have it
install.packages("renv")

# 2. Restore the project library from renv.lock
renv::restore()

# 3. Run the app
shiny::runApp()
```
