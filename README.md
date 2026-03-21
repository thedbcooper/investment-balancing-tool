# Portfolio Rebalancing Tool

A Shiny app that calculates how to allocate new investment funds across a portfolio to bring allocations as close as possible to target weights — **without selling any assets**.

Based on: J. Bartroff, "Rebalance your portfolio without selling", arXiv:2305.12274.
This repository implements the algorithms from that paper; the implementation is original code. See the paper for theory and proofs:

> Bartroff, J. (2023). *Rebalance your portfolio without selling.* College Mathematics Journal. [arXiv:2305.12274](https://arxiv.org/abs/2305.12274)

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
Asset,Ticker,Current_Value,Target_Percent
Total US Stock Market Index,VTI,40739.2,70
Total International Stock Index,VXUS,13062.5,20
Bonds,BND,7019,10
```

`Target_Percent` values must sum to 100.

---

## Running the App

```r
# Install dependencies if needed
install.packages(c("shiny", "DT", "tidyverse", "bslib"))

# Run
shiny::runApp("app.R")
```
