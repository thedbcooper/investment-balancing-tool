# Copilot Instructions — Portfolio Rebalancing Tool

## What This Project Is

A Shiny app implementing two closed-form portfolio rebalancing algorithms from Bartroff (2023, arXiv:2305.12274): buy-only rebalancing without selling, using ℓ₁ (proportional) and ℓ₂ (threshold) optimal solutions. Dollar-amount (fractional) purchases only — no whole-share constraints.

---

## Architecture

Single-directory Shiny app with three files:

| File | Role |
|------|------|
| `global.R` | Pure functions + `method_info` dispatch list. Loaded before `server.R`/`ui.R`. |
| `server.R` | All reactivity. `results()` is the central reactive that computes everything. |
| `ui.R` | `bslib::page_navbar` layout with sidebar, three `nav_panel`s. |

**Data flow:**
1. `values$data` (reactiveValues) ← `rhandsontable` edits or CSV upload
2. `clean_input_data()` normalises every write to `values$data`
3. `results()` reactive: group-level ℓ₁/ℓ₂ calculations → asset-level splits via `Buy_Weight`
4. Outputs consume `results()` and dispatch via `method_info[[input$method]]`

---

## Key Patterns

### Group/Asset two-level structure
Assets belong to groups (`Group` column). Rebalancing targets operate at the **group level**; the `Chosen Weight for Buy` column (optional) controls how group buys are split across assets within a group. Default split = current within-group value weight (`Within_Group_Weight`).

```r
# In server.R results() — group-level first, then distribute to assets
group_df <- df |> summarize(..., .by = Group) |> mutate(L1_Group_Buys = l1_adjustments(...))
asset_df <- df |> left_join(group_df, by = "Group") |> mutate(L1_Buys = L1_Group_Buys * Buy_Weight)
```

### `method_info` dispatch
`global.R` defines `method_info` as a named list (`l1`, `l2`, `sell`). Output renderers use `method_info[[input$method]]` to look up column names rather than `if/else` branching:

```r
meta <- method_info[[input$method]]
results()$df[[meta$buy_col]]   # e.g. "L1_Buys", "L2_Buys", or "Naive_Adj"
```

When adding a new method, add an entry to `method_info` in `global.R` — no server-side `if/else` needed.

### `clean_input_data()` is the single normalisation point
All writes to `values$data` pass through `clean_input_data()`. It enforces column order, coerces types, fills missing `Group` with `Asset`, and strips blank rows. Never write raw data to `values$data`.

### Core algorithm functions (`global.R`)
- `l2_adjustments(delta, y)` — Theorem 1: threshold λ*, funds to top k* assets
- `l1_adjustments(delta, y)` — Theorem 2: deflation α, proportional to all underweight assets
- Both operate on **group-level** naive adjustments `δᵢ = pᵢ(x+y) − xᵢ`

---

## Developer Workflow

```r
renv::restore()       # restore packages from renv.lock (first time)
shiny::runApp()       # run the app
```

- `renv` manages all dependencies — always use `renv::snapshot()` after adding packages
- No test suite currently exists
- `portfolio.csv` is the default starting data loaded at startup

---