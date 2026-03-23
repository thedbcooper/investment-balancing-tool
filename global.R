library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(readr)
library(plotly)
library(DT)
library(rhandsontable)

# ── ℓ₂ helper: Theorem 1 of Bartroff (2023) ──────────────────────────────────
# Given naive adjustments delta and total new funds y,
# returns optimal buy amounts y* = (delta_i - lambda*)_+
l2_adjustments <- function(delta, y) {
  n   <- length(delta)
  ord <- order(delta, decreasing = TRUE)
  d   <- delta[ord]

  k_star <- 1
  for (k in seq_len(n)) {
    if (sum(d[seq_len(k)] - d[k]) < y) k_star <- k
  }

  lambda_star   <- (sum(d[seq_len(k_star)]) - y) / k_star
  y_star_sorted <- pmax(d - lambda_star, 0)

  y_star      <- numeric(n)
  y_star[ord] <- y_star_sorted
  y_star
}

# ── ℓ₁ helper: Theorem 2 of Bartroff (2023) ──────────────────────────────────
# Case 2 (normal): y ≤ Σδᵢ⁺  →  α = y / Σδᵢ⁺,  yᵢ* = α·δᵢ⁺
# Case 1 (surplus): y > Σδᵢ⁺  →  yᵢ* = δᵢ⁺ + ε,  ε = (y - Σδᵢ⁺) / n
l1_adjustments <- function(delta, y) {
  n       <- length(delta)
  pos_sum <- sum(pmax(delta, 0))

  if (pos_sum == 0) return(rep(y / n, n))

  if (y > pos_sum) {
    epsilon <- (y - pos_sum) / n
    pmax(delta, 0) + epsilon
  } else {
    alpha <- y / pos_sum
    alpha * pmax(delta, 0)
  }
}

# ── Shared helpers ────────────────────────────────────────────────────────────
make_pie <- function(labels, values, title) {
  plot_ly(
    labels        = labels,
    values        = values,
    type          = "pie",
    sort          = FALSE,
    textinfo      = "label+percent",
    hovertemplate = "%{label}<br>$%{value:,.0f}<br>%{percent}<extra></extra>"
  ) |>
    layout(
      title  = list(text = title, font = list(size = 15)),
      margin = list(l = 10, r = 10, t = 40, b = 10),
      legend = list(orientation = "h", y = -0.15)
    ) |>
    config(displayModeBar = FALSE)
}

fmt_dt <- function(df_renamed, currency_cols, pct_cols) {
  DT::datatable(
    df_renamed,
    rownames = FALSE,
    options  = list(dom = "t", pageLength = 25)
  ) |>
    formatCurrency(currency_cols, digits = 2) |>
    formatPercentage(pct_cols, digits = 2)
}

first_group_target <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) 0 else x[[1]]
}

safe_share <- function(value, total, n_items) {
  ifelse(total > 0, value / total, 1 / n_items)
}

method_info <- list(
  l1 = list(
    title       = "ℓ₁ Solution — Proportional allocation (Theorem 2)",
    description = "Minimises sum of absolute deviations from group targets. Each group's buy amount is then split across its assets using the chosen buy weights, which default to current within-group weights.",
    group_value_col = "L1_Group_Value_After",
    value_col       = "L1_Value_After",
    buy_col         = "L1_Buys",
    group_pct_col   = "L1_Group_Final_Pct",
    pct_col         = "L1_Final_Pct"
  ),
  l2 = list(
    title       = "ℓ₂ Solution — Threshold allocation (Theorem 1)",
    description = "Minimises sum of squared deviations from group targets. Each group's buy amount is then split across its assets using the chosen buy weights, which default to current within-group weights.",
    group_value_col = "L2_Group_Value_After",
    value_col       = "L2_Value_After",
    buy_col         = "L2_Buys",
    group_pct_col   = "L2_Group_Final_Pct",
    pct_col         = "L2_Final_Pct"
  ),
  sell = list(
    title       = "Full Rebalance (With Selling) — Group targets met exactly",
    description = "Reference only: buy and sell at the group level to hit target group weights exactly, then distribute each group's trades across its assets using the chosen buy weights.",
    group_value_col = "Group_Target_Value",
    value_col       = "Target_Value",
    buy_col         = "Naive_Adj",
    group_pct_col   = "Group_Target_Percent",
    pct_col         = "Final_Pct"
  )
)
