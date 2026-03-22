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
  d   <- delta[ord]                      # sorted descending

  # k*: largest k s.t. sum_{i=1}^{k}(d_i - d_k) < y  [Theorem 1, eq. 5]
  k_star <- 1
  for (k in 1:n) {
    if (sum(d[1:k] - d[k]) < y) k_star <- k
  }

  lambda_star   <- (sum(d[1:k_star]) - y) / k_star
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

  if (pos_sum == 0) return(rep(y / n, n))   # edge case: all negative deltas

  if (y > pos_sum) {
    # Case 1: more funds than needed to fully close all positive gaps
    epsilon <- (y - pos_sum) / n
    pmax(delta, 0) + epsilon
  } else {
    # Case 2: the normal portfolio rebalancing case (Σδᵢ = y so Σδᵢ⁺ ≥ y)
    alpha <- y / pos_sum
    alpha * pmax(delta, 0)
  }
}

# ── Shared pie chart helper ───────────────────────────────────────────────────
make_pie <- function(labels, values, title) {
  plot_ly(
    labels        = labels,
    values        = values,
    type          = "pie",
    sort          = FALSE,
    textinfo      = "label+percent",
    hovertemplate = "%{label}<br>$%{value:,.0f}<extra></extra>"
  ) |>
    layout(
      title  = list(text = title, font = list(size = 15)),
      margin = list(l = 10, r = 10, t = 40, b = 10),
      legend = list(orientation = "h", y = -0.15)
    ) |>
    config(displayModeBar = FALSE)
}

# ── Shared DT helper — accepts already-renamed df, formats currency + pct ─────
# currency_cols and pct_cols must match the renamed column names passed in
fmt_dt <- function(df_renamed, currency_cols, pct_cols) {
  DT::datatable(
    df_renamed,
    rownames = FALSE,
    options  = list(dom = "t", pageLength = 25)
  ) |>
    formatCurrency(currency_cols, digits = 2) |>
    formatPercentage(pct_cols, digits = 2)
}

# ── Method metadata lookup ────────────────────────────────────────────────────
# Maps radio button value → display strings and column names used in results df
method_info <- list(
  l1 = list(
    title       = "ℓ₁ Solution — Proportional allocation (Theorem 2)",
    description = "Minimises sum of absolute deviations from targets. Deflates all positive naive adjustments by α = y / Σδᵢ⁺ (case 2), or adds surplus evenly when y > Σδᵢ⁺ (case 1).",
    value_col   = "L1_Value_After",
    buy_col     = "L1_Buys",
    pct_col     = "L1_Final_Pct"
  ),
  l2 = list(
    title       = "ℓ₂ Solution — Threshold allocation (Theorem 1)",
    description = "Minimises sum of squared deviations from targets. Adds only to the k* assets with the largest naive adjustments, via threshold λ* = (Σᵢ₌₁ᵏ* δᵢ − y) / k*.",
    value_col   = "L2_Value_After",
    buy_col     = "L2_Buys",
    pct_col     = "L2_Final_Pct"
  ),
  sell = list(
    title       = "Full Rebalance (With Selling) — Naive adjustments in full",
    description = "Reference only: the traditional buy-and-sell rebalance to exact targets.",
    value_col   = "Target_Value",
    buy_col     = "Naive_Adj",
    pct_col     = NULL
  )
)

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- page_sidebar(
  title = "Portfolio Rebalancing Tool",
  theme = bs_theme(
    bootswatch = "flatly",
    base_font  = font_google("Inter")
  ),

  tags$head(tags$style(HTML("
    .method-desc { font-size: 0.85rem; color: #6c757d; margin-bottom: 1rem; }
    .total-row   { font-size: 0.85rem; color: #495057; padding: 6px 4px 2px; }
    .total-row strong { color: #212529; }
    .min-buy-btn {
      width: 100%;
      text-align: left;
      padding: 0.75rem 1rem;
      border: 1px solid #198754;
      border-radius: 0.5rem;
      background: #f8fff9;
      color: #198754;
      font-weight: 600;
      cursor: pointer;
    }
    .min-buy-btn:hover {
      background: #1eac41;
    }
    .min-buy-label {
      display: block;
      font-size: 0.8rem;
      margin-bottom: 0.25rem;
    }
    .min-buy-value {
      display: block;
      font-size: 1.1rem;
    }
  "))),

  # ── Sidebar ────────────────────────────────────────────────────────────────
  sidebar = sidebar(
    width = 290,

    fileInput("upload_csv", "Upload CSV",
              accept      = ".csv",
              placeholder = "Asset, Ticker, Current_Value, Target_Percent"),

    numericInput("new_funds", "Total New Buy Amount ($):", value = 3000, min = 0),

    uiOutput("min_buy_sidebar"),

    hr(),

    # Method selector — ℓ₁ is the recommended default (buy-only, ℓ₁-optimal)
    radioButtons(
      "method", "Rebalancing Method:",
      choices = c(
        "ℓ₁ — Proportional (Theorem 2)" = "l1",
        "ℓ₂ — Threshold (Theorem 1)"    = "l2",
        "Full Rebalance (With Selling)"  = "sell"
      ),
      selected = "l1"
    ),

    hr(),

    helpText("Edit the asset table directly. Use the table context menu to insert or remove rows."),
    uiOutput("target_warning")
  ),

  # ── Main panel ─────────────────────────────────────────────────────────────
  navset_card_underline(
    id = "main_tabs",

    # ── Tab 1: Input data ───────────────────────────────────────────────────
    nav_panel(
      "Input Data",
      card_body(
        rHandsontableOutput("input_table"),
        tags$p(
          class = "text-muted small mt-2 mb-0",
          "Double-click to edit cells. Right-click a row to add or remove assets."
        )
      )
    ),

    # ── Tab 2: Rebalance analysis ───────────────────────────────────────────
    nav_panel(
      "Rebalance Analysis",

      # Dynamic title + description for the selected method
      uiOutput("method_header"),

      # ── Current vs Rebalanced columns — each has pie + table + totals ────
      layout_columns(
        col_widths = c(6, 6),

        # Current portfolio column
        card(
          card_header("Current"),
          plotlyOutput("pie_current", height = "300px"),
          card_body(
            DT::DTOutput("table_current"),
            uiOutput("totals_current")
          )
        ),

        # Rebalanced portfolio column
        card(
          card_header("Rebalanced"),
          plotlyOutput("pie_rebalanced", height = "300px"),
          card_body(
            DT::DTOutput("table_rebalanced"),
            uiOutput("totals_rebalanced")
          )
        )
      )
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  clean_input_data <- function(df) {
    expected <- c("Asset", "Ticker", "Current_Value", "Target_Percent")

    df <- as.data.frame(df, stringsAsFactors = FALSE)

    for (col in setdiff(expected, names(df))) {
      df[[col]] <- NA
    }

    df <- df[, expected, drop = FALSE]
    df$Asset <- trimws(ifelse(is.na(df$Asset), "", as.character(df$Asset)))
    df$Ticker <- trimws(ifelse(is.na(df$Ticker), "", as.character(df$Ticker)))
    df$Current_Value <- suppressWarnings(as.numeric(df$Current_Value))
    df$Target_Percent <- suppressWarnings(as.numeric(df$Target_Percent))

    keep_row <-
      df$Asset != "" |
      df$Ticker != "" |
      !is.na(df$Current_Value) |
      !is.na(df$Target_Percent)

    df <- df[keep_row, , drop = FALSE]
    df$Current_Value[is.na(df$Current_Value)] <- 0
    df$Target_Percent[is.na(df$Target_Percent)] <- 0
    rownames(df) <- NULL
    df
  }

  values <- reactiveValues(
    data = data.frame(
      Asset          = c("Total US Stock Market Index", "Total International Stock Index", "Bonds"),
      Ticker         = c("VTI", "VXUS", "BND"),
      Current_Value  = c(40739.2, 13062.5, 7019),
      Target_Percent = c(70, 20, 10),
      stringsAsFactors = FALSE
    )
  )

  # CSV upload — replace table contents if columns match expected schema
  observeEvent(input$upload_csv, {
    req(input$upload_csv)
    uploaded <- read_csv(input$upload_csv$datapath, show_col_types = FALSE)
    expected <- c("Asset", "Ticker", "Current_Value", "Target_Percent")
    if (all(expected %in% names(uploaded))) {
      values$data <- uploaded |> select(all_of(expected)) |> clean_input_data()
    }
  })

  # Warn if target %s don't sum to 100
  output$target_warning <- renderUI({
    pct_sum <- sum(as.numeric(values$data$Target_Percent), na.rm = TRUE)
    if (abs(pct_sum - 100) > 0.01) {
      tags$p(
        style = "color: #dc3545; font-weight: bold; font-size: 0.85rem;",
        paste0("⚠ Target % sum = ", round(pct_sum, 2), "% (must be 100)")
      )
    }
  })

  output$input_table <- renderRHandsontable({
    rhandsontable(
      values$data,
      rowHeaders = NULL,
      useTypes = TRUE,
      stretchH = "all",
      contextMenu = TRUE,
      manualRowMove = TRUE,
      minSpareRows = 1
    ) |>
      hot_col("Asset", type = "text") |>
      hot_col("Ticker", type = "text") |>
      hot_col("Current_Value", type = "numeric", format = "0,0.00") |>
      hot_col("Target_Percent", type = "numeric", format = "0,0.00")
  })

  observeEvent(input$input_table, {
    values$data <- hot_to_r(input$input_table) |> clean_input_data()
  })



  # ── Core calculations ──────────────────────────────────────────────────────
  results <- reactive({
    df <- values$data
    df$Current_Value  <- as.numeric(df$Current_Value)
    df$Target_Percent <- as.numeric(df$Target_Percent) / 100

    x  <- sum(df$Current_Value)   # current portfolio value
    y  <- input$new_funds         # new funds to invest
    xf <- x + y                   # final portfolio value

    # Naive adjustments: δᵢ = pᵢ(x + y) − xᵢ  [eq. 1]
    df$Naive_Adj <- xf * df$Target_Percent - df$Current_Value

    # Min buy to avoid any selling: max(xᵢ / pᵢ) − x
    min_buy_no_sell <- max(0, max(df$Current_Value / df$Target_Percent, na.rm = TRUE) - x)

    # ── ℓ₁ solution (Theorem 2)
    df$L1_Buys        <- l1_adjustments(df$Naive_Adj, y)
    df$L1_Value_After <- df$Current_Value + df$L1_Buys
    df$L1_Final_Pct   <- df$L1_Value_After / xf

    # ── ℓ₂ solution (Theorem 1)
    df$L2_Buys        <- l2_adjustments(df$Naive_Adj, y)
    df$L2_Value_After <- df$Current_Value + df$L2_Buys
    df$L2_Final_Pct   <- df$L2_Value_After / xf

    # Key scalars for display
    pos_sum <- sum(pmax(df$Naive_Adj, 0))
    alpha   <- if (y <= pos_sum) y / pos_sum else NA_real_   # NA in case 1

    d_sorted    <- sort(df$Naive_Adj, decreasing = TRUE)
    n           <- length(d_sorted)
    k_star      <- 1
    for (k in 1:n) if (sum(d_sorted[1:k] - d_sorted[k]) < y) k_star <- k
    lambda_star <- (sum(d_sorted[1:k_star]) - y) / k_star

    # Full naive rebalance (reference)
    df$Target_Value <- xf * df$Target_Percent

    list(df = df, alpha = alpha, lambda = lambda_star,
         min_buy = min_buy_no_sell, x = x, xf = xf, y = y)
  })

  output$min_buy_sidebar <- renderUI({
    req(results())
    actionButton(
      "use_min_buy",
      label = HTML(paste0(
        '<span class="min-buy-label">Min Buy to Avoid Selling</span>',
        '<span class="min-buy-value">$', format(round(results()$min_buy, 2), big.mark = ','), '</span>'
      )),
      class = "min-buy-btn"
    )
  })

  observeEvent(input$use_min_buy, {
    updateNumericInput(session, "new_funds", value = results()$min_buy)
  })

  # ── Dynamic method header (title + description) ────────────────────────────
  output$method_header <- renderUI({
    meta <- method_info[[input$method]]
    tagList(
      tags$h5(meta$title,      class = "mt-3 mb-1 fw-semibold"),
      tags$p(meta$description, class = "method-desc")
    )
  })

  # ── Pie charts — react to both results() and input$method ─────────────────
  output$pie_current <- renderPlotly({
    req(results())
    df <- results()$df
    make_pie(
      df$Asset, df$Current_Value,
      paste0("Current  ($", format(round(results()$x), big.mark = ","), ")")
    )
  })

  output$pie_rebalanced <- renderPlotly({
    req(results())
    df   <- results()$df
    meta <- method_info[[input$method]]
    make_pie(
      df$Asset, df[[meta$value_col]],
      paste0("Rebalanced  ($", format(round(results()$xf), big.mark = ","), ")")
    )
  })

  # ── Current table — Asset, Ticker, Value, Current Weight, Target Weight ────
  output$table_current <- DT::renderDT({
    req(results())
    df <- results()$df

    df |>
      mutate("Current Weight" = Current_Value / results()$x) |>
      select(
        Asset,
        Ticker,
        "Value"          = Current_Value,
        "Current Weight",
        "Target Weight"  = Target_Percent
      ) |>
      fmt_dt(
        currency_cols = "Value",
        pct_cols      = c("Current Weight", "Target Weight")
      )
  })

  # ── Rebalanced table — columns differ by method ────────────────────────────
  output$table_rebalanced <- DT::renderDT({
    req(results())
    df   <- results()$df
    meta <- method_info[[input$method]]

    if (input$method == "sell") {
      # Full rebalance: show direction (Buy / Sell / Hold) and target value
      df |>
        mutate(
          Direction      = case_when(
            Naive_Adj > 0 ~ "Buy",
            Naive_Adj < 0 ~ "Sell",
            TRUE          ~ "Hold"
          ),
          "Final Weight" = Target_Value / results()$xf
        ) |>
        select(
          Asset,
          Ticker,
          "Target Value"  = Target_Value,
          "Final Weight",
          "Target Weight" = Target_Percent,
          "Buy / Sell"    = Naive_Adj,
          Direction
        ) |>
        fmt_dt(
          currency_cols = c("Target Value", "Buy / Sell"),
          pct_cols      = c("Final Weight", "Target Weight")
        )
    } else {
      # ℓ₁ or ℓ₂: show amount bought, resulting value, and resulting allocation
      df |>
        mutate("Final Weight" = .data[[meta$pct_col]]) |>
        select(
          Asset,
          Ticker,
          "Value After"   = !!sym(meta$value_col),
          "Final Weight",
          "Target Weight" = Target_Percent,
          "Amount Bought" = !!sym(meta$buy_col)
        ) |>
        fmt_dt(
          currency_cols = c("Value After", "Amount Bought"),
          pct_cols      = c("Final Weight", "Target Weight")
        )
    }
  })

  # ── Totals footers
  # ── Totals footers — mirrors Python app's "Total positions / Cash" lines ───
  output$totals_current <- renderUI({
    req(results())
    total <- sum(results()$df$Current_Value)
    tags$div(
      class = "total-row mt-2",
      tags$strong("Total: "),
      paste0("$", format(round(total, 2), big.mark = ","))
    )
  })

  output$totals_rebalanced <- renderUI({
    req(results())
    df          <- results()$df
    meta        <- method_info[[input$method]]
    total_after <- sum(df[[meta$value_col]])
    funds_used  <- sum(df[[meta$buy_col]])   # total $ deployed (buy-only methods will always equal y)

    tags$div(
      class = "total-row mt-2",
      tags$strong("Total: "),    paste0("$", format(round(total_after, 2), big.mark = ",")),
      tags$br(),
      tags$strong("Invested: "), paste0("$", format(round(funds_used,  2), big.mark = ","))
    )
  })
}

shinyApp(ui = ui, server = server)