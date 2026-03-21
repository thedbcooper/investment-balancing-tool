library(shiny)
library(DT)
library(tidyverse)

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

  lambda_star    <- (sum(d[1:k_star]) - y) / k_star
  y_star_sorted  <- pmax(d - lambda_star, 0)

  y_star        <- numeric(n)
  y_star[ord]   <- y_star_sorted
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

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  titlePanel("Portfolio Rebalancing Tool"),

  sidebarLayout(
    sidebarPanel(
      fileInput("upload_csv", "Upload CSV",
                accept = ".csv",
                placeholder = "Asset, Ticker, Current_Value, Target_Percent"),
      numericInput("new_funds", "Total New Buy Amount ($):", value = 3000, min = 0),
      helpText("Enter your assets below. Target % should sum to 100."),
      uiOutput("target_warning"),
      hr(),
      actionButton("add_row", "Add Asset"),
      actionButton("remove_row", "Remove Last Asset"),
      br(), br(),
      actionButton("calculate", "Calculate Rebalance", class = "btn-primary")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Input Data",
                 DTOutput("input_table")),

        tabPanel("Rebalance Analysis",
                 fluidRow(
                   column(4, wellPanel(h5("ℓ₁ Deflation Factor (α)"),
                                       verbatimTextOutput("deflation_val"))),
                   column(4, wellPanel(h5("ℓ₂ Threshold (λ*)"),
                                       verbatimTextOutput("lambda_val"))),
                   column(4, wellPanel(h5("Min Buy to Avoid All Selling"),
                                       verbatimTextOutput("min_buy_val")))
                 ),

                 h4("ℓ₁ Solution — Proportional allocation (Theorem 2)"),
                 helpText("Minimises sum of absolute deviations from targets. Deflates all positive naive adjustments by α = y / Σδᵢ⁺ (case 2), or adds surplus evenly when y > Σδᵢ⁺ (case 1)."),
                 DTOutput("l1_table"),
                 hr(),

                 h4("ℓ₂ Solution — Threshold allocation (Theorem 1)"),
                 helpText("Minimises sum of squared deviations from targets. Adds only to the k* assets with the largest naive adjustments, via threshold λ* = (Σᵢ₌₁ᵏ* δᵢ − y) / k*."),
                 DTOutput("l2_table"),
                 hr(),

                 h4("Full Rebalance (With Selling) — Naive adjustments in full"),
                 helpText("Reference only: the traditional buy-and-sell rebalance to exact targets."),
                 DTOutput("selling_table"))
      )
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  values <- reactiveValues(
    data = data.frame(
      Asset          = c("Total US Stock Market Index", "Total International Stock Index", "Bonds"),
      Ticker         = c("VTI", "VXUS", "BND"),
      Current_Value  = c(40739.2, 13062.5, 7019),
      Target_Percent = c(70, 20, 10),
      stringsAsFactors = FALSE
    )
  )

  # CSV upload
  observeEvent(input$upload_csv, {
    req(input$upload_csv)
    uploaded <- read_csv(input$upload_csv$datapath, show_col_types = FALSE)
    expected <- c("Asset", "Ticker", "Current_Value", "Target_Percent")
    if (all(expected %in% names(uploaded))) {
      values$data <- uploaded |> select(all_of(expected)) |> as.data.frame()
    }
  })

  # Warn if target %s don't sum to 100
  output$target_warning <- renderUI({
    pct_sum <- sum(as.numeric(values$data$Target_Percent), na.rm = TRUE)
    if (abs(pct_sum - 100) > 0.01) {
      tags$p(style = "color:red; font-weight:bold;",
             paste0("⚠ Target % sum = ", round(pct_sum, 2), "% (must be 100)"))
    }
  })

  output$input_table <- renderDT({
    datatable(values$data, editable = TRUE, options = list(dom = "t"))
  })

  observeEvent(input$input_table_cell_edit, {
    info <- input$input_table_cell_edit
    values$data[info$row, info$col] <- info$value
  })

  observeEvent(input$add_row, {
    values$data <- rbind(values$data, list("New Asset", "TICKER", 0, 0))
  })

  observeEvent(input$remove_row, {
    if (nrow(values$data) > 1) values$data <- values$data[-nrow(values$data), ]
  })

  # ── Core calculations ──────────────────────────────────────────────────────
  results <- eventReactive(input$calculate, {
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
    pos_sum     <- sum(pmax(df$Naive_Adj, 0))
    alpha       <- if (y <= pos_sum) y / pos_sum else NA_real_   # NA in case 1

    d_sorted    <- sort(df$Naive_Adj, decreasing = TRUE)
    n           <- length(d_sorted)
    k_star      <- 1
    for (k in 1:n) if (sum(d_sorted[1:k] - d_sorted[k]) < y) k_star <- k
    lambda_star <- (sum(d_sorted[1:k_star]) - y) / k_star

    # Full naive rebalance (reference)
    df$Target_Value <- xf * df$Target_Percent

    list(df = df, alpha = alpha, lambda = lambda_star, min_buy = min_buy_no_sell)
  })

  # ── Outputs ────────────────────────────────────────────────────────────────
  output$deflation_val <- renderText({
    a <- results()$alpha
    if (is.na(a)) "N/A (case 1: surplus)" else round(a, 6)
  })
  output$lambda_val  <- renderText({ paste0("$", format(round(results()$lambda, 2), big.mark = ",")) })
  output$min_buy_val <- renderText({ paste0("$", format(round(results()$min_buy, 2), big.mark = ",")) })

  output$l1_table <- renderDT({
    results()$df |>
      select(Asset, Ticker, Current_Value, Target_Percent, Naive_Adj, L1_Buys, L1_Value_After, L1_Final_Pct) |>
      datatable(options = list(dom = "t")) |>
      formatCurrency(c("Current_Value", "Naive_Adj", "L1_Buys", "L1_Value_After")) |>
      formatPercentage(c("Target_Percent", "L1_Final_Pct"), 2)
  })

  output$l2_table <- renderDT({
    results()$df |>
      select(Asset, Ticker, Current_Value, Target_Percent, Naive_Adj, L2_Buys, L2_Value_After, L2_Final_Pct) |>
      datatable(options = list(dom = "t")) |>
      formatCurrency(c("Current_Value", "Naive_Adj", "L2_Buys", "L2_Value_After")) |>
      formatPercentage(c("Target_Percent", "L2_Final_Pct"), 2)
  })

  output$selling_table <- renderDT({
    results()$df |>
      mutate(Direction = case_when(
        Naive_Adj > 0 ~ "Buy",
        Naive_Adj < 0 ~ "Sell",
        TRUE          ~ "Hold"
      )) |>
      select(Asset, Ticker, Current_Value, Naive_Adj, Target_Value, Direction) |>
      datatable(options = list(dom = "t")) |>
      formatCurrency(c("Current_Value", "Naive_Adj", "Target_Value"))
  })
}

shinyApp(ui = ui, server = server)