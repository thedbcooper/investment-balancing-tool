server <- function(input, output, session) {

  clean_input_data <- function(df) {
    required <- c("Asset", "Ticker", "Current_Value", "Target_Percent")
    expected <- c("Asset", "Ticker", "Group", "Current_Value", "Target_Percent", "Chosen Weight for Buy")

    df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)

    if (!"Group" %in% names(df) && all(required %in% names(df))) {
      df$Group <- df$Asset
    }

    for (col in setdiff(expected, names(df))) {
      df[[col]] <- NA
    }

    df <- df[, expected, drop = FALSE]
    df$Asset   <- trimws(ifelse(is.na(df$Asset),   "", as.character(df$Asset)))
    df$Ticker  <- trimws(ifelse(is.na(df$Ticker),  "", as.character(df$Ticker)))
    df$Group   <- trimws(ifelse(is.na(df$Group),   "", as.character(df$Group)))
    df$Group   <- ifelse(df$Group == "", df$Asset, df$Group)
    df$Current_Value              <- suppressWarnings(as.numeric(df$Current_Value))
    df$Target_Percent             <- suppressWarnings(as.numeric(df$Target_Percent))
    df[["Chosen Weight for Buy"]] <- suppressWarnings(as.numeric(df[["Chosen Weight for Buy"]]))

    keep_row <-
      df$Asset != "" |
      df$Ticker != "" |
      df$Group != "" |
      !is.na(df$Current_Value) |
      !is.na(df$Target_Percent) |
      !is.na(df[["Chosen Weight for Buy"]])

    df <- df[keep_row, , drop = FALSE]
    df$Current_Value[is.na(df$Current_Value)]   <- 0
    df$Target_Percent[is.na(df$Target_Percent)] <- 0
    rownames(df) <- NULL
    df
  }

  load_starting_data <- function(path = "portfolio.csv") {
    expected <- c("Asset", "Ticker", "Group", "Current_Value", "Target_Percent", "Chosen Weight for Buy")

    if (!file.exists(path)) {
      return(as.data.frame(
        setNames(replicate(length(expected), logical(0), simplify = FALSE), expected),
        check.names = FALSE
      ))
    }

    read_csv(path, show_col_types = FALSE) |>
      clean_input_data()
  }

  values <- reactiveValues(
    data = load_starting_data()
  )

  observeEvent(input$upload_csv, {
    req(input$upload_csv)
    uploaded <- read_csv(input$upload_csv$datapath, show_col_types = FALSE)
    required <- c("Asset", "Ticker", "Current_Value", "Target_Percent")

    if (all(required %in% names(uploaded))) {
      values$data <- clean_input_data(uploaded)
    }
  })

  output$target_warning <- renderUI({
    df <- clean_input_data(values$data)

    if (nrow(df) == 0) {
      return(NULL)
    }

    group_checks <- df |>
      summarize(
        Group_Target_Percent = first_group_target(Target_Percent),
        Distinct_Targets     = n_distinct(round(Target_Percent, 8)),
        .by = Group
      )

    warnings <- character()

    if (any(df[["Chosen Weight for Buy"]] < 0, na.rm = TRUE)) {
      warnings <- c(
        warnings,
        "Chosen Weight for Buy must be non-negative. Leave it blank to use the current within-group weight based on value."
      )
    }

    inconsistent_groups <- group_checks |>
      filter(Distinct_Targets > 1) |>
      pull(Group)

    if (length(inconsistent_groups) > 0) {
      warnings <- c(
        warnings,
        paste0(
          "Groups with inconsistent target values across rows: ",
          paste(inconsistent_groups, collapse = ", "),
          ". Only one target should be used per group."
        )
      )
    }

    pct_sum <- sum(group_checks$Group_Target_Percent, na.rm = TRUE)
    if (abs(pct_sum - 100) > 0.01) {
      warnings <- c(
        warnings,
        paste0("Group target % sum = ", round(pct_sum, 2), "% (must be 100)")
      )
    }

    if (length(warnings) == 0) {
      return(NULL)
    }

    tagList(
      lapply(warnings, function(note) {
        tags$p(class = "warning-note", note)
      })
    )
  })

  output$input_table <- rhandsontable::renderRHandsontable({
    rhandsontable::rhandsontable(
      values$data,
      rowHeaders  = NULL,
      useTypes    = TRUE,
      stretchH    = "all",
      contextMenu = TRUE,
      manualRowMove = TRUE,
      minSpareRows  = 1
    ) |>
      rhandsontable::hot_col("Asset",                  type = "text") |>
      rhandsontable::hot_col("Ticker",                 type = "text") |>
      rhandsontable::hot_col("Group",                  type = "text") |>
      rhandsontable::hot_col("Current_Value",          type = "numeric", format = "0,0.00") |>
      rhandsontable::hot_col("Target_Percent",         type = "numeric", format = "0,0.00") |>
      rhandsontable::hot_col("Chosen Weight for Buy",  type = "numeric", format = "0,0.00")
  })

  observeEvent(input$input_table, {
    values$data <- rhandsontable::hot_to_r(input$input_table) |>
      clean_input_data()
  })

  results <- reactive({
    df <- clean_input_data(values$data)
    req(nrow(df) > 0)

    df$Current_Value              <- as.numeric(df$Current_Value)
    df$Target_Percent             <- as.numeric(df$Target_Percent)
    df[["Chosen Weight for Buy"]] <- as.numeric(df[["Chosen Weight for Buy"]])

    x  <- sum(df$Current_Value)
    y  <- input$new_funds
    xf <- x + y

    group_df <- df |>
      summarize(
        Group_Current_Value  = sum(Current_Value, na.rm = TRUE),
        Group_Target_Percent = first_group_target(Target_Percent) / 100,
        Asset_Count          = dplyr::n(),
        .by = Group
      ) |>
      mutate(
        Group_Naive_Adj = xf * Group_Target_Percent - Group_Current_Value
      )

    zero_target_with_value <- any(
      group_df$Group_Target_Percent <= 0 & group_df$Group_Current_Value > 0
    )

    if (zero_target_with_value) {
      min_buy_no_sell <- Inf
    } else if (any(group_df$Group_Target_Percent > 0)) {
      min_buy_no_sell <- max(
        0,
        max(
          group_df$Group_Current_Value[group_df$Group_Target_Percent > 0] /
            group_df$Group_Target_Percent[group_df$Group_Target_Percent > 0],
          na.rm = TRUE
        ) - x
      )
    } else {
      min_buy_no_sell <- 0
    }

    group_df <- group_df |>
      mutate(
        L1_Group_Buys        = l1_adjustments(Group_Naive_Adj, y),
        L1_Group_Value_After = Group_Current_Value + L1_Group_Buys,
        L1_Group_Final_Pct   = if (xf > 0) L1_Group_Value_After / xf else 0,
        L2_Group_Buys        = l2_adjustments(Group_Naive_Adj, y),
        L2_Group_Value_After = Group_Current_Value + L2_Group_Buys,
        L2_Group_Final_Pct   = if (xf > 0) L2_Group_Value_After / xf else 0,
        Group_Target_Value   = xf * Group_Target_Percent,
        Group_Current_Weight = if (x > 0) Group_Current_Value / x else 0
      )

    pos_sum <- sum(pmax(group_df$Group_Naive_Adj, 0))
    alpha   <- if (y <= pos_sum && pos_sum > 0) y / pos_sum else NA_real_

    d_sorted    <- sort(group_df$Group_Naive_Adj, decreasing = TRUE)
    n           <- length(d_sorted)
    k_star      <- 1
    for (k in seq_len(n)) {
      if (sum(d_sorted[seq_len(k)] - d_sorted[k]) < y) k_star <- k
    }
    lambda_star <- (sum(d_sorted[seq_len(k_star)]) - y) / k_star

    asset_df <- df |>
      left_join(group_df, by = "Group") |>
      mutate(
        Current_Weight = if (x > 0) Current_Value / x else 0,
        Within_Group_Weight = dplyr::if_else(
          Group_Current_Value > 0,
          Current_Value / Group_Current_Value,
          1 / Asset_Count
        ),
        Buy_Weight_Input = dplyr::if_else(
          !is.na(.data[["Chosen Weight for Buy"]]),
          pmax(.data[["Chosen Weight for Buy"]], 0),
          Within_Group_Weight * 100
        )
      ) |>
      mutate(
        Buy_Weight_Total = sum(Buy_Weight_Input, na.rm = TRUE),
        .by = Group
      ) |>
      mutate(
        Buy_Weight = dplyr::if_else(
          Buy_Weight_Total > 0,
          Buy_Weight_Input / Buy_Weight_Total,
          1 / Asset_Count
        ),
        Naive_Adj      = Group_Naive_Adj * Buy_Weight,
        L1_Buys        = L1_Group_Buys * Buy_Weight,
        L2_Buys        = L2_Group_Buys * Buy_Weight,
        Target_Value   = Group_Target_Value * Buy_Weight,
        L1_Value_After = Current_Value + L1_Buys,
        L2_Value_After = Current_Value + L2_Buys,
        Final_Pct      = if (xf > 0) Target_Value / xf else 0,
        L1_Final_Pct   = if (xf > 0) L1_Value_After / xf else 0,
        L2_Final_Pct   = if (xf > 0) L2_Value_After / xf else 0
      )

    list(
      df       = asset_df,
      group_df = group_df,
      alpha    = alpha,
      lambda   = lambda_star,
      min_buy  = min_buy_no_sell,
      x        = x,
      xf       = xf,
      y        = y
    )
  })

  output$min_buy_sidebar <- renderUI({
    req(results())

    if (is.infinite(results()$min_buy)) {
      return(
        tags$div(
          class = "warning-note",
          "Min buy is not available when a group with a positive current value has a 0% target."
        )
      )
    }

    actionButton(
      "use_min_buy",
      label = HTML(paste0(
        '<span class="min-buy-label">Min Buy to Avoid Selling</span>',
        '<span class="min-buy-value">$', format(round(results()$min_buy, 2), big.mark = ","), "</span>"
      )),
      class = "min-buy-btn"
    )
  })

  observeEvent(input$use_min_buy, {
    updateNumericInput(session, "new_funds", value = results()$min_buy)
  })

  output$method_header <- renderUI({
    meta <- method_info[[input$method]]
    tagList(
      tags$h5(meta$title, class = "mt-3 mb-1 fw-semibold"),
      tags$p(meta$description, class = "method-desc")
    )
  })

  output$pie_current <- renderPlotly({
    req(results())
    df_group <- results()$group_df

    make_pie(
      df_group$Group,
      df_group$Group_Current_Value,
      paste0("Current Groups  ($", format(round(results()$x), big.mark = ","), ")")
    )
  })

  output$pie_rebalanced <- renderPlotly({
    req(results())
    df_group <- results()$group_df
    meta     <- method_info[[input$method]]

    make_pie(
      df_group$Group,
      df_group[[meta$group_value_col]],
      paste0("Rebalanced Groups  ($", format(round(results()$xf), big.mark = ","), ")")
    )
  })

  output$table_current <- DT::renderDT({
    req(results())
    df <- results()$df

    df |>
      transmute(
        Group,
        Asset,
        Ticker,
        Value                  = Current_Value,
        `Portfolio Weight`     = Current_Weight,
        `Weight in Group`      = Within_Group_Weight,
        `Group Current Weight` = Group_Current_Weight
      ) |>
      fmt_dt(
        currency_cols = "Value",
        pct_cols      = c("Portfolio Weight", "Weight in Group", "Group Current Weight")
      )
  })

  output$table_rebalanced <- DT::renderDT({
    req(results())
    meta <- method_info[[input$method]]

    results()$df |>
      transmute(
        Group,
        Asset,
        Ticker,
        `Value After`         = !!sym(meta$value_col),
        `Group Final Weight`  = .data[[meta$group_pct_col]],
        `Group Target Weight` = Group_Target_Percent,
        `Amount Bought`       = !!sym(meta$buy_col)
      ) |>
      fmt_dt(
        currency_cols = c("Value After", "Amount Bought"),
        pct_cols      = c("Group Final Weight", "Group Target Weight")
      )
  })

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

    if (input$method == "sell") {
      net_trade   <- sum(df$Naive_Adj)
      gross_trade <- sum(abs(df$Naive_Adj))

      return(
        tags$div(
          class = "total-row",
          tags$strong("Total: "),       paste0("$", format(round(total_after, 2), big.mark = ",")),
          tags$br(),
          tags$strong("Net Buy: "),     paste0("$", format(round(net_trade,   2), big.mark = ",")),
          tags$br(),
          tags$strong("Gross Trades: "), paste0("$", format(round(gross_trade, 2), big.mark = ","))
        )
      )
    }

    funds_used <- sum(df[[meta$buy_col]])

    tags$div(
      class = "total-row",
      tags$strong("Total: "),    paste0("$", format(round(total_after, 2), big.mark = ",")),
      tags$br(),
      tags$strong("Invested: "), paste0("$", format(round(funds_used,  2), big.mark = ","))
    )
  })
}
