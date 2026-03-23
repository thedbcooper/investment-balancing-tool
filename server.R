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

  # observeEvent(input$nav, {
  #   toggle_sidebar(
  #     id   = "sidebar",
  #     open = !(input$nav == "About")
  #   )
  # })

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

  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("portfolio_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write_csv(values$data, file)
    }
  )

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

  output$buy_orders_boxes <- renderUI({
    req(results())
    meta <- method_info[[input$method]]
    df   <- results()$df

    buys <- df[[meta$buy_col]]
    buying <- df[buys > 0.005, ]

    total_invested <- sum(buys)
    n_assets       <- sum(buys > 0.005)
    biggest_buy    <- if (nrow(buying) > 0) max(buying[[meta$buy_col]]) else 0
    biggest_ticker <- if (nrow(buying) > 0) buying$Ticker[which.max(buying[[meta$buy_col]])] else "—"

    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(
        title    = "Total to Invest",
        value    = paste0("$", format(round(total_invested, 2), big.mark = ",")),
        showcase = bs_icon("cash-stack"),
        theme    = "primary"
      ),
      value_box(
        title    = "Assets Being Bought",
        value    = n_assets,
        showcase = bs_icon("cart-check"),
        theme    = "success"
      ),
      value_box(
        title    = paste0("Largest Purchase (", biggest_ticker, ")"),
        value    = paste0("$", format(round(biggest_buy, 2), big.mark = ",")),
        showcase = bs_icon("graph-up-arrow"),
        theme    = "info"
      )
    )
  })

  output$buy_bar_chart <- renderPlotly({
    req(results())
    meta <- method_info[[input$method]]
    df   <- results()$df

    plot_df <- df |>
      filter(.data[[meta$buy_col]] > 0.005) |>
      mutate(
        Label       = if_else(Ticker != "", paste0(Ticker, " — ", Asset), Asset),
        Amount      = .data[[meta$buy_col]],
        HoverText   = paste0(
          "<b>", Asset, "</b> (", Ticker, ")<br>",
          "Group: ", Group, "<br>",
          "Buy: $", format(round(Amount, 2), big.mark = ","), "<br>",
          "New value: $", format(round(.data[[meta$value_col]], 2), big.mark = ",")
        )
      ) |>
      arrange(Amount)

    n_groups <- n_distinct(plot_df$Group)
    palette  <- RColorBrewer::brewer.pal(max(3, min(n_groups, 9)), "Set2")
    groups   <- unique(plot_df$Group)
    color_map <- setNames(palette[seq_along(groups)], groups)

    plot_ly(
      data        = plot_df,
      x           = ~Amount,
      y           = ~reorder(Label, Amount),
      color       = ~Group,
      colors      = color_map,
      type        = "bar",
      orientation = "h",
      text        = ~paste0("$", format(round(Amount, 0), big.mark = ",")),
      textposition = "outside",
      hovertext   = ~HoverText,
      hoverinfo   = "text"
    ) |>
      layout(
        xaxis      = list(title = "Amount to Buy ($)", tickprefix = "$", tickformat = ",.0f"),
        yaxis      = list(title = "", automargin = TRUE),
        showlegend = TRUE,
        legend     = list(title = list(text = "<b>Group</b>"), orientation = "v"),
        margin     = list(l = 10, r = 80, t = 10, b = 50),
        barmode    = "overlay"
      ) |>
      config(displayModeBar = FALSE)
  })

  output$buy_receipt_table <- DT::renderDT({
    req(results())
    meta <- method_info[[input$method]]
    df   <- results()$df

    df |>
      filter(.data[[meta$buy_col]] > 0.005) |>
      transmute(
        Group,
        Asset,
        Ticker,
        `Amount to Buy`  = .data[[meta$buy_col]],
        `Value After`    = .data[[meta$value_col]],
        `New Weight`     = .data[[meta$pct_col]]
      ) |>
      arrange(desc(`Amount to Buy`)) |>
      fmt_dt(
        currency_cols = c("Amount to Buy", "Value After"),
        pct_cols      = "New Weight"
      )
  })
}
