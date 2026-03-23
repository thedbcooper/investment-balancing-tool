ui <- page_navbar(
  title = "Portfolio Rebalancing Tool",
  theme = bs_theme(
    bootswatch = "flatly",
    base_font  = font_google("Inter")
  ),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  sidebar = sidebar(
    width = 290,

    fileInput(
      "upload_csv", "Upload CSV",
      accept      = ".csv",
      placeholder = "Asset, Ticker, Group, Current_Value, Target_Percent, Chosen Weight for Buy"
    ),

    numericInput("new_funds", "Total New Buy Amount ($):", value = 3000, min = 0),

    uiOutput("min_buy_sidebar"),

    hr(),

    radioButtons(
      "method", "Rebalancing Method:",
      choices = c(
        "ℓ₁ — Proportional (Theorem 2)" = "l1",
        "ℓ₂ — Threshold (Theorem 1)"    = "l2",
        "Full Rebalance (With Selling)" = "sell"
      ),
      selected = "l1"
    ),

    hr(),

    helpText("Rows in the same group share one target percent. Repeat the same target on each row in that group."),
    helpText("Chosen Weight for Buy is optional and works within each group. Leave it blank to use the current within-group weight based on value."),
    helpText("Edit the asset table directly. Use the table context menu to insert or remove rows."),
    uiOutput("target_warning")
  ),

  nav_panel(
    "Input Data",
    card_body(
      rhandsontable::rHandsontableOutput("input_table"),
      tags$p(
        class = "text-muted small mt-2 mb-0",
        "Double-click to edit cells. Right-click a row to add or remove assets. Pie charts and target checks are now based on groups, not individual assets. Chosen Weight for Buy is used only to split each group's trades across its assets."
      )
    )
  ),

  nav_panel(
    "Rebalance Analysis",
    uiOutput("method_header"),
    layout_columns(
      col_widths = c(6, 6),

      card(
        card_header("Current by Group"),
        plotlyOutput("pie_current", height = "300px"),
        card_body(
          DT::DTOutput("table_current")
        )
      ),

      card(
        card_header("Rebalanced by Group"),
        plotlyOutput("pie_rebalanced", height = "300px"),
        card_body(
          DT::DTOutput("table_rebalanced")
        )
      )
    )
  ),

  nav_panel(
    "About",
    card(
      card_header("About This Tool"),
      card_body(
        tags$h3("About This Tool"),
        tags$p(
          "A Shiny app that calculates how to allocate new investment funds across a portfolio to bring allocations as close as possible to target weights — ",
          tags$strong("without selling any assets"), "."
        ),
        tags$p(
          "Based on: J. Bartroff, \"Rebalance your portfolio without selling\", arXiv:2305.12274. This repository implements the algorithms from that paper; the implementation is original code."
        ),
        tags$p(
          tags$a(
            href = "https://arxiv.org/abs/2305.12274",
            target = "_blank",
            rel = "noopener noreferrer",
            "Read the paper for theory and proofs"
          )
        ),
        tags$h3("How to Use This App"),
        tags$ul(
          tags$li(tags$strong("Input Data Tab:"), " Edit assets, tickers, current values, and target percentages directly in the table. Upload a CSV file with your portfolio data."),
          tags$li(tags$strong("CSV Format:"), " Columns: Asset, Ticker, Group (optional), Current_Value, Target_Percent, Chosen Weight for Buy (optional). Target percentages must sum to 100%."),
          tags$li(tags$strong("Groups:"), " Assign assets to groups for aggregated analysis. Within each group, you can specify how new buys are split."),
          tags$li(tags$strong("New Funds:"), " Enter the total amount of new money to invest. The app shows the minimum amount needed to avoid selling."),
          tags$li(tags$strong("Rebalancing Methods:"), " Choose between ℓ₁ (proportional), ℓ₂ (threshold), or full rebalance (with selling) for comparison."),
          tags$li(tags$strong("Analysis Tab:"), " View current and rebalanced allocations with pie charts and detailed tables.")
        ),
        tags$p(
          "To run locally: Install dependencies and use ", tags$code("shiny::runApp(\"app.R\")"), "."
        ),
        tags$h3("How Does It Work"),
        tags$p(
          "You have a portfolio with current values and target proportions. You want to invest additional funds ", tags$em("y"), " to get as close as possible to targets without selling."
        ),
        tags$p(
          "The ", tags$strong("naive adjustments"), " that would achieve exact targets (if selling were allowed) are: ", tags$code("δᵢ = pᵢ(x + y) - xᵢ"), " where ", tags$code("x"), " is current total value."
        ),
        tags$p(
          "Since some δᵢ may be negative, we use optimal solutions:"
        ),
        tags$ul(
          tags$li(
            tags$strong("ℓ₂ Solution (Theorem 1):"), " Minimizes sum of squared deviations. Funds go to the top k* underweight assets. Best for closing biggest gaps first."
          ),
          tags$li(
            tags$strong("ℓ₁ Solution (Theorem 2):"), " Minimizes sum of absolute deviations. Funds spread proportionally to all underweight assets."
          )
        ),
        tags$p(
          "These closed-form solutions are mathematically proven optimal, fast, and deterministic — unlike heuristic optimizers."
        ),
        tags$h3("Author"),
        tags$h2("Daniel B. Cooper, MPH"),
        tags$p(
          "Data scientist dipping my toes in quantitative finance and optimization."
        ),
        tags$a(
          class = "github-nav-link",
          href = "https://github.com/thedbcooper/",
          target = "_blank",
          rel = "noopener noreferrer",
          bs_icon("github"),
          "GitHub"
        )
      )
    )
  ),

  nav_spacer(),
  nav_item(
    tags$a(
      class = "github-nav-link",
      href = "https://github.com/thedbcooper/investment-balancing-tool",
      target = "_blank",
      rel = "noopener noreferrer",
      bs_icon("github"),
      "GitHub"
    )
  )
)
