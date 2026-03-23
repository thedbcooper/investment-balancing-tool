ui <- page_navbar(
  title = tagList(
    bs_icon("bar-chart-steps", class = "me-2 text-primary"),
    "Portfolio Rebalancing Tool"
  ),
  window_title = "Portfolio Rebalancing Tool",
  theme = bs_theme(
    bootswatch  = "flatly",
    base_font   = font_google("Inter"),
    heading_font = font_google("Inter"),
    primary     = "#2C6BAC",
    success     = "#1fbb9c",
    "navbar-bg" = "#1a2f4a",
    "navbar-dark" = TRUE
  ),

  selected = "About",

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  sidebar = sidebar(
    width = 300,
    class = "sidebar-panel",

    # ── Upload ─────────────────────────────────────────────────────────────
    div(
      class = "sidebar-section",
      div(class = "sidebar-section-label", bs_icon("upload", class = "me-1"), "Portfolio Data"),
      fileInput(
        "upload_csv", NULL,
        accept      = ".csv",
        placeholder = "Upload CSV…",
        buttonLabel = tagList(bs_icon("folder2-open"), " Browse")
      )
    ),

    # ── Funds ──────────────────────────────────────────────────────────────
    div(
      class = "sidebar-section",
      div(class = "sidebar-section-label", bs_icon("currency-dollar", class = "me-1"), "New Investment"),
      numericInput("new_funds", NULL, value = 3000, min = 0),
      uiOutput("min_buy_sidebar")
    ),

    # ── Method ────────────────────────────────────────────────────────────
    div(
      class = "sidebar-section",
      div(class = "sidebar-section-label", bs_icon("sliders", class = "me-1"), "Rebalancing Method"),
      radioButtons(
        "method", NULL,
        choices = c(
          "ℓ₁ — Proportional (Theorem 2)" = "l1",
          "ℓ₂ — Threshold (Theorem 1)"    = "l2",
          "Full Rebalance (With Selling)"  = "sell"
        ),
        selected = "l1"
      )
    ),

    # ── Help & warnings ───────────────────────────────────────────────────
    div(
      class = "sidebar-section sidebar-help",
      helpText(bs_icon("people-fill", class = "me-1"), "Rows in the same group share one target percent."),
      helpText(bs_icon("bar-chart-line", class = "me-1"), "Chosen Weight for Buy splits group buys across assets. Leave blank to weight by current value."),
      helpText(bs_icon("table", class = "me-1"), "Edit the asset table directly. Right-click a row to insert or remove assets.")
    ),

    uiOutput("target_warning")
  ),

  # ── Tabs ───────────────────────────────────────────────────────────────────

  nav_panel(
    title = tagList(bs_icon("table", class = "me-1"), "Input Data"),
    value = "Input Data",
    card(
      class = "mt-3",
      card_header(
        class = "d-flex align-items-center justify-content-between",
        tagList(bs_icon("pencil-square", class = "me-2 text-primary"), "Portfolio Assets"),
        downloadButton("download_csv", tagList(bs_icon("download", class = "me-1"), "Export CSV"),
                       class = "btn-sm btn-outline-secondary")
      ),
      card_body(
        rhandsontable::rHandsontableOutput("input_table"),
        tags$p(
          class = "text-muted small mt-2 mb-0",
          "Double-click to edit cells. Right-click a row to add or remove assets. ",
          "Pie charts and target checks are based on groups. ",
          "Chosen Weight for Buy splits each group's trades across its assets."
        )
      )
    )
  ),

  nav_panel(
    title = tagList(bs_icon("cart-check-fill", class = "me-1"), "Buy Orders"),
    value = "Buy Orders",
    uiOutput("buy_orders_boxes"),
    card(
      class = "mt-2",
      card_header(
        tagList(bs_icon("bar-chart-fill", class = "me-2 text-primary"), "What to Buy")
      ),
      plotlyOutput("buy_bar_chart", height = "auto", width = "100%")
    ),
    card(
      class = "mt-2",
      card_header(
        tagList(bs_icon("receipt", class = "me-2 text-success"), "Purchase Breakdown")
      ),
      card_body(
        tags$p(
          class = "text-muted small mb-2",
          "Only assets receiving a non-zero allocation are shown. Sorted by purchase size."
        ),
        DT::DTOutput("buy_receipt_table")
      )
    )
  ),

  nav_panel(
    title = tagList(bs_icon("pie-chart-fill", class = "me-1"), "Rebalance Analysis"),
    value = "Rebalance Analysis",
    uiOutput("method_header"),
    layout_columns(
      col_widths = c(6, 6),

      card(
        card_header(
          tagList(bs_icon("clock-history", class = "me-2 text-secondary"), "Current Allocation")
        ),
        plotlyOutput("pie_current", height = "300px"),
        card_body(DT::DTOutput("table_current"))
      ),

      card(
        card_header(
          tagList(bs_icon("check2-circle", class = "me-2 text-success"), "After Rebalancing")
        ),
        plotlyOutput("pie_rebalanced", height = "300px"),
        card_body(DT::DTOutput("table_rebalanced"))
      )
    )
  ),

  nav_panel(
    title = tagList(bs_icon("info-circle", class = "me-1"), "About"),
    value = "About",
    layout_columns(
      col_widths = c(4, 8),

      # --- Author card (left column) ---
      card(
        class = "text-center author-card",
        card_body(
          bs_icon("person-circle", size = "3.5rem", class = "text-primary mb-2"),
          tags$h3("Daniel B. Cooper, MPH", class = "mt-1 mb-1"),
          tags$p(
            class = "text-muted",
            "Data scientist dipping my toes in quantitative finance and optimization."
          ),
          tags$a(
            class = "btn btn-outline-secondary btn-sm mt-1",
            href = "https://github.com/thedbcooper/",
            target = "_blank",
            rel = "noopener noreferrer",
            bs_icon("github"), " GitHub"
          )
        )
      ),

      # --- Right column: About, How to Use, How It Works ---
      layout_columns(
        col_widths = 12,

        card(
          card_header(
            bs_icon("info-circle-fill", class = "text-primary me-2"),
            "About This Tool"
          ),
          card_body(
            tags$p(
              "A Shiny app that calculates how to allocate new investment funds across a portfolio to bring allocations as close as possible to target weights — ",
              tags$strong("without selling any assets"), "."
            ),
            tags$p(
              "Based on: J. Bartroff, \"Rebalance your portfolio without selling\", arXiv:2305.12274.",
              " This repository implements the algorithms from that paper; the implementation is original code."
            ),
            tags$a(
              href = "https://arxiv.org/abs/2305.12274",
              target = "_blank",
              rel = "noopener noreferrer",
              bs_icon("journal-text", class = "me-1"),
              "Read the paper for theory and proofs"
            )
          )
        ),

        card(
          card_header(
            bs_icon("map", class = "text-success me-2"),
            "How to Use This App"
          ),
          card_body(
            tags$ul(
              class = "mb-0",
              tags$li(tags$strong("Input Data Tab:"), " Edit assets, tickers, current values, and target percentages directly in the table. Upload a CSV file with your portfolio data."),
              tags$li(tags$strong("CSV Format:"), " Columns: Asset, Ticker, Group (optional), Current_Value, Target_Percent, Chosen Weight for Buy (optional). Target percentages must sum to 100%."),
              tags$li(tags$strong("Groups:"), " Assign assets to groups for aggregated analysis. Within each group, you can specify how new buys are split."),
              tags$li(tags$strong("New Funds:"), " Enter the total amount of new money to invest. The app shows the minimum amount needed to avoid selling."),
              tags$li(tags$strong("Rebalancing Methods:"), " Choose between ℓ₁ (proportional), ℓ₂ (threshold), or full rebalance (with selling) for comparison."),
              tags$li(tags$strong("Analysis Tab:"), " View current and rebalanced allocations with pie charts and detailed tables.")
            )
          )
        ),

        card(
          card_header(
            bs_icon("calculator-fill", class = "text-warning me-2"),
            "How Does It Work"
          ),
          card_body(
            tags$p(
              "You have a portfolio with current values and target proportions. You want to invest additional funds ",
              tags$em("y"), " to get as close as possible to targets without selling."
            ),
            tags$p(
              "The ", tags$strong("naive adjustments"), " that would achieve exact targets (if selling were allowed) are: ",
              tags$code("δᵢ = pᵢ(x + y) − xᵢ"), " where ", tags$code("x"), " is the current total value."
            ),
            tags$p("Since some δᵢ may be negative, we use optimal solutions:"),
            tags$ul(
              class = "mb-0",
              tags$li(
                tags$strong("ℓ₂ Solution (Theorem 1):"),
                " Minimizes sum of squared deviations. Funds go to the top k* underweight assets. Best for closing biggest gaps first."
              ),
              tags$li(
                tags$strong("ℓ₁ Solution (Theorem 2):"),
                " Minimizes sum of absolute deviations. Funds spread proportionally to all underweight assets."
              )
            ),
            tags$p(
              class = "mt-2 mb-0",
              "These closed-form solutions are mathematically proven optimal, fast, and deterministic — unlike heuristic optimizers."
            )
          )
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
