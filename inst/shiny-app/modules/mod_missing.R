# ============================================================
# Module: Missing Data Analysis
# ============================================================

mod_missing_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    # Summary table
    box(
      title = "Missing Data Summary", width = 12, status = "primary",
      solidHeader = TRUE,
      DT::dataTableOutput(ns("miss_table"))
    ),

    # Visualisations row
    column(
      width = 6,
      box(
        title = "Missing Values per Variable", width = 12,
        plotOutput(ns("miss_bar"), height = "350px")
      )
    ),
    column(
      width = 6,
      box(
        title = "Missingness Heatmap", width = 12,
        plotOutput(ns("miss_heatmap"), height = "350px")
      )
    ),

    # Pattern analysis
    box(
      title = "Missingness Patterns", width = 6,
      plotOutput(ns("miss_upset"), height = "350px")
    ),

    # Co-missingness
    box(
      title = "Co-Missingness Correlation", width = 6,
      plotOutput(ns("co_miss"), height = "350px")
    ),

    # MCAR test
    box(
      title = "Little's MCAR Test", width = 6, status = "warning",
      solidHeader = TRUE,
      verbatimTextOutput(ns("mcar_result"))
    ),

    # Does it matter?
    box(
      title = "Does Missingness Matter?", width = 6, status = "danger",
      solidHeader = TRUE,
      DT::dataTableOutput(ns("impact_table"))
    ),

    # Imputation suggestions
    box(
      title = "Suggested Imputation Strategies", width = 12, status = "info",
      solidHeader = TRUE,
      DT::dataTableOutput(ns("imputation_table"))
    )
  )
}


mod_missing_server <- function(id, user_data) {
  moduleServer(id, function(input, output, session) {

    analysis <- reactive({
      req(user_data())
      missing_pattern_analysis(user_data(), test_mcar = TRUE)
    })

    # Summary table
    output$miss_table <- DT::renderDataTable({
      req(analysis())
      df <- analysis()$summary
      df$severity <- sprintf('<span class="severity-%s">%s</span>', df$severity, df$severity)

      DT::datatable(df, escape = FALSE, options = list(pageLength = 20),
                    class = "compact stripe hover") |>
        DT::formatRound("pct_missing", 1)
    })

    # Bar chart
    output$miss_bar <- renderPlot({
      req(user_data())
      naniar::gg_miss_var(user_data()) +
        theme_minimal() +
        labs(x = "Variable", y = "Number missing")
    })

    # Heatmap
    output$miss_heatmap <- renderPlot({
      req(user_data())
      visdat::vis_miss(user_data(), cluster = TRUE) +
        theme(text = element_text(size = 10))
    })

    # Upset plot (patterns)
    output$miss_upset <- renderPlot({
      req(user_data())
      cols_with_na <- names(user_data())[colSums(is.na(user_data())) > 0]
      if (length(cols_with_na) >= 2) {
        naniar::gg_miss_upset(user_data(), nsets = min(8, length(cols_with_na)))
      } else {
        plot.new()
        text(0.5, 0.5, "Need >= 2 variables with missing data\nto show patterns", cex = 1.2)
      }
    })

    # Co-missingness
    output$co_miss <- renderPlot({
      req(analysis())
      co <- analysis()$co_missingness
      if (!is.null(co) && nrow(co) >= 2) {
        corrplot::corrplot(co, method = "color", type = "lower",
                          tl.cex = 0.8, tl.col = "black",
                          title = "Pairwise co-missingness", mar = c(0, 0, 2, 0))
      } else {
        plot.new()
        text(0.5, 0.5, "Not enough variables with\nmissing data for co-missingness", cex = 1.2)
      }
    })

    # MCAR test
    output$mcar_result <- renderPrint({
      req(analysis())
      mcar <- analysis()$mcar_test
      if (is.null(mcar)) {
        cat("MCAR test not available for this dataset.")
      } else if ("statistic" %in% names(mcar) || inherits(mcar, "data.frame")) {
        cat(sprintf("Little's MCAR Test\n"))
        cat(sprintf("Chi-squared: %.3f\n", mcar$statistic))
        cat(sprintf("Degrees of freedom: %d\n", mcar$df))
        cat(sprintf("P-value: %.6f\n\n", mcar$p.value))
        if (mcar$p.value < 0.05) {
          cat("INTERPRETATION: p < 0.05 — Reject MCAR hypothesis.\n")
          cat("The data is likely NOT Missing Completely At Random.\n")
          cat("Missingness may depend on observed (MAR) or unobserved (MNAR) values.\n")
          cat("Consider using multiple imputation rather than complete case analysis.")
        } else {
          cat("INTERPRETATION: p >= 0.05 — Cannot reject MCAR.\n")
          cat("No evidence that missingness depends on observed data.\n")
          cat("Complete case analysis or simple imputation may be appropriate.")
        }
      } else {
        cat(mcar$note)
      }
    })

    # Does it matter?
    output$impact_table <- DT::renderDataTable({
      req(analysis())
      df <- analysis()$does_it_matter
      df <- df[df$variable %in% analysis()$summary$variable[analysis()$summary$n_missing > 0], ]

      df$matters <- sprintf('<span class="matters-%s">%s</span>',
                            df$matters, ifelse(df$matters, "YES", "No"))

      DT::datatable(df, escape = FALSE, options = list(pageLength = 15),
                    class = "compact stripe")
    })

    # Imputation suggestions
    output$imputation_table <- DT::renderDataTable({
      req(user_data())
      imp <- suggest_imputation(user_data())
      if (!is.null(imp)) {
        DT::datatable(imp, options = list(pageLength = 15, scrollX = TRUE),
                      class = "compact stripe") |>
          DT::formatRound("pct_missing", 1)
      }
    })
  })
}
