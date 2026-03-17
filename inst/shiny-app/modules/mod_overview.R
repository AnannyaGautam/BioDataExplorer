# ============================================================
# Module: Data Overview
# ============================================================

mod_overview_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    # Info boxes row
    fluidRow(
      infoBoxOutput(ns("box_rows"), width = 3),
      infoBoxOutput(ns("box_cols"), width = 3),
      infoBoxOutput(ns("box_complete"), width = 3),
      infoBoxOutput(ns("box_missing_vars"), width = 3)
    ),

    # Type breakdown + data preview
    column(
      width = 6,
      box(
        title = "Variable Type Breakdown", width = 12, status = "primary",
        plotly::plotlyOutput(ns("type_chart"), height = "300px")
      )
    ),
    column(
      width = 6,
      box(
        title = "Data Structure (visdat)", width = 12, status = "primary",
        plotOutput(ns("visdat_plot"), height = "300px")
      )
    ),

    # Full variable table
    box(
      title = "All Variables", width = 12, status = "primary",
      DT::dataTableOutput(ns("var_table"))
    ),

    # Numeric summary
    box(
      title = "Numeric Variable Summary", width = 12, status = "primary",
      collapsible = TRUE,
      DT::dataTableOutput(ns("numeric_table"))
    )
  )
}


mod_overview_server <- function(id, user_data) {
  moduleServer(id, function(input, output, session) {

    # Reactive: variable types
    var_types <- reactive({
      req(user_data())
      detect_variable_types(user_data())
    })

    miss_df <- reactive({
      req(user_data())
      missing_summary(user_data())
    })

    # Info boxes
    output$box_rows <- renderInfoBox({
      req(user_data())
      infoBox("Rows", nrow(user_data()), icon = icon("list-ol"), color = "blue")
    })

    output$box_cols <- renderInfoBox({
      req(user_data())
      infoBox("Columns", ncol(user_data()), icon = icon("columns"), color = "teal")
    })

    output$box_complete <- renderInfoBox({
      req(user_data())
      total <- nrow(user_data()) * ncol(user_data())
      pct <- round((1 - sum(is.na(user_data())) / total) * 100, 1)
      colour <- if (pct >= 95) "green" else if (pct >= 80) "yellow" else "red"
      infoBox("Completeness", paste0(pct, "%"), icon = icon("check-circle"), color = colour)
    })

    output$box_missing_vars <- renderInfoBox({
      req(miss_df())
      n <- sum(miss_df()$n_missing > 0)
      infoBox("Vars with NA", n, icon = icon("exclamation-triangle"),
              color = if (n == 0) "green" else "orange")
    })

    # Type breakdown chart
    output$type_chart <- plotly::renderPlotly({
      req(var_types())
      type_counts <- as.data.frame(table(var_types()$detected_type))
      names(type_counts) <- c("Type", "Count")

      colours <- c(
        numeric_continuous = "#2c7bb6", numeric_discrete = "#5aa4d9",
        categorical_nominal = "#27ae60", categorical_ordinal = "#16a085",
        binary = "#8e44ad", date_time = "#e67e22",
        identifier = "#95a5a6", free_text = "#7f8c8d"
      )

      p <- ggplot(type_counts, aes(x = reorder(Type, Count), y = Count, fill = Type)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_manual(values = colours, drop = FALSE) +
        labs(x = NULL, y = "Count") +
        theme_minimal() +
        theme(legend.position = "none")

      plotly::ggplotly(p, tooltip = c("y")) |>
        plotly::layout(margin = list(l = 120))
    })

    # Visdat plot
    output$visdat_plot <- renderPlot({
      req(user_data())
      visdat::vis_dat(user_data()) + theme(text = element_text(size = 10))
    })

    # Variable table
    output$var_table <- DT::renderDataTable({
      req(var_types(), miss_df())
      df <- merge(var_types(), miss_df()[, c("variable", "n_missing", "pct_missing", "severity")],
                  by = "variable")

      df$detected_type <- sprintf('<span class="type-badge type-%s">%s</span>',
                                  df$detected_type, df$detected_type)
      df$severity <- sprintf('<span class="severity-%s">%s</span>',
                             df$severity, df$severity)

      DT::datatable(
        df,
        escape = FALSE,
        options = list(scrollX = TRUE, pageLength = 20),
        class = "compact stripe hover"
      )
    })

    # Numeric summary
    output$numeric_table <- DT::renderDataTable({
      req(user_data())
      overview <- data_overview(user_data())
      if (!is.null(overview$numeric_summary)) {
        DT::datatable(
          overview$numeric_summary,
          options = list(scrollX = TRUE, pageLength = 15),
          class = "compact stripe"
        ) |> DT::formatRound(columns = 2:6, digits = 2)
      }
    })
  })
}
