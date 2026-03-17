# ============================================================
# Module: Variable Types
# ============================================================

mod_variables_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    # Type detection results with override capability
    box(
      title = "Detected Variable Types (click a row to override)", width = 12,
      status = "primary", solidHeader = TRUE,
      DT::dataTableOutput(ns("type_table"))
    ),

    # Override panel
    box(
      title = "Override Variable Type", width = 6,
      uiOutput(ns("override_ui")),
      actionButton(ns("apply_override"), "Apply Override", icon = icon("check"),
                   class = "btn-warning")
    ),

    # Type distribution
    box(
      title = "Type Distribution", width = 6,
      plotOutput(ns("type_pie"), height = "300px")
    ),

    # Per-type variable lists
    box(
      title = "Variables by Type", width = 12,
      uiOutput(ns("type_groups"))
    )
  )
}


mod_variables_server <- function(id, user_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive: types (can be overridden)
    var_types <- reactiveVal(NULL)

    observeEvent(user_data(), {
      req(user_data())
      var_types(detect_variable_types(user_data()))
    })

    # Type table
    output$type_table <- DT::renderDataTable({
      req(var_types())
      df <- var_types()
      df$detected_type <- sprintf('<span class="type-badge type-%s">%s</span>',
                                  df$detected_type, df$detected_type)

      DT::datatable(
        df, escape = FALSE, selection = "single",
        options = list(pageLength = 25, scrollX = TRUE),
        class = "compact stripe hover"
      )
    })

    # Override UI
    output$override_ui <- renderUI({
      req(var_types())
      sel <- input$type_table_rows_selected
      if (is.null(sel) || length(sel) == 0) {
        return(p("Select a row in the table above to override its type."))
      }

      var_name <- var_types()$variable[sel]
      current <- var_types()$detected_type[sel]

      tagList(
        h4(sprintf("Variable: %s", var_name)),
        p(sprintf("Current type: %s", current)),
        selectInput(ns("new_type"), "New type:",
                    choices = c("numeric_continuous", "numeric_discrete",
                                "categorical_nominal", "categorical_ordinal",
                                "binary", "date_time", "identifier", "free_text"),
                    selected = current)
      )
    })

    # Apply override
    observeEvent(input$apply_override, {
      sel <- input$type_table_rows_selected
      req(sel, input$new_type)
      vt <- var_types()
      var_name <- vt$variable[sel]
      vt <- override_variable_type(vt, var_name, input$new_type)
      var_types(vt)
      showNotification(sprintf("Updated %s -> %s", var_name, input$new_type), type = "message")
    })

    # Pie chart
    output$type_pie <- renderPlot({
      req(var_types())
      type_counts <- as.data.frame(table(var_types()$detected_type))
      names(type_counts) <- c("Type", "Count")

      colours <- c(
        numeric_continuous = "#2c7bb6", numeric_discrete = "#5aa4d9",
        categorical_nominal = "#27ae60", categorical_ordinal = "#16a085",
        binary = "#8e44ad", date_time = "#e67e22",
        identifier = "#95a5a6", free_text = "#7f8c8d"
      )

      ggplot(type_counts, aes(x = "", y = Count, fill = Type)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        scale_fill_manual(values = colours) +
        theme_void() +
        theme(legend.position = "right", legend.text = element_text(size = 11))
    })

    # Variable groups
    output$type_groups <- renderUI({
      req(var_types())
      types <- unique(var_types()$detected_type)

      tagList(lapply(types, function(t) {
        vars <- var_types()$variable[var_types()$detected_type == t]
        tags$div(
          style = "margin-bottom: 10px;",
          tags$strong(sprintf("%s (%d):", t, length(vars))),
          tags$span(paste(vars, collapse = ", "), style = "color: #555;")
        )
      }))
    })
  })
}
