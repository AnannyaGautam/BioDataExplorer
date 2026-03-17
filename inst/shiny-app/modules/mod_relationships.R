# ============================================================
# Module: Variable Relationships
# ============================================================

mod_relationships_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    # Variable selection
    box(
      title = "Explore Variable Relationships", width = 12,
      status = "primary", solidHeader = TRUE,
      fluidRow(
        column(4, selectInput(ns("var1"), "Variable 1:", choices = NULL)),
        column(4, selectInput(ns("var2"), "Variable 2:", choices = NULL)),
        column(4, br(), actionButton(ns("analyse"), "Analyse", icon = icon("chart-line"),
                                     class = "btn-primary btn-lg", style = "margin-top: 5px;"))
      )
    ),

    # Results
    box(
      title = "Relationship Analysis", width = 12,
      fluidRow(
        column(7, plotly::plotlyOutput(ns("rel_plot"), height = "400px")),
        column(5,
               h4("Test Results"),
               verbatimTextOutput(ns("test_output")),
               hr(),
               h4("Interpretation"),
               uiOutput(ns("interpretation"))
        )
      )
    ),

    # Correlation matrix (all numeric)
    box(
      title = "Full Correlation Matrix (numeric variables)", width = 12,
      collapsible = TRUE, collapsed = TRUE,
      plotOutput(ns("corr_matrix"), height = "500px")
    )
  )
}


mod_relationships_server <- function(id, user_data) {
  moduleServer(id, function(input, output, session) {

    var_types <- reactive({
      req(user_data())
      detect_variable_types(user_data())
    })

    # Update variable choices (exclude identifiers and free text)
    observe({
      req(var_types())
      usable <- var_types()$variable[!var_types()$detected_type %in% c("identifier", "free_text")]
      updateSelectInput(session, "var1", choices = usable, selected = usable[1])
      updateSelectInput(session, "var2", choices = usable, selected = usable[min(2, length(usable))])
    })

    # Run analysis
    result <- eventReactive(input$analyse, {
      req(user_data(), input$var1, input$var2)
      if (input$var1 == input$var2) {
        showNotification("Please select two different variables", type = "warning")
        return(NULL)
      }
      variable_relationship(user_data(), input$var1, input$var2, var_types())
    })

    # Plot
    output$rel_plot <- plotly::renderPlotly({
      req(result())
      if (!is.null(result()$plot)) {
        plotly::ggplotly(result()$plot)
      }
    })

    # Test output
    output$test_output <- renderPrint({
      req(result())
      cat(sprintf("Test: %s\n", result()$test_name))
      cat(sprintf("N used: %d\n", result()$n_used))
      if (!is.null(result()$effect_size)) {
        cat(sprintf("Effect size: %.4f\n", result()$effect_size))
      }
    })

    # Interpretation
    output$interpretation <- renderUI({
      req(result())
      tags$div(
        style = "background: #f0f7ff; padding: 12px; border-radius: 6px; border-left: 4px solid #2c7bb6;",
        tags$p(result()$interpretation)
      )
    })

    # Correlation matrix
    output$corr_matrix <- renderPlot({
      req(user_data(), var_types())
      numeric_vars <- var_types()$variable[var_types()$detected_type %in%
                                             c("numeric_continuous", "numeric_discrete")]
      if (length(numeric_vars) >= 2) {
        num_data <- user_data()[, numeric_vars, drop = FALSE]
        cor_mat <- cor(num_data, use = "pairwise.complete.obs")
        corrplot::corrplot(
          cor_mat, method = "color", type = "lower",
          order = "hclust", tl.cex = 0.8, tl.col = "black",
          addCoef.col = "black", number.cex = 0.7
        )
      } else {
        plot.new()
        text(0.5, 0.5, "Need >= 2 numeric variables\nfor a correlation matrix", cex = 1.2)
      }
    })
  })
}
