# ============================================================
# Module: Data Upload
# ============================================================

mod_upload_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 8, offset = 2,
      box(
        title = "Upload Your Biological Dataset", width = 12, status = "primary",
        solidHeader = TRUE,

        p("Upload a CSV, TSV, or Excel (.xlsx) file. The app will auto-detect
          variable types, analyse missing data, and help you explore relationships."),

        fileInput(ns("file"), "Choose file",
                  accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls"),
                  width = "100%"),

        fluidRow(
          column(4, selectInput(ns("separator"), "Separator (CSV/TSV)",
                                choices = c("Comma" = ",", "Tab" = "\t", "Semicolon" = ";"),
                                selected = ",")),
          column(4, checkboxInput(ns("header"), "First row is header", value = TRUE)),
          column(4, selectInput(ns("na_strings"), "NA strings",
                                choices = c('NA, ""' = "default", "Custom" = "custom"),
                                selected = "default"))
        ),

        conditionalPanel(
          condition = sprintf("input['%s'] == 'custom'", ns("na_strings")),
          textInput(ns("custom_na"), "Custom NA values (comma-separated)", value = "NA, , ., -9999")
        ),

        hr(),

        h4(icon("flask"), " Or try a demo dataset:"),
        fluidRow(
          column(4, actionButton(ns("demo_iris"), "Iris (general)", icon = icon("leaf"),
                                 class = "btn-info btn-block")),
          column(4, actionButton(ns("demo_clinical"), "Clinical trial", icon = icon("hospital"),
                                 class = "btn-success btn-block")),
          column(4, actionButton(ns("demo_microbiome"), "Microbiome", icon = icon("bacteria"),
                                 class = "btn-warning btn-block"))
        )
      ),

      # Preview
      box(
        title = "Data Preview", width = 12, collapsible = TRUE, collapsed = TRUE,
        DT::dataTableOutput(ns("preview"))
      )
    )
  )
}


mod_upload_server <- function(id, user_data) {
  moduleServer(id, function(input, output, session) {

    # File upload
    observeEvent(input$file, {
      req(input$file)
      ext <- tools::file_ext(input$file$name)

      na_vals <- if (input$na_strings == "default") c("NA", "") else {
        trimws(strsplit(input$custom_na, ",")[[1]])
      }

      tryCatch({
        df <- switch(ext,
          csv = readr::read_csv(input$file$datapath, na = na_vals,
                                show_col_types = FALSE),
          tsv = readr::read_tsv(input$file$datapath, na = na_vals,
                                show_col_types = FALSE),
          txt = readr::read_delim(input$file$datapath, delim = input$separator,
                                  na = na_vals, show_col_types = FALSE),
          xlsx = readxl::read_excel(input$file$datapath, na = na_vals),
          xls = readxl::read_excel(input$file$datapath, na = na_vals),
          { showNotification("Unsupported file format", type = "error"); NULL }
        )
        if (!is.null(df)) {
          user_data(as.data.frame(df))
          showNotification(
            sprintf("Loaded %d rows x %d columns", nrow(df), ncol(df)),
            type = "message"
          )
        }
      }, error = function(e) {
        showNotification(paste("Error reading file:", e$message), type = "error")
      })
    })

    # Demo datasets
    observeEvent(input$demo_iris, {
      user_data(datasets::iris)
      showNotification("Loaded: Iris dataset (150 x 5)", type = "message")
    })

    observeEvent(input$demo_clinical, {
      set.seed(42)
      n <- 200
      df <- data.frame(
        patient_id = paste0("PT-", sprintf("%04d", 1:n)),
        age = round(rnorm(n, 55, 12)),
        sex = sample(c("Male", "Female"), n, replace = TRUE),
        bmi = round(rnorm(n, 27, 5), 1),
        treatment = sample(c("Drug A", "Drug B", "Placebo"), n, replace = TRUE),
        disease_stage = sample(c("Stage 1", "Stage 2", "Stage 3", "Stage 4"), n,
                               replace = TRUE, prob = c(0.3, 0.35, 0.25, 0.1)),
        biomarker_1 = round(rlnorm(n, 2, 0.8), 2),
        biomarker_2 = round(rnorm(n, 100, 25), 1),
        response = sample(c("Complete", "Partial", "None"), n, replace = TRUE,
                          prob = c(0.25, 0.4, 0.35)),
        survival_months = round(rexp(n, 0.03), 1),
        stringsAsFactors = FALSE
      )
      # Inject realistic missingness
      df$bmi[sample(n, 15)] <- NA
      df$biomarker_1[sample(n, 30)] <- NA
      df$biomarker_2[sample(n, 8)] <- NA
      df$disease_stage[sample(n, 5)] <- NA
      # MNAR: biomarker_1 more likely missing for Stage 4
      stage4 <- which(df$disease_stage == "Stage 4")
      df$biomarker_1[sample(stage4, min(10, length(stage4)))] <- NA

      user_data(df)
      showNotification("Loaded: Simulated clinical trial (200 x 10)", type = "message")
    })

    observeEvent(input$demo_microbiome, {
      set.seed(123)
      n <- 80
      taxa <- paste0("Taxon_", 1:20)
      counts <- matrix(rnbinom(n * 20, mu = 50, size = 2), nrow = n, ncol = 20)
      colnames(counts) <- taxa
      meta <- data.frame(
        sample_id = paste0("S", sprintf("%03d", 1:n)),
        group = sample(c("Healthy", "Disease"), n, replace = TRUE),
        site = sample(c("Gut", "Oral", "Skin"), n, replace = TRUE),
        shannon_diversity = round(rnorm(n, 3.5, 0.8), 2),
        age = round(rnorm(n, 40, 15)),
        antibiotics = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.3, 0.7)),
        stringsAsFactors = FALSE
      )
      df <- cbind(meta, as.data.frame(counts))
      # Inject missingness
      df$shannon_diversity[sample(n, 6)] <- NA
      df$age[sample(n, 4)] <- NA
      df$antibiotics[sample(n, 3)] <- NA

      user_data(df)
      showNotification("Loaded: Simulated microbiome data (80 x 26)", type = "message")
    })

    # Preview table
    output$preview <- DT::renderDataTable({
      req(user_data())
      DT::datatable(
        head(user_data(), 50),
        options = list(scrollX = TRUE, pageLength = 10),
        class = "compact stripe"
      )
    })
  })
}
