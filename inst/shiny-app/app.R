# ============================================================
# BioData Explorer — Interactive Shiny Dashboard
# ============================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(naniar)
library(visdat)
library(readr)
library(readxl)

# Source modules
source("modules/mod_upload.R")
source("modules/mod_overview.R")
source("modules/mod_missing.R")
source("modules/mod_variables.R")
source("modules/mod_relationships.R")

# ---- UI ----
ui <- dashboardPage(
  skin = "black",

  dashboardHeader(
    title = span(
      icon("dna"), "BioData Explorer",
      style = "font-weight: 700; font-size: 18px;"
    ),
    titleWidth = 280
  ),

  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      menuItem("Upload Data", tabName = "upload", icon = icon("cloud-upload-alt")),
      menuItem("Data Overview", tabName = "overview", icon = icon("table")),
      menuItem("Missing Data", tabName = "missing", icon = icon("question-circle")),
      menuItem("Variable Types", tabName = "variables", icon = icon("tags")),
      menuItem("Relationships", tabName = "relationships", icon = icon("project-diagram")),

      hr(),

      div(
        style = "padding: 10px 15px; color: #999; font-size: 12px;",
        p("Upload a CSV, TSV, or Excel file to begin exploring your biological dataset."),
        p(icon("github"), tags$a("GitHub Repo", href = "#", style = "color: #ccc;"))
      )
    )
  ),

  dashboardBody(
    # Custom CSS
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f7f9fc; }
      .box { border-top: 3px solid #2c7bb6; }
      .box-header { background-color: #fff; }
      .info-box-icon { background-color: #2c7bb6 !important; }
      .severity-none    { color: #27ae60; font-weight: bold; }
      .severity-low     { color: #2ecc71; }
      .severity-moderate { color: #f39c12; font-weight: bold; }
      .severity-high    { color: #e74c3c; font-weight: bold; }
      .severity-critical { color: #c0392b; font-weight: bold; background: #fdecea; padding: 2px 6px; border-radius: 3px; }
      .type-badge {
        display: inline-block; padding: 3px 10px; border-radius: 12px;
        font-size: 12px; font-weight: 600; color: #fff; margin: 2px;
      }
      .type-numeric_continuous  { background: #2c7bb6; }
      .type-numeric_discrete    { background: #5aa4d9; }
      .type-categorical_nominal { background: #27ae60; }
      .type-categorical_ordinal { background: #16a085; }
      .type-binary              { background: #8e44ad; }
      .type-date_time           { background: #e67e22; }
      .type-identifier          { background: #95a5a6; }
      .type-free_text           { background: #7f8c8d; }
      .matters-TRUE  { color: #e74c3c; font-weight: bold; }
      .matters-FALSE { color: #27ae60; }
    "))),

    tabItems(
      tabItem(tabName = "upload", mod_upload_ui("upload")),
      tabItem(tabName = "overview", mod_overview_ui("overview")),
      tabItem(tabName = "missing", mod_missing_ui("missing")),
      tabItem(tabName = "variables", mod_variables_ui("variables")),
      tabItem(tabName = "relationships", mod_relationships_ui("relationships"))
    )
  )
)


# ---- Server ----
server <- function(input, output, session) {

  # Shared reactive: the uploaded data
  user_data <- reactiveVal(NULL)

  # Module servers
  mod_upload_server("upload", user_data)
  mod_overview_server("overview", user_data)
  mod_missing_server("missing", user_data)
  mod_variables_server("variables", user_data)
  mod_relationships_server("relationships", user_data)

  # Auto-navigate to Overview after upload
  observeEvent(user_data(), {
    if (!is.null(user_data())) {
      updateTabItems(session, "tabs", "overview")
    }
  })
}

shinyApp(ui, server)
