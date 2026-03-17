#' Launch the BioData Explorer Shiny App
#'
#' Opens the interactive dashboard for exploring biological datasets.
#'
#' @param ... Additional arguments passed to \code{shiny::runApp}.
#' @export
#' @examples
#' \dontrun{
#' launch_app()
#' }
launch_app <- function(...) {
  app_dir <- system.file("shiny-app", package = "BioDataExplorer")
  if (app_dir == "") {
    stop("Shiny app directory not found. Try reinstalling `BioDataExplorer`.")
  }
  shiny::runApp(app_dir, ...)
}
