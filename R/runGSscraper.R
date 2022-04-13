#' Wrapper to run GSscraper Shiny app locally
#'
#' @description Run the GSscraper Shiny app locally with this
#' function. Calls the latest version of the app from GitHub without
#' the need to install GSscraper.
#' @importFrom shiny runGitHub
#' @export
runGSscraper <- function(){
  shiny::runGitHub("GSscraper",
                   "nealhaddaway",
                   subdir = "inst/shiny-examples/GSscraper")
}
