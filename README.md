# GSscraper <img src="inst/extdata/GSscraper_hex.png" align="right" width="15%"/>

This package contains a suite of functions to scrape search results from Google Scholar by using a function that pauses before downloading each page of results to avoid IP blocking. It then scrapes the locally saved files for citation relevant information.

Run the GSscraper Shiny app on your machine with the following code:

`install.packages(c("shiny", "RCurl", "purrr", "dplyr", "shinyjs", "shinyWidgets", "textclean", "stringi", "magrittr", "zen4R", "shinyalert", "shinybusy", "strex"))`<br>
`devtools::install_github("hrbrmstr/cfhttr")`<br>
`shiny::runGitHub("GSscraper", "nealhaddaway", subdir = "inst/shiny-examples/GSscraper")`
