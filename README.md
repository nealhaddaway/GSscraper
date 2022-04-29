# GSscraper <img src="inst/extdata/GSscraper_hex.png" align="right" width="15%"/>

This package contains a suite of functions to scrape search results from Google Scholar by using a function that pauses before downloading each page of results to avoid IP blocking. It then scrapes the locally saved files for citation relevant information.

Run the GSscraper Shiny app on your machine with the following code:

`install.packages("shiny")`<br>
`install.packages("RCurl")`<br>
`devtools::install_github("hrbrmstr/cfhttr")`<br>
`install.packages("purrr")`<br>
`install.packages("dplyr")`<br>
`install.packages("shinyjs")`<br>
`install.packages("shinyWidgets")`<br>
`install.packages("textclean")`<br>
`install.packages("stringi")`<br>
`install.packages("mgsub")`<br>
`install.packages("magrittr")`<br>
`install.packages("zen4R")`<br>
`install.packages("shinyalert")`<br>
`install.packages("shinybusy")`<br>
`install.packages("strex")`<br>
`shiny::runGitHub("GSscraper", "nealhaddaway", subdir = "inst/shiny-examples/GSscraper")`
