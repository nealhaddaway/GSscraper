#' Download one results page as html file
#'
#' @description Downloads one page of Google Scholar search results from a URLs as html files with
#' a specific wait-time to avoid IP address blocking.
#' @param url One URLs corresponding to a page of search results.
#' @param pause Integer specifying the number of seconds to wait between download attempts. The
#' default value is 4 seconds.
#' @param backoff A logical argument (TRUE or FALSE) specifying whether responsive backing-off should be used.
#' If set to TRUE, the time between calls is varied depending on how long the server takes to respond to the
#' original request. The responsive back-off time is set to multiple the response time by the `pause` time: i.e.
#' if the system takes 1.02 seconds to respond and `pause` time is set to 4 seconds, a 4.10 second delay will
#' be employed before the next call. The default for back-off is `FALSE`.
#' @importFrom stats runif
#' @examples
#' \dontrun{
#' url <- 'https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=testing&btnG='
#' html <- save_html(url, pause = 3, backoff = FALSE)
#' }
#' @return An HTML file is downloaded as a string object Pause and success messages are printed to the console.
#' @export
save_html <- function(url,
                      pause = 0.5,
                      backoff = FALSE){
  t0 <- Sys.time()

  pause <- pause * stats::runif(1, 0.5, 1.5)

  #initiate scrape and detect redirect
  message('Saving next page of search results...\n')
  html <- RCurl::getURL(url, followlocation=TRUE)
  names(html) <- url

  t1 <- Sys.time()
  response_delay <- round(as.numeric(t1-t0), 3)
  if(backoff == TRUE){
    message(paste('Request took',
                  round(response_delay, 3),
                'seconds to perform. Waiting',
                round(pause*response_delay, 3),
                'seconds...\n'))
    Sys.sleep(pause*response_delay)
  } else {
    message(paste('Request took',
                  round(response_delay, 3),
                'second(s) to perform. Waiting',
                round(pause, 3),
                'seconds...\n'))
    Sys.sleep(pause)
  }

  return(html)

}


#' Download multiple results pages as html files
#'
#' @description Downloads one or more Google Scholar URLs as html files with a specific wait-time
#' to avoid IP address blocking.
#' @param urls One or more URLs corresponding to pages of Google Scholar search results.
#' @param pause Integer specifying the number of seconds to wait between download attempts. The
#' default value is 4 seconds.
#' @param backoff A logical argument (TRUE or FALSE) specifying whether responsive backing-off should be used.
#' If set to TRUE, the time between calls is varied depending on how long the server takes to respond to the
#' original request. The responsive back-off time is set to multiple the response time by the `pause` time: i.e.
#' if the system takes 1.02 seconds to respond and `pause` time is set to 4 seconds, a 4.10 second delay will
#' be employed before the next call. The default for back-off is `FALSE`.
#' @examples
#' \dontrun{
#' urls <- c('https://www.google.com/search?q=site:www.sei.org+climate&start=0',
#'           'https://www.google.com/search?q=site:www.sei.org+climate&start=10')
#' htmls <- save_htmls(urls, pause = 5)
#' }
#' @return HTML files are downloaded as a string object Pause and success messages are printed to the console.
#' @export
save_htmls <- function(urls,
                       pause = 0.5,
                       backoff = FALSE){
  t0 <- Sys.time()
  htmls <- list()
  for(i in 1:length(urls)){
    html <- save_html(urls[i], pause = pause, backoff = FALSE)
    htmls <- c(htmls, html)
  }
  t1 <- Sys.time()
  response_delay <- round(as.numeric(t1-t0), 3)

  message(paste('Downloads finished in',
                round(response_delay, 3),
              'seconds.'))

  return(htmls)

}
