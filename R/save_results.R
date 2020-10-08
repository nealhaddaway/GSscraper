#' Download one results page as html file
#'
#' @Description Downloads one page of Google Scholar results from a URLs as html files with
#' a specific wait-time to avoid IP address blocking.
#' @param url One URLs corresponding to a page of Google Scholar search results.
#' @param sleep Integer specifying the number of seconds to wait between download attempts. The
#' default value is 4 seconds.
#' @examples
#' urls <- 'https://scholar.google.co.uk/scholar?start=50&q=insect+population+%22systematic+review%22&hl=en&as_vis=0,5&as_sdt=0,5'
#' save_html(urls, sleep = '5');
#' @return An html file is downloaded with a file name corresponding to the URL with punctuation removed
#' for clarity. Files are saved to the working directory. A pause notification is printed to the
#' console.
#' @export
save_html <- function(url, sleep = '4'){
  download.file(url,
                destfile = paste(tm::removePunctuation(url), ".html", sep = ""),
                method = "auto",
                quiet = FALSE)
  print(paste('Waiting',
              sleep,
              'seconds before next attempt'))
  Sys.sleep(sleep)
}


#' Download multiple results pages as html files
#'
#' @Description Downloads one or more Google Scholar URLs as html files with a specific wait-time
#' to avoid IP address blocking.
#' @param url One or more URLs corresponding to pages of Google Scholar search results.
#' @param sleep Integer specifying the number of seconds to wait between download attempts. The
#' default value is 4 seconds.
#' @examples
#' urls <- c('https://scholar.google.co.uk/scholar?start=50&q=insect+population+%22systematic+review%22&hl=en&as_vis=0,5&as_sdt=0,5',
#'    'https://scholar.google.co.uk/scholar?start=90&q=insect+population+%22systematic+review%22&hl=en&as_vis=0,5&as_sdt=0,5')
#' save_html(urls, sleep = '5');
#' @return Files are downloaded with a file name corresponding to the URL with punctuation removed
#' for clarity. Files are saved to the working directory. A pause notification is printed to the
#' console.
#' @export
save_htmls <- function(url, sleep = '4'){
  mapply(save_html, url, sleep)
}
