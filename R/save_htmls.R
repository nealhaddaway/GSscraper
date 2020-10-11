#' Download one results page as html file
#'
#' @description Downloads one page of Google Scholar results from a URLs as html files with
#' a specific wait-time to avoid IP address blocking.
#' @param url One URLs corresponding to a page of Google Scholar search results.
#' @param path The path in which the file should be saved. The default is to save in the working directory.
#' @param pause Integer specifying the number of seconds to wait between download attempts. The
#' default value is 4 seconds.
#' @param backoff A logical argument (TRUE or FALSE) specifying whether responsive backing-off should be used.
#' If set to TRUE, the time between calls is varied depending on how long the server takes to respond to the
#' original request. The responsive back-off time is set to multiple the response time by the `pause` time: i.e.
#' if the system takes 1.02 seconds to respond and `pause` time is set to 4 seconds, a 4.10 second delay will
#' be employed before the next call. The default for back-off is `FALSE`.
#' @examples
#' url <- 'https://scholar.google.co.uk/scholar?start=50&q=insect+population+%22systematic+review%22&hl=en&as_vis=0,5&as_sdt=0,5'
#' save_html(url, pause = 5, backoff = FALSE);
#' @return An HTML file is downloaded with a file name corresponding to the URL with punctuation removed
#' for clarity. Files are saved to the working directory. A pause notification is printed to the
#' console.tm::removePunctuation(url)
#' @export
save_html <- function(url, path = '', pause = 4, backoff = FALSE){
  t0 <- Sys.time()
  utils::download.file(url,
                destfile = paste(path,
                                 paste('page',
                                       sub('022.*', '', (gsub("\\D", "", url))),
                                       sep = '_'),
                                 '.html',
                                 sep = ''),
                method = 'auto',
                quiet = FALSE)
  t1 <- Sys.time()
  response_delay <- round(as.numeric(t1-t0), 3)
  if(backoff == TRUE){
    print(paste('Request took',
                response_delay,
                'seconds to perform. Waiting',
                pause*response_delay,
                'seconds before next attempt.'))
    Sys.sleep(pause*response_delay)
  } else {
    print(paste('Request took',
                response_delay,
                'second(s) to perform. Waiting',
                pause,
                'seconds before next attempt.'))
    Sys.sleep(pause)
  }
}


#' Download multiple results pages as html files
#'
#' @description Downloads one or more Google Scholar URLs as html files with a specific wait-time
#' to avoid IP address blocking.
#' @param urls One or more URLs corresponding to pages of Google Scholar search results.
#' @param path The path in which the files should be saved. The default is to save in the working directory.
#' @param pause Integer specifying the number of seconds to wait between download attempts. The
#' default value is 4 seconds.
#' @param backoff A logical argument (TRUE or FALSE) specifying whether responsive backing-off should be used.
#' If set to TRUE, the time between calls is varied depending on how long the server takes to respond to the
#' original request. The responsive back-off time is set to multiple the response time by the `pause` time: i.e.
#' if the system takes 1.02 seconds to respond and `pause` time is set to 4 seconds, a 4.10 second delay will
#' be employed before the next call. The default for back-off is `FALSE`.
#' @examples
#' urls <- c('https://scholar.google.co.uk/scholar?start=50&q=insect+population+%22systematic+review%22&hl=en&as_vis=0,5&as_sdt=0,5',
#'    'https://scholar.google.co.uk/scholar?start=90&q=insect+population+%22systematic+review%22&hl=en&as_vis=0,5&as_sdt=0,5')
#' save_htmls(urls, pause = 5);
#' @return Files are downloaded with a file name corresponding to the URL with punctuation removed
#' for clarity. Files are saved to the working directory. A pause notification is printed to the
#' console.
#' @export
save_htmls <- function(urls,
                       path = '',
                       pause = 4,
                       backoff = FALSE){
  t0 <- Sys.time()
  mapply(save_html,
         urls,
         path,
         pause,
         backoff)
  t1 <- Sys.time()
  response_delay <- round(as.numeric(t1-t0), 3)
  print(paste('Downloads finished in',
              response_delay,
              'seconds. Check destination folder to ensure all files were downloaded successfully.'))
}
