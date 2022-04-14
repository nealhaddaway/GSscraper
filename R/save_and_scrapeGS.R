#' Global wrapper function for generating, saving and scraping info
#'
#' @description Function wraps the `buildGSlinks()`, `save_htmls()`, and scrape functions.
#' @param and_terms Vector of alphanumeric terms searched using the AND Boolean operator,
#' specified by Google Scholar as 'with all of the words'.
#' @param exact_phrase Vector of alphanumeric terms enclosed in inverted commas and searched
#' as phrases (e.g. "large cat"), specified by Google Scholar as 'with the exact phrase'.
#' @param or_terms Vector of alphanumeric terms searched using the OR Boolean operator,
#' specified by Google Scholar as 'with at least one of the words'.
#' @param not_terms Vector of alphanumeric terms searched using the NOT Boolean operator,
#' specified by Google Scholar as 'without the words'.
#' @param language Two-letter language code for search language. The default is 'en' (English).
#' @param year_from Integer full numeric year (e.g. 2000) from which searching will be performed
#' (inclusive). If no value provided, all years are searched.
#' @param year_to Integer full numeric year (e.g. 2020) to which searching will be performed
#' (inclusive). If no value provided, all years are searched.
#' @param start_page Integer specifying which page(s) of search results should be displayed.
#' If multiple pages are selected, multiple URLs are returned, one for each page of ten
#' search results. The default is set to generate a list of 100 URLs (maximum set of Google
#' Scholar results visible).
#' @param pages Integer for the number of pages of search results to be returned (one link per
#' page). A maximum of 100 pages can be displayed in Google Scholar. The default value is 1.
#' @param incl_cit Logical argument (TRUE or FALSE) specifying whether citations should be
#' included in the search
#' @param incl_pat Logical argument (TRUE or FALSE) specifying whether patents should be
#' included in the search
#' @param titlesearch Logical argument (TRUE or FALSE) specifying whether the search should
#' be performed on article titles only or anywhere in the record. The default is FALSE.
#' @param authors The names of authors searched for.
#' @param source The name of the source of the articles (e.g. academic journal).
#' @param pause Integer specifying the number of seconds to wait between download attempts.
#' The default value is 4 seconds.
#' @param backoff A logical argument (TRUE or FALSE) specifying whether responsive backing-off
#' should be used. If set to TRUE, the time between calls is varied depending on how long the
#' server takes to respond to the original request. The responsive back-off time is set to
#' multiple the response time by the `pause` time: i.e. if the system takes 1.02 seconds to
#' respond and `pause` time is set to 4 seconds, a 4.10 second delay will be employed before
#' the next call. The default for back-off is `FALSE`.
#' @return A list containing: 1) (data) a data frame containing all information that can be
#' extracted from all html files in the working directory; and, 2) (report) a report of the
#' links generated and the input variables used.
#' @examples
#' \dontrun{
#' and_terms <- c('river', 'aquatic')
#' exact_phrase <- c('water chemistry')
#' or_terms <- c('crayfish', 'fish')
#' not_terms <- c('lobster', 'coral')
#' year_from <- 1900
#' year_to <- 2020
#' info <- save_and_scrapeGS(and_terms, exact_phrase, or_terms, not_terms, pages = 3)
#' info
#' }
#'@export
save_and_scrapeGS <- function(and_terms = '',
                              exact_phrase = '',
                              or_terms = '',
                              not_terms = '',
                              language = 'en',
                              year_from = '',
                              year_to = '',
                              start_page = 1,
                              pages = 1,
                              incl_cit = TRUE,
                              incl_pat = TRUE,
                              titlesearch = FALSE,
                              authors = '',
                              source = '',
                              pause = 4,
                              backoff = FALSE){
  links <- buildGSlinks(and_terms = and_terms,
                        exact_phrase = exact_phrase,
                        or_terms = or_terms,
                        not_terms = not_terms,
                        language = language,
                        year_from = year_from,
                        year_to = year_to,
                        start_page = start_page,
                        pages = pages,
                        incl_cit = incl_cit,
                        incl_pat = incl_pat,
                        titlesearch = titlesearch,
                        authors = authors,
                        source = source)
  htmls <- save_htmls(urls = links,
                      pause = pause,
                      backoff = backoff)
  df <- data.frame()
  for(i in 1:length(htmls)){
    info <- get_info(unlist(htmls[i]))
    data <- info$df
    df <- dplyr::bind_rows(df, data)
  }
  data <- df
  report <- paste('File generated: ',
                  paste('Search date, time, timezone: ',
                        Sys.time(),
                        ' (',
                        Sys.timezone(),
                        ')',
                        sep = ''),
                  '\n',
                  'Search parameters:',
                  paste('All these words: ',
                        paste(and_terms,
                              collapse = '; '),
                        sep = ''),
                  paste('This exact word or phrase: ',
                        paste('"',
                              trimws(exact_phrase),
                              '"',
                              sep = ''),
                        sep = ''),
                  paste('Any these words: ',
                        paste(or_terms,
                              collapse = '; '),
                        sep = ''),
                  paste('None of these words: ',
                        paste(not_terms,
                              collapse = '; '),
                        sep = ''),
                  if(language == ''){
                    'Language: Any'
                  } else {
                    paste('Language: ',
                          language,
                          sep = '')
                  },
                  paste('Between and including these years:',
                        if(year_from == ''){
                          'all'
                        } else {
                          year_from
                        },
                        'and',
                        if(year_to == ''){
                          'now'
                        } else {
                          year_to
                        },
                        sep = ' '),
                  if(pages == ''){
                    'Number of pages exported: 1'
                  } else {
                    paste('Number of pages exported: ',
                          pages,
                          sep = '')
                  },
                  if(start_page == ''){
                    'Starting from page: 1'
                  } else {
                    paste('Starting from page: ',
                          start_page,
                          sep = '')
                  },
                  if(incl_cit == ''){
                    'Citations included: TRUE'
                  } else {
                    paste('Citations included: ',
                          incl_cit,
                          sep = '')
                  },
                  if(incl_pat == ''){
                    'Patents included: TRUE'
                  } else {
                    paste('Citations included: ',
                          incl_pat,
                          sep = '')
                  },
                  if(titlesearch == ''){
                    'Search only in the title: FALSE'
                  } else {
                    paste('Search only in the title: ',
                          titlesearch,
                          sep = '')
                  },
                  paste('Authors:',
                        authors,
                        sep = ' '),
                  paste('Source:',
                        source,
                        sep = ' '),
                  if(path == ''){
                    paste('Files saved to working directory: ',
                          getwd(),
                          sep = '')
                    } else {
                      paste('Files saved to: ',
                            path,
                            sep = ' ')
                      },
                  if(pause == ''){
                    'Paused 4 seconds between calls.'
                  } else {
                    paste('Paused ',
                          pause,
                          ' seconds between calls',
                          sep = '')
                  },
                  if(backoff == ''){
                    'Sensitive backing off not employed (see help files for more information).'
                  } else {
                    'Sensitive backing off employed (see help files for more information).'
                  },
                  paste('Search date, time, timezone: ',
                        Sys.time(),
                        ' (',
                        Sys.timezone(),
                        ')',
                        sep = ''),
                  'Google Scholar search pages exported:',
                  paste(links,
                        collapse = '\n'),
                  '\n',
                  sep = '\n')

  output <- list(data = data,
                 report = report)

  return(output)
}
