#' Build GS links
#'
#' Function takes as input a partial Boolean search string and produces a functioning (set of)
#' URLs; one for each page of search results on Google Scholar.
#' @description Constructs series of Google Scholar search page URLs
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
#' @examples
#' \dontrun{
#' and_terms <- c('river', 'aquatic')
#' exact_phrase <- c('water chemistry')
#' or_terms <- c('crayfish', 'fish')
#' not_terms <- c('lobster', 'coral')
#' year_from <- 1900
#' year_to <- 2020
#' link <- buildGSlinks(and_terms = and_terms,
#'     exact_phrase = exact_phrase,
#'     or_terms = or_terms,
#'     not_terms = not_terms,
#'     year_from = year_from,
#'     year_to = year_to,
#'     pages = 10,
#'     incl_cit = TRUE,
#'     incl_pat = FALSE)
#' link$link
#' }
#' @return A list containing: 1) ($link) links to the specified search results; and, 2) ($report)
#' a text string containing a report of the links generated and the input variables used.
#' @export
buildGSlinks <- function(and_terms = '',
                         exact_phrase = '',
                         or_terms = '',
                         not_terms = '',
                         year_from = '',
                         year_to = '',
                         start_page = 1,
                         pages = 1,
                         language = 'en',
                         incl_cit = TRUE,
                         incl_pat = TRUE,
                         titlesearch = FALSE,
                         authors = '',
                         source = '') {
  #report
  report <- paste(
          'File generated: ',
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
          paste('None of these words: ',
                paste(not_terms,
                      collapse = '; '),
                sep = ''),
          paste('This exact word or phrase: ',
                paste('"',
                      exact_phrase,
                      '"',
                      sep = ''),
                sep = ''),
          paste('Any these words: ',
                paste(or_terms,
                      collapse = '; '),
                sep = ''),
          if(language == ''){
            'Language: Any'
          } else {
            paste('Language: ',
                  language,
                  sep = '')
          },
          paste('Between these years:',
                year_from,
                'and',
                year_to,
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
          sep = '\n')


    #calculations
    start_after <- (start_page-1)*10 #calculate start record from start page

    and_terms <- if(any(and_terms == '') == TRUE) { #if and_terms is blank then leave it blank, otherwise combine terms with '+'
      and_terms <- ''
    } else {
      and_terms <- paste(gsub(' ',
                              '+',
                              paste(and_terms,
                                    collapse = '+')),
                         sep = '')
    }

    exact_phrase <- if(any(exact_phrase == '') == TRUE){ #if exact_phrase is blank then leave it blank, otherwise combine terms with '+' and top/tail with '+%22'/'%22'
      exact_phrase <- ''
    } else {
      exact_phrase <- paste('+%22',
                            gsub(' ',
                                 '+',
                                 paste(exact_phrase,
                                       collapse = '+')),
                            '%22',
                            sep = '')
    }

    or_terms <- if(any(or_terms == '') == TRUE){ #if or_terms is blank then leave it blank, otherwise combine terms with '+OR+'
      or_terms <- ''
    } else {
      or_terms <- paste('+',
                        gsub(' ',
                             '+OR+',
                             paste(or_terms,
                                   collapse = '+OR+')),
                        sep = '')
    }

    not_terms <- if(any(not_terms == '') == TRUE){ #if not_terms is blank then leave it blank, otherwise combine terms with '+OR+'
      not_terms <- ''
    } else {
      not_terms <- paste('+-',
                         gsub(' ',
                              '+-',
                              paste(not_terms,
                                    collapse = '+-')),
                         sep = '')
    }

    language <- paste('&hl=',
                      language,
                      sep = '') #specify the language

    if((year_from == '') == TRUE){ #specify the start year
      year_from <- ''
    } else {
      year_from <- paste('&as_ylo=',
                       year_from,
                       sep = '')
    }
    if((year_to == '') == TRUE){ #specify the stop year
      year_to <- ''
    } else {
      year_to <- paste('&as_yhi=',
                       year_to,
                       sep = '')
    }

    if(incl_cit == TRUE){ #if user selects 'incl_cit' (include citations) then as_vis is changed
      incl_cit <- '&as_vis=0'
    } else {
      incl_cit <- '&as_vis=1'
    }
    if(incl_pat == TRUE){ #if user selects 'incl_pat' (include patents) then as_sdt is changed
      incl_pat <- '&as_sdt=2007'
    } else {
      incl_pat <- ''
    }

    if(titlesearch == TRUE){ #specify if title only
      titlesearch <- 'allintitle%3A'
    } else {
      titlesearch <- ''
    }

    if(any(authors == '') == TRUE){ #search authors
      authors <- ''
    } else {
      authors <- paste('+author:',
                       paste(gsub(' ', '+', authors),
                             collapse = '+author:'),
                       sep = '')
    }

    if(any(source == '') == TRUE){ #search authors
      source <- ''
    } else {
      source <- paste('+source:',
                       paste(gsub(' ', '+', source),
                             collapse = '+source:'),
                       sep = '')
    }

    #build URL
    if (pages == 1){ #if pages = 1 then only a single link is generated
      link <- paste('https://scholar.google.co.uk/scholar?start=',
                    start_after,
                    '&q=',
                    titlesearch,
                    and_terms,
                    or_terms,
                    exact_phrase,
                    not_terms,
                    authors,
                    source,
                    language,
                    year_from,
                    year_to,
                    incl_cit,
                    incl_pat,
                    sep = '')
    } else { #otherwise, one link is generated for each page
      link <- list()
      init <- seq(start_after, (pages*10-10), 10)
      for (i in init){
        x <- paste('https://scholar.google.co.uk/scholar?start=',
              i,
              '&q=',
              titlesearch,
              and_terms,
              or_terms,
              exact_phrase,
              not_terms,
              authors,
              source,
              language,
              year_from,
              year_to,
              incl_cit,
              incl_pat,
              sep = '')
        link <- append(link, x)
        link <- unlist(link)
      }

    }

    report <- paste(report,
                    '\n',
                    'GS links generated:',
                    paste(link,
                          collapse = '\n'),
                    '\n',
                    sep = '\n')

    output <- list(link = link, report = report)

    return(output)
}
