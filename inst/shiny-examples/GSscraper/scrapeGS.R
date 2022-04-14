#' Scrape all Google Scholar information
#'
#' @description A wrapper for the scraping functions to produce a dataframe of citations for
#' each page of Google Scholar search results.
#' @return A dataframe containing all extractable information from all html files in the working
#' directory.
#' @examples
#' info <- get_info(html)
#'@export
get_info <- function(html){
  code_lines <- split_by_div(html)
  titles <- get_titles(code_lines)
  citations <- get_citations(code_lines)
  authors <- get_authors(code_lines)
  descriptions <- get_descriptions(code_lines)
  year <- get_years(code_lines)
  year[year == 'character(0)'] <- NA
  links <- get_links(code_lines)
  dois <- links2dois(code_lines)
  df <- data.frame(titles = titles[,1],
                   citations = citations[,1],
                   authors = authors[,1],
                   descriptions = descriptions[,1],
                   year = year[,1],
                   links = links[,1],
                   dois = dois[,1])

  #extract number of results
  n_results <- code_lines[grep('gs_ab_mdw', code_lines)]
  n_results <- n_results[grep('results', n_results)]
  n_results <-  gsub('.', ',', stringr::str_extract_all(tolower(n_results), "(?<=about ).+(?= results ())" )[[1]])
  report_full <- paste0('Your search resulted in c. ', n_results, ' records')

  #if df is blank, check to see if error code in html
  if(all(apply(df, 2, function(x) all(is.na(x)))) == TRUE){
    error <- any(grepl('Error 403', html))
    if(error == FALSE){
      error <- 'No further reults on this page'
    } else {
      error <- 'Error 403 - you have been temporarily blocked by Google Scholar. Please wait 24 hours and try again'
    }
    report_page <- error
  } else {
    report_page <- paste0(nrow(df), ' results on this page')
  }

  output <- list(df = df, report = list(full = report_full, page = report_page))

  return(output)
}


#' Split file by '<div' code into different lines
#'
#' @description Split the imported html code into lines based on the '<div' field code.
#' @param html A string consisting of the html code for a webpage
#' @return A vector of strings, one for each separated line
#' @examples
#' lines <- split_by_div(code)
#' lines;
#' @export
split_by_div <- function(html) {
  code_lines <- unlist(strsplit(html, '\\<div class', useBytes = TRUE))
  return(code_lines)
}


#' Extract titles from Google Scholar results
#'
#' @description Extract the titles of search results from Google Scholar search results, saved as
#' html files.
#' @param code_lines A vector of lines consisting of the html code for a webpage
#' @return A vector of result titles (10 per page)
#' @examples
#' titles <- get_titles(code_lines)
#' @export
get_titles <- function(code_lines){
  y <- grep("gs_ri", code_lines) #find location of lines containing GS title tag 'gs_ri'
  titles <- code_lines[y] #extract lines
  titles <- (gsub("<.*?>", "", titles))[2:11] #remove code inside '<>'
  titles <- data.frame(titles)
  titles$titles <- sub("=\"gs_ri\">", "", titles$titles) #remove field codes
  titles$titles <- gsub('\n', '', titles$titles) #remove line break codes
  titles$titles <- sub('.*] ', '', titles$titles) #remove field codes
  titles$titles <- gsub('<', '', titles$titles) #remove field codes
  titles$titles <- gsub('&#8230;', '…', titles$titles) #replace '&#8230' with ellipsis
  return(titles)
}


#' Extract citation information from Google Scholar results
#'
#' @description Extract information relating to the record's citation (including the authors,
#' journal/source and publication year), from Google Scholar search results.
#' @param code_lines A vector of lines consisting of the html code for a webpage
#' @return A vector of citation information
#' @examples
#' citations <- get_citations(code_lines)
#' @export
get_citations <- function(code_lines){
  y <- grep("gs_ri", code_lines)
  citations <- code_lines[y+1]
  citations <- (gsub("<.*?>", "", citations))[2:11]
  citations <- data.frame(citations)
  citations$citations <- sub("=\"gs_a\">", "", citations$citations)
  citations$citations <- sub("<", "", citations$citations)
  citations$citations <- gsub('\n', '', citations$citations) #remove line break codes
  citations$citations <- mgsub::mgsub(citations$citations, c('\302', '\240', '\342', '\200', '\246', '\303', '\241', '\305', '\201', '\242', '\223'), c('', '', '', '', '', '', '', '', '', '', ''))
  citations$citations <- gsub('&#8230;', '…', citations$citations) #replace '&#8230' with ellipsis
  return(citations)
}


#' Extract authors from Google Scholar results
#'
#' @description Extract author details from the record's citation from Google
#' Scholar search results.
#' @param code_lines A vector of lines consisting of the html code for a web page
#' @return A vector of author lists
#' @examples
#' authors <- get_authors(code_lines)
#' authors;
#' @export
get_authors <- function(code_lines){
  authors <- get_citations(code_lines)
  authors$citations <- sub("\\-.*", "", authors$citations)
  authors$citations <- gsub('\\.\\.\\.', '; et al.', authors$citations)
  authors$citations <- gsub('&#8230;', '…', authors$citations) #replace '&#8230' with ellipsis
  authors$citations <- gsub(',', ';', authors$citations)
  authors$citations <- textclean::replace_non_ascii(authors$citations)
  return(authors)
}


#' Extract article descriptions from Google Scholar results
#'
#' @description Extract article descriptions (summaries of key sentences), from Google
#' Scholar search results.
#' @param code_lines A vector of lines consisting of the html code for a webpage
#' @return A vector of descriptions
#' @examples
#' descriptions <- get_descriptions(code_lines)
#' descriptions;
#' @export
get_descriptions <- function(code_lines){
  y <- grep("gs_ri", code_lines)
  descriptions <- code_lines[y+2]
  descriptions <- (gsub("<.*?>", "", descriptions))[2:11]
  descriptions <- data.frame(descriptions)
  descriptions$descriptions <- sub("=\"gs_rs\">", "", descriptions$descriptions)
  descriptions$descriptions <- sub("<", "", descriptions$descriptions)
  descriptions$descriptions <- mgsub::mgsub(descriptions$descriptions, c('\302', '\240', '\342', '\200', '\246', '\303', '\241', '\305', '\201', '\242', '\223'), c('', '', '', '', '', '', '', '', '', '', ''))
  descriptions$descriptions <- gsub('\n', '', descriptions$descriptions) #remove line break codes
  descriptions$descriptions <- gsub('&#8230;', '…', descriptions$descriptions) #replace '&#8230' with ellipsis
  return(descriptions)
}


#' Extract article descriptions from Google Scholar results
#'
#' @description Extract article publication years from Google Scholar search results.
#' @param code_lines A vector of lines consisting of the html code for a webpage
#' @return A vector of publication years
#' @examples
#' years <- get_years(lines)
#' years;
#' @export
get_years <- function(code_lines){
  y <- get_citations(code_lines)
  year <- data.frame()
  for(i in 1:length(y$citations)){
    yr <- unlist(regmatches(y$citations[i], gregexpr("\\b(19|20)\\d{2}\\b", y$citations[i])))[1]
    year <- rbind(year, yr)
  }
  return(year)
}


#' Extract article links from Google Scholar results
#'
#' @description Extract links to websites holding article information from Google
#' Scholar search results.
#' @param code_lines A vector of lines consisting of the html code for a webpage
#' @return A vector of URLs
#' @examples
#' links <- get_links(code_lines)
#' links;
#' @export
get_links <- function(code_lines){
  y <- grep("gs_ri", code_lines)
  links <- code_lines[y]
  links <- sub('.*href=', '', (links)[2:11])
  links <- data.frame(links)
  links$links <- sub('\\ data.*', '', links$links)
  links$links <- gsub('\"', '', links$links)
  return(links)
}


#' Extract DOIs from multiple article links
#'
#' @description Extract digital object identifiers ('DOIs') from  links to the websites
#' holding article information, where they are contained within (e.g.
#' 'https://link.springer.com/article/10.1186/1475-2875-13-446' contains the DOI
#' 10.1186/1475-2875-13-446).
#' @param code_lines A data frame of links to a webpage from Google Scholar
#' @return A dataframe of dois
#' @examples
#' dois <- links2dois(code_lines)
#' @export
links2dois <- function(code_lines){
  links <- get_links(code_lines)
  dois <- data.frame()
  for(i in 1:length(links$links)){
    x <- stringr::str_extract(links$links[i], '10.*')
    dois <- rbind(dois, x)
  }
  names(dois) <- 'doi'
  dois$doi <- decode_dois(dois$doi)
  dois$doi <- gsub('/html', '', dois$doi)
  dois$doi <- gsub('/pdf', '', dois$doi)
  return(dois)
}
