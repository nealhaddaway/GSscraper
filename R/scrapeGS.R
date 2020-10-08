#' Scrape all Google Scholar information
#'
#' @description A wrapper for the scraping functions to produce a dataframe of citations for
#' each page of Google Scholar search results.
#' @return A dataframe containing all extractable information from all html files in the working
#' directory.
#' @examples
#' info <- get_info();
#'@export
get_info <- function(){
  codes <- get_htmls_code()
  code_lines <- mapply(split_by_div, codes)
  titles <- as.data.frame(mapply(get_titles, code_lines)) %>%
    stack()
  names(titles) <- c('title', 'sourcefile')
  citations <- as.data.frame(mapply(get_citations, code_lines)) %>%
    stack()
  descriptions <- as.data.frame(mapply(get_descriptions, code_lines)) %>%
    stack()
  year <- as.data.frame(mapply(get_years, code_lines))
  year[year == 'character(0)'] <- NA
  year <- stack(year)
  names(year) <- c('year', 'sourcefile')
  links <- as.data.frame(mapply(get_links, code_lines)) %>%
    stack()
  dois <- links2dois(code_lines)
  df <- data.frame(titles[1], citations[1], descriptions[1], year[1], links[1], dois[1])
  colnames(df) <- c('title', 'citation', 'description', 'year', 'links', 'doi')
  return(df)
}


#' Scrape in code from local html file
#'
#'@description Scrape in code from an html file saved locally
#'@param filename Name of the file to be scraped. File should be saved in the working
#'directory.
#'@return A string of html code for a webpage
#'@examples
#'code <- get_html_code(file.choose());
#'@export
get_html_code <- function(filename){
  x <- xml2::read_html(filename)
  x <- paste(x, collapse = '')
  return(x)
}


#' Scrape in code from multiple local html files
#'
#'@description Scrape in code from html files saved locally. Files should be saved in the working
#'directory.
#'@return One or more strings of html code for a webpage, stored as a list
#'@examples
#'html_codes <- get_htmls_code()
#'file1 <- unlist(html_codes[1]);
#'@export
get_htmls_code <- function(){
  filenames <- list.files(getwd())
  x <- as.list(mapply(get_html_code, filenames))
  return(x)
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
  x <- unlist(strsplit(html, "\\<div", useBytes = TRUE))
  return(x)
}


#' Extract titles from Google Scholar results
#'
#' @description Extract the titles of search results from Google Scholar search results, saved as
#' html files.
#' @param html A vector of lines consisting of the html code for a webpage
#' @return A vector of result titles (10 per page)
#' @examples
#' titles <- get_titles(lines);
#' @export
get_titles <- function(html){
  Sys.setlocale('LC_ALL','C')
  y <- grep("gs_ri", html) #find location of lines containing GS title tag 'gs_ri'
  titles <- html[y] #extract lines
  titles <- (gsub("<.*?>", "", titles))[2:11] #remove code inside '<>'
  titles <- sub(" class=\"gs_ri\">", "", titles) #remove field codes
  titles <- gsub('\n', '', titles) #remove line break codes
  titles <- sub('.*] ', '', titles) #remove field codes
  titles <- gsub('<', '', titles) #remove field codes
  return(titles)
}


#' Extract citation information from Google Scholar results
#'
#' @description Extract information relating to the record's citation (including the authors,
#' journal/source and publication year), from Google Scholar search results.
#' @param html A vector of lines consisting of the html code for a webpage
#' @return A vector of citation information
#' @examples
#' citations <- get_citations(lines)
#' citations;
#' @export
get_citations <- function(html){
  y <- grep("gs_ri", html)
  citations <- html[y+1]
  citations <- (gsub("<.*?>", "", citations))[2:11]
  citations <- sub(" class=\"gs_a\">", "", citations)
  citations <- sub("</", "", citations)
  citations <- gsub('\n', '', citations) #remove line break codes
  citations <- mgsub::mgsub(citations, c('\302', '\240', '\342', '\200', '\246', '\303', '\241', '\305', '\201', '\242', '\223'), c('', '', '', '', '', '', '', '', '', '', ''))
  return(citations)
}


#' Extract article descriptions from Google Scholar results
#'
#' @description Extract article descriptions (summaries of key sentences), from Google
#' Scholar search results.
#' @param html A vector of lines consisting of the html code for a webpage
#' @return A vector of descriptions
#' @examples
#' descriptions <- get_descriptions(lines)
#' descriptions;
#' @export
get_descriptions <- function(html){
  y <- grep("gs_ri", html)
  descriptions <- html[y+3]
  descriptions <- (gsub("<.*?>", "", descriptions))[2:11]
  descriptions <- sub(" class=\"gs_rs\">", "", descriptions)
  descriptions <- sub("</", "", descriptions)
  descriptions <- mgsub::mgsub(descriptions, c('\302', '\240', '\342', '\200', '\246', '\303', '\241', '\305', '\201', '\242', '\223'), c('', '', '', '', '', '', '', '', '', '', ''))
  descriptions <- gsub('\n', '', descriptions) #remove line break codes
  return(descriptions)
}


#' Extract article descriptions from Google Scholar results
#'
#' @description Extract article publication years from Google Scholar search results.
#' @param html A vector of lines consisting of the html code for a webpage
#' @return A vector of publication years
#' @examples
#' years <- get_years(lines)
#' years;
#' @export
get_years <- function(html){
  y <- get_citations(html)
  year <- c()
  for(i in y){
    yr <- regmatches(i, gregexpr("\\b(19|20)\\d{2}\\b", i))
    year <- c(year, yr)
  }
  return(year)
}


#' Extract article links from Google Scholar results
#'
#' @description Extract links to websites holding article information from Google
#' Scholar search results.
#' @param html A vector of lines consisting of the html code for a webpage
#' @return A vector of URLs
#' @examples
#' links <- get_links(lines)
#' links;
#' @export
get_links <- function(html){
  y <- grep("gs_ri", html)
  links <- html[y]
  links <- sub('.*href=', '', (links)[2:11])
  links <- sub('\\ data.*', '', links)
  links <- gsub('\"', '', links)
  return(links)
}


#' Extract a DOI from an article link
#'
#' @description Extract a digital object identifier ('DOI') from the link to the website
#' holding article information, where they are contained within (e.g.
#' 'https://link.springer.com/article/10.1186/1475-2875-13-446' contains the DOI
#' 10.1186/1475-2875-13-446).
#' @param html A vector of lines consisting of the html code for a webpage
#' @return A doi
#' @examples
#' link <- 'http://scholarworks.uni.edu/cgi/viewcontent.cgi?article=1098&amp;context=pias'
#' doi <- link2doi(link)
#' doi
#' links <- c('http://scholarworks.uni.edu/cgi/viewcontent.cgi?article=1098&amp;context=pias',
#' 'https://www.sciencedirect.com/science/article/pii/S1050464883710429',
#' 'https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1890/110136',
#' 'https://europepmc.org/article/med/24364302',
#' 'https://onlinelibrary.wiley.com/doi/abs/10.1002/aqc.2470',
#' 'https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0042723',
#' 'https://www.nrcresearchpress.com/doi/abs/10.1139/f95-144',
#' 'https://bioone.org/journals/AMBIO-A-Journal-of-the-Human-Environment/volume-29/issue-1/0044-7447-29.1.45/Silicon-Retention-in-River-Basins--Far-reaching-Effects-on/10.1579/0044-7447-29.1.45.short',
#' 'https://www.jstor.org/stable/25037927',
#' 'https://www.tandfonline.com/doi/abs/10.1080/146349801317276152')
#' dois <- mapply(link2doi, links);
#' @export
link2doi <- function(link){
  if(grepl('10\\.', link) == TRUE){
    x <- stringr::str_extract(link, '10.*')
  } else {
    x <- NA
  }
  return(x)
}


#' Extract DOIs from multiple article links
#'
#' @description Extract digital object identifiers ('DOIs') from  links to the websites
#' holding article information, where they are contained within (e.g.
#' 'https://link.springer.com/article/10.1186/1475-2875-13-446' contains the DOI
#' 10.1186/1475-2875-13-446).
#' @param html A vector of lines consisting of the html code for a webpage
#' @return A vector of DOIs
#' @examples
#' dois <- links2dois(links)
#' dois;
#' @export
links2dois <- function(link){
  x <- as.data.frame(mapply(link2doi, links))
  return(x)
}
