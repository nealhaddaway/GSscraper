#' Decode DOIs (digital object identifiers)
#' 
#' @description Replace URL encoded DOIs (often exported from bibliographic 
#' databases and search tools like Publish or Perish, e.g. '%28' = '(') 
#' with original special character encoding (e.g. '(').
#' @param dois A vector of DOIs.
#' @param keep_http Logical argument specifying whether to retain http 
#' prefix (TRUE) or not (FALSE). Default is set to TRUE.
#' @importFrom dplyr mutate
#' @importFrom stringi stri_replace_all_fixed
#' @return A vector of edited DOIs
#' @examples \dontrun{
#' refs <- synthesisr::read_refs('www/embase.ris')
#' dois <- refs$doi
#' new_dois <- decode_dois(dois, keep_http = FALSE)
#' }
#' @export
decode_dois <- function(dois,
                        keep_http = TRUE){
  
  if(keep_http == FALSE){
    #remove trailing '/' and http stem
    dois <- gsub("/$", '', dois)
    dois <- gsub('http://dx.doi.org/', '', dois)
    dois <- gsub('http://doi.org/', '', dois)
    dois <- gsub('http://www.doi.org/', '', dois)
    dois <- gsub('https://dx.doi.org/', '', dois)
    dois <- gsub('https://doi.org/', '', dois)
    dois <- gsub('https://www.doi.org/', '', dois)
    dois <- gsub('dx.doi.org/', '', dois)
    dois <- gsub('doi.org/', '', dois)
    dois <- gsub('www.doi.org/', '', dois)
  }
  
  #convert to df
  dois <- data.frame(dois)
  
  #load lookup_table
  lookup_table <- read.delim('www/lookup_table.txt',
                             sep = '\t', 
                             header =  TRUE, 
                             quote = '')
  
  #replace characters using lookup table
  dois <- dois %>% 
    dplyr::mutate(across(c(dois), ~ stringi::stri_replace_all_fixed(
      str = .x,
      pattern = lookup_table[["From.UTF.8"]],
      replacement = lookup_table[["Character"]],
      vectorise_all = FALSE
    )))
  
  #convert back to vector
  dois <- dois$dois
  
  return(dois)
  
}
