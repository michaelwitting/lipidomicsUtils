#'
#'
#'
#' @export
inchikey_to_swissid_exact <- function(inchikey) {
  
  # clean inchikey
  inchikey <- stringr::str_remove_all(inchikey, "InChIkey=|InChIKey=")
  
  # create query URL
  query_url <- paste0("https://www.swisslipids.org/api/index.php/advancedSearch?InChIkey=", inchikey)
  
  # perform GET request
  result <- httr::GET(query_url)
  
  # check result, if not 200 then failure
  if(!result$status_code == 200) {
    return(NA)
  }
  
  # parse results
  result_text <- httr::content(result, "text")
  result_json <- jsonlite::fromJSON(result_text, flatten = TRUE)
  
  # return id
  return(result_json$entity_id)
}

#'
#'
#'
#' @export
inchikey_to_swissid_nocharge <- function(inchikey) {
  
  # clean inchikey
  inchikey <- stringr::str_remove_all(inchikey, "InChIkey=|InChIKey=")
  
  # remove charge
  inchikey <- stringr::str_sub(inchikey, 1, -3)
  
  # create query URL
  query_url <- paste0("https://www.swisslipids.org/api/index.php/search?term=", inchikey)
  
  # perform GET request
  result <- httr::GET(query_url)
  
  # check result, if not 200 then failure
  if(!result$status_code == 200) {
    return(NA)
  }
  
  # parse results
  result_text <- httr::content(result, "text")
  result_json <- jsonlite::fromJSON(result_text, flatten = TRUE)
  
  # return id
  return(result_json$entity_id)
}
