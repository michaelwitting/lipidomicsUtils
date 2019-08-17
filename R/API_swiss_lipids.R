#' @title  Search for SwissLipids ID from InChIKey (exact)
#' 
#' This function uses the SwissLipids API to search for a SwissLipids ID from a given InChIKey. An exact match of the InChIKey is expected.
#' 
#' @param inchikey InChIKey for which a SwissLipids ID shall be searched for
#' @examples 
#' library(lipidomicsUtils)
#' inchikey_to_swissid_exact("WTJKGGKOPKCXLL-VYOBOKEXSA-N")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#'
#' @seealso \code{\link{inchikey_to_swissid_nocharge}}
#' @seealso \code{\link{inchikey_to_swissid_structure}}
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

#' @title  Search for SwissLipids ID from InChIKey (ignoring charge state)
#' 
#' This function uses the SwissLipids API to search for a SwissLipids ID from a given InChIKey. The charge state is ignored and only the atom connectivity and stereochemistry block are used..
#' 
#' @param inchikey InChIKey for which a SwissLipids ID shall be searched for
#' @examples 
#' library(lipidomicsUtils)
#' inchikey_to_swissid_nocharge("WTJKGGKOPKCXLL-VYOBOKEXSA-N")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#'
#' @seealso \code{\link{inchikey_to_swissid_exact}}
#' @seealso \code{\link{inchikey_to_swissid_structure}}
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

#' @title  Search for SwissLipids ID from InChIKey (only atom connectivity)
#' 
#' This function uses the SwissLipids API to search for a SwissLipids ID from a given InChIKey. Only the atom connectivity block is used.
#' 
#' @param inchikey InChIKey for which a SwissLipids ID shall be searched for
#' @examples 
#' library(lipidomicsUtils)
#' inchikey_to_swissid_structure("WTJKGGKOPKCXLL-VYOBOKEXSA-N")
#' 
#'  @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#'
#' @seealso \code{\link{inchikey_to_swissid_exact}}
#' @seealso \code{\link{inchikey_to_swissid_nocharge}}
#'
#' @export
inchikey_to_swissid_structure <- function(inchikey) {
  
  # clean inchikey
  inchikey <- stringr::str_remove_all(inchikey, "InChIkey=|InChIKey=")
  
  # remove charge
  inchikey <- stringr::str_sub(inchikey, 1, -14)
  
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
