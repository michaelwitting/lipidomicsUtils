#' @title Get all fatty acyls, alkyls and alkenyls
#' 
#' This functions isolates all fatty acyls, alkyls and alkenyls from a given lipid shorthand notation and returns them as vector. upported modifications are currently hydroxy groups (OH), hydroperoxy groups (OOH), keto groups (O) and amino groups (NH2)
#' 
#' @param lipid Shorthand notation of a acyl (as string), e.g. "PC(18:0/20:4(7E,9E,11Z,14Z)(5OH[S],6OH[R])"
#' @examples
#' library(lipidomicsUtils) 
#' isolate_fatty_acyls("PC(18:0/20:4(7E,9E,11Z,14Z)(5OH[S],6OH[R])")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#'
#' @export
isolate_fatty_acyls <- function(lipid) {
  
  .Deprecated("This function will be removed in the next version. Use isolate_radyls() instead.")
  
  # get all possible building blocks
  #fatty_acyls <- stringr::str_extract_all(lipid, "(m|d|t|O-|P-)*\\d+:\\d+(\\((\\d*(E|Z|Me|OH|OOH|O|NH2),*)*\\))*")[[1]]
  fatty_acyls <- stringr::str_extract_all(lipid, "(m|d|t|O-|P-)*\\d+:\\d+(\\((\\d*(E|Z|Me|OH|OOH|O|NH2|delta|\\d+-\\d+cy\\d+:\\d+(\\(\\d+\\))*)(\\[(S|R)\\])*,*)*\\))*")[[1]]
  
  # remove sphingoid bases
  filter <- stringr::str_detect(fatty_acyls, "(m|d|t)", negate = TRUE)
  fatty_acyls <- fatty_acyls[filter]
  
  return(fatty_acyls)
  
}

#' @title Get all fatty acyls, alkyls and alkenyls
#' 
#' This functions isolates all fatty acyls, alkyls and alkenyls from a given lipid shorthand notation and returns them as vector. upported modifications are currently hydroxy groups (OH), hydroperoxy groups (OOH), keto groups (O) and amino groups (NH2)
#' 
#' @param lipids List of vector of shorthand notation of a acyl (as string), e.g. "c(PC(18:0/20:4(7E,9E,11Z,14Z)), "PC(16:0/18:1(9Z))")
#' @examples
#' library(lipidomicsUtils) 
#' lipids <- c("PC(18:0/20:4(7E,9E,11Z,14Z))", "PC(16:0/18:1(9Z))")
#' isolate_radyls(lipids)
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#'
#' @export
isolate_radyls <- function(lipids) {
  
  # use of lapply for list and vectors
  radyls <- lapply(lipids, FUN = .isolate_radyls_helper)
  return(radyls)
  
}

# helper function to isolate radyls from a single lipid
.isolate_radyls_helper <- function(lipid) {
  
  # get all possible building blocks
  #fatty_acyls <- stringr::str_extract_all(lipid, "(m|d|t|O-|P-)*\\d+:\\d+(\\((\\d*(E|Z|Me|OH|OOH|O|NH2),*)*\\))*")[[1]]
  radyls <- stringr::str_extract_all(lipid, "(m|d|t|O-|P-)*\\d+:\\d+(\\((\\d*(E|Z|Me|OH|OOH|O|NH2|delta|\\d+-\\d+cy\\d+:\\d+(\\(\\d+\\))*)(\\[(S|R)\\])*,*)*\\))*")[[1]]
  
  # remove sphingoid bases
  filter <- stringr::str_detect(radyls, "(m|d|t)", negate = TRUE)
  radyls <- radyls[filter]
  
  # return result
  return(radyls)
}