#' @title Get all fatty acyls
#'
#' @export
isolate_sphingoid_base <- function(lipid) {
  
  # get all possible building blocks
  sphingoid_base <- stringr::str_extract_all(lipid, "(m|d|t|O-|P-)*\\d+:\\d+(\\((\\d*(E|Z|Me|OH),*)*\\))*")[[1]]
  
  # remove sphingoid bases
  filter <- stringr::str_detect(sphingoid_base, "(m|d|t)", negate = FALSE)
  sphingoid_base <- sphingoid_base[filter]
  
  return(sphingoid_base)
  
}