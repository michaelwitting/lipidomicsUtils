#' @title Lipid category (according to LIPIDMAPS)
#'
#' @export
get_lipid_category <- function(lipid) {
  
  lipid_class <- stringr::str_extract(lipid, "^([A-Za-z0-9])*-*([A-Za-z0-9])*")
  
  if(lipid_class %in% c("FA", "CoA", "NAE")) {
    
    lipid_category <- "FA"
    
  } else if(lipid_class %in% c("MG", "DG", "TG")) {
    
    lipid_category <- "GL"
    
  } else if(lipid_class %in% c("PC", "PE", "PS", "PG", "PGP", "PI", "PIP",
                                "PIP2", "PIP3", "PA", "PPA", "CL", "CDP-DG")) {
    
    lipid_category <- "GP"
    
  } else if(lipid_class %in% c("SPH", "Cer", "S1P", "C1P", "SM", "GlcCer",
                               "GalCer", "LacCer", "HexCer", "Hex2Cer")) {
    
    lipid_category <- "SP"
    
  } else {
    
    stop("Unknown lipid")
    
  }
  
  
  
  
}

#' @title Lipid class (according to LIPIDMAPS)
#'
#' @export
get_lipid_mainclass <- function(lipid, lmid = FALSE) {
  
  lipid_main_class <- stringr::str_extract(lipid, "^([A-Za-z0-9])*-*([A-Za-z0-9])*")
  
  return(lipid_mainclass)
  
}

#' @title Lipid subclass (according to LIPIDMAPS)
#'
#' @export
get_lipid_subclass <- function(lipid, lmid = FALSE) {
  
  lipid_class <- stringr::str_extract(lipid, "^([A-Za-z0-9])*-*([A-Za-z0-9])*(\\((O|P))*")
  lipid_class <- stringr::str_replace(lipid_class, "\\(", "-")
  
  # return abbrevation or LMID
  if(lmid) {
    
    # get list with LMIDs
    lmid_list <- .get_lmid_list()
    
  } else {
    
    return(lipid_class)
    
  }
  
  return(lipid_class)
  
}


#'
#'
#' @export
.get_lmid_list <- function() {
  
  lmid_list <- list(
    "MG" = "GL0101",
    "MG-O" = "GL0102",
    "MG-P" = "GL0103",
    "DG" = "GL0201",
    "DG-O" = "",
    "DG-P" = "",
    "DG-O-O" = "",
    "DG-P-P" = "",
    "DG-O-P" = "",
    "DG-P-O" = "",
    "TG" = "GL0301"
  )
  
}