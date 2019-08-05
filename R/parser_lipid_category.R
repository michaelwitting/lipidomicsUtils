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
get_lipid_mainclass <- function(lipid) {
  
  lipid_mainclass <- stringr::str_extract(lipid, "^([A-Za-z0-9])*-*([A-Za-z0-9])*")
  
  return(lipid_mainclass)
  
}

#' @title Lipid subclass (according to LIPIDMAPS)
#'
#' @export
get_lipid_subclass <- function(lipid) {
  
  lipid_subclass <- stringr::str_extract(lipid, "^([A-Za-z0-9])*-*([A-Za-z0-9])*(\\((O|P))*")
  lipid_subclass <- stringr::str_replace(lipid_subclass, "\\(", "-")
  
  return(lipid_subclass)
  
}