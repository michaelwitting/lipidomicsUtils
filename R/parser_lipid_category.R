#' @title Determine the lipid category
#' 
#' This function determines the lipid category to which a lipid given as shorthand notation belongs
#' 
#' @param lipid Lipid for which the category shall be determined.
#' @examples 
#' library(lipidomicsUtils)
#' get_lipid_category("PC(16:0/18:1(9Z)")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#' 
#' @seealso \code{\link{get_lipid_mainclass}}
#' @seealso \code{\link{get_lipid_subclass}}
#'
#' @export
get_lipid_category <- function(lipid) {
  
  lipid_mainclass <- stringr::str_extract(lipid, "^([A-Za-z0-9])*-*([A-Za-z0-9])*")
  
  if(lipid_mainclass %in% c("FA", "CoA", "NAE", "PNAE", "GPNAE")) {
    
    lipid_category <- "FA"
    
  } else if(lipid_mainclass %in% c("MG", "DG", "TG")) {
    
    lipid_category <- "GL"
    
  } else if(lipid_mainclass %in% c("PC", "PE", "PS", "PG", "PGP", "PI", "PIP",
                                "PIP2", "PIP3", "PA", "PPA", "CL", "CDP-DG", "NAPE")) {
    
    lipid_category <- "GP"
    
  } else if(lipid_mainclass %in% c("SPH", "Cer", "S1P", "CerP", "SM", "GlcCer",
                               "GalCer", "LacCer", "HexCer", "Hex2Cer")) {
    
    lipid_category <- "SP"
    
  } else {
    
    stop("Unknown lipid")
    
  }
}

#' @title Determine the lipid main class
#' 
#' This function determines the lipid main class to which a lipid given as shorthand notation belongs
#' 
#' @param lipid Lipid for which the main class shall be determined.
#' @examples 
#' library(lipidomicsUtils)
#' get_lipid_category("PC(16:0/18:1(9Z)")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#' 
#' @seealso \code{\link{get_lipid_category}}
#' @seealso \code{\link{get_lipid_subclass}}
#'
#' @export
get_lipid_mainclass <- function(lipid) {
  
  lipid_mainclass <- stringr::str_extract(lipid, "^([A-Za-z0-9])*-*([A-Za-z0-9])*")
  
  return(lipid_mainclass)
  
}

#' @title Determine the lipid sub class
#' 
#' This function determines the lipid sub class to which a lipid given as shorthand notation belongs
#' 
#' @param lipid Lipid for which the sub class shall be determined.
#' @examples 
#' library(lipidomicsUtils)
#' get_lipid_category("PC(16:0/18:1(9Z)")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#' 
#' @seealso \code{\link{get_lipid_category}}
#' @seealso \code{\link{get_lipid_mainclass}}
#'
#' @export
get_lipid_subclass <- function(lipid) {
  
  lipid_subclass <- stringr::str_extract(lipid, "^([A-Za-z0-9])*-*([A-Za-z0-9])*(\\((O|P))*")
  lipid_subclass <- stringr::str_replace(lipid_subclass, "\\(", "-")
  
  return(lipid_subclass)
  
}