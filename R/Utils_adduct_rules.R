#' @title Create list with all adduct calculation rules (all rules)
#' 
#' This function returns a list with rules for the calculation of adducts. It is required for the calculation of adduct m/z values from exact masses and the other way round. This list contains all rules for both ionization modes.
#' 
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#'
#' @seealso \code{\link{adduct_rules_pos}}
#' @seealso \code{\link{adduct_rules_neg}}
#' @seealso \code{\link{get_adduct_names}}
#'
#' @export
adduct_rules <- function() {
  
  ## create list with all the adduct definitoins
  adduct_list <- metabolomicsUtils::adduct_rules()
  
  ## return values
  return(adduct_list)
  
}


#' @title Create list with all adduct calculation rules (positive ionisation)
#' 
#' This function returns a list with rules for the calculation of adducts. It is required for the calculation of adduct m/z values from exact masses and the other way round. This list contains all rules for the positive ionization mode.
#' 
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#'
#' @seealso \code{\link{adduct_rules}}
#' @seealso \code{\link{adduct_rules_neg}}
#' @seealso \code{\link{get_adduct_names}}
#'
#' @export
adduct_rules_pos <- function() {
  
  ## create list with all the adduct definitoins
  adduct_list <- metabolomicsUtils::adduct_rules_pos()
  
  ## return values
  return(adduct_list)
  
}

#' @title Create list with all adduct calculation rules (negative ion model)
#' 
#' This function returns a list with rules for the calculation of adducts. It is required for the calculation of adduct m/z values from exact masses and the other way round. This list contains all rules for the negative ionization mode.
#' 
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#'
#' @seealso \code{\link{adduct_rules}}
#' @seealso \code{\link{adduct_rules_pos}}
#' @seealso \code{\link{get_adduct_names}}
#'
#' @export
adduct_rules_neg <- function() {
  
  ## create list with all the adduct definitoins
  adduct_list <- metabolomicsUtils::adduct_rules_neg()
  
  ## return values
  return(adduct_list)
  
}

#' @title Return current adduct naming
#' 
#' This function returns either all or only positive or negative ionization mode adduct names used in this package.
#' 
#' @param mode Ionization mode for which the adduct names shall be returned, either \code{all}, \code{positive} or \code{negative}
#' 
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#'
#' @seealso \code{\link{adduct_rules}}
#' @seealso \code{\link{adduct_rules_pos}}
#' @seealso \code{\link{adduct_rules_neg}}
#'
#' @export
get_adduct_names <- function(mode = c("all", "positive", "negative")) {
  
  # get adduct lists
  adduct_list_neg <- metabolomicsUtils::adduct_rules_neg()
  adduct_list_pos <- metabolomicsUtils::adduct_rules_pos()
  
  if(mode == "all") {
    
    return(c(names(adduct_list_neg), names(adduct_list_pos)))
    
  } else if(mode == "positive") {
    
    return(names(adduct_list_pos))
    
  } else if(mode == "negative") {
    
    return(names(adduct_list_neg))
    
  }
}