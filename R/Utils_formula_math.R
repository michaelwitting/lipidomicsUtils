#' @title Check if one formula is contained in another
#' 
#' This function checks if one sum formula is contained in another.
#' 
#' @param target_chem_formula Single string with chemical formula
#' @param query_chem_formula Single string with chemical formula that should be contained in the targetFormula
#' 
#' @examples
#' library(lipidomicsUtils)
#' contains_formula("C6H12O6", "H2O")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#'
#' @seealso \code{\link{formula_subtration}}
#' @seealso \code{\link{formula_addition}}
#' 
#' @export
contains_formula <- function(target_chem_formula, query_chem_formula) {
  
  # parse both formmula
  target_formula_list <- formula_to_list(target_chem_formula)
  query_formula_list <- formula_to_list(query_chem_formula)
  
  # get atoms from query formula to check
  atoms <- names(query_formula_list)
  
  # return value
  contains <- FALSE
  
  for (atom in atoms) {
    if (is.na(target_formula_list[atom])) {
      return(FALSE)
    } else if (target_formula_list[atom] - query_formula_list[atom] >= 0) {
      contains <- TRUE
    }
  }
  
  return(contains)
}

#' @title subtract two chemical formula
#' 
#' This function subtracts one formula from another.
#' 
#' @param target_chem_formula Single string with chemical formula
#' @param query_chem_formula Single string with chemical formula that should be subtracted from the targetFormula
#' 
#' @examples
#' library(lipidomicsUtils)
#' formula_subtraction("C6H12O6", "H2O")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#'
#' @seealso \code{\link{contains_formula}}
#' @seealso \code{\link{formula_addition}}
#' 
#' @export
formula_subtraction <- function(target_chem_formula, query_chem_formula) {
  
  if(!contains_formula(target_chem_formula, query_chem_formula)) {
    stop(paste0(query_chem_formula, " not contained in ", target_chem_formula))
  }
  
  # parse both formmula
  target_formula_list <- formula_to_list(target_chem_formula)
  query_formula_list <- formula_to_list(query_chem_formula)
  
  # get all atoms from both formulas
  atoms <- unique(c(names(target_formula_list), names(query_formula_list)))
  
  # create new object for result
  result_formula_list <- rep(0, length(atoms))
  result_formula_list <- setNames(result_formula_list, atoms)

  for(atom in names(result_formula_list)) {
    
    target <- if(!is.na(target_formula_list[atom])) {
      target_formula_list[atom]
    } else {
      0
    }
    
    query <- if(!is.na(query_formula_list[atom])) {
      query_formula_list[atom]
    } else {
      0
    }

    result_formula_list[atom] <- target - query
      
  }

  return(list_to_formula(result_formula_list))
  
}

#' @title add two chemical formula
#' 
#' This function add one formula to another.
#' 
#' @param target_chem_formula Single string with chemical formula
#' @param query_chem_formula Single string with chemical formula
#' 
#' @examples
#' library(lipidomicsUtils)
#' formula_addition("C6H12O6", "H2O")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#'
#' @seealso \code{\link{contains_formula}}
#' @seealso \code{\link{formula_subtraction}}
#' 
#' @export
formula_addition <- function(target_chem_formula, query_chem_formula) {
  
  # parse both formmula
  target_formula_list <- formula_to_list(target_chem_formula)
  query_formula_list <- formula_to_list(query_chem_formula)
  
  # get all atoms from both formulas
  atoms <- unique(c(names(target_formula_list), names(query_formula_list)))
  
  # create new object for result
  result_formula_list <- rep(0, length(atoms))
  result_formula_list <- setNames(result_formula_list, atoms)
  
  for(atom in names(result_formula_list)) {
    
    target <- if(!is.na(target_formula_list[atom])) {
      target_formula_list[atom]
    } else {
      0
    }
    
    query <- if(!is.na(query_formula_list[atom])) {
      query_formula_list[atom]
    } else {
      0
    }
    
    result_formula_list[atom] <- target + query
    
  }
  
  return(list_to_formula(result_formula_list))
  
}
