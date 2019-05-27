get_lipid_category <- function(lipid) {
  
  lipid_class <- stringr::str_extract(lipid, "^([A-Z])*-*([A-Z])*")
  
  if(lipid_class %in% c("PC", "PE", "PS", "PG", "PGP", "PI", "PIP",
                        "PIP2", "PIP3", "PA", "PPA", "CL", "CDP-DG")) {
    lipid_category <- "GP"
  }
  
  return(lipid_category)
}

get_lipid_class <- function(lipid) {
  
  lipid_class <- stringr::str_extract(lipid, "^([A-Z])*-*([A-Z])*")
  
  return(lipid_class)
  
}

get_lipid_subclass <- function(lipid) {
  
  lipid_class <- stringr::str_extract(lipid, "^([A-Z])*-*([A-Z])*(\\((O|P))*")
  lipid_class <- stringr::str_replace(lipid_class, "\\(", "-")
  
  return(lipid_class)
  
}
