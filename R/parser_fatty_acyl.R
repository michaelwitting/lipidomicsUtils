isolate_fatty_acyls <- function(lipid) {
  
  # get all possible building blocks
  #fatty_acyls <- stringr::str_extract_all(lipid, "(m|d|t|O-|P-)*(\\d+:\\d+(\\((\\d*(E|Z|Me|OH),*\\))*)*)")[[1]]
  fatty_acyls <- stringr::str_extract_all(lipid, "(m|d|t|O-|P-)*\\d+:\\d+(\\((\\d*(E|Z|Me|OH),*)*\\))*")[[1]]
  
  
  # remove sphingoid bases
  filter <- stringr::str_detect(fatty_acyls, "(m|d|t)", negate = TRUE)
  fatty_acyls <- fatty_acyls[filter]
  
  return(fatty_acyls)
  
}

get_carbon_number <- function(fatty_acyl) {
  
  base_fatty_acyl <- stringr::str_extract(fatty_acyl, "\\d+:\\d+")[[1]][1]
  methyl_branches <- stringr::str_count(fatty_acyl, "Me")
  
  carbon_number <-
    as.numeric(stringr::str_split(base_fatty_acyl, ":")[[1]][1]) +
    methyl_branches
  
  return(carbon_number)
  
}

get_bond_number <- function(fatty_acyl) {
  
  base_fatty_acyl <- stringr::str_extract(fatty_acyl, "\\d+:\\d+")[[1]][1]
  
  bond_number <-
    as.numeric(stringr::str_split(base_fatty_acyl, ":")[[1]][2])
  
  return(bond_number)
  
}