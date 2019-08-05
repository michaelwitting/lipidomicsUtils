#'
#'
#' @export
calc_lipid_mass <- function(lipid) {
  
  lipid_class <- stringr::str_extract(lipid, "^([A-Za-z0-9])*-*([A-Za-z0-9])*")
  
  if(lipid_class == "PC") {
    
    fatty_acyls <- isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils:::gpc_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_class == "PE") {
    
    fatty_acyls <- isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils:::gpe_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_class == "PS") {
    
    fatty_acyls <- isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils:::gps_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_class == "PG") {
    
    fatty_acyls <- isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils:::gpg_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_class == "PI") {
    
    fatty_acyls <- isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils:::gpi_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  }
  
  return(lipid_mass)
  
}
