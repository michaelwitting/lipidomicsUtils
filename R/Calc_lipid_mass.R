#' @title Calculation of mass of intact lipids
#' 
#' This function calcluates the mass of an intact lipid. Supported modifications are currently hydroxy groups (OH), hydroperoxy groups (OOH), keto groups (O) and amino groups (NH2).
#' 
#' @param lipid Lipid for which the mass shall be calculated.
#' @examples 
#' library(lipidomicsUtils)
#' calc_lipid_mass("CoA(18:1(9Z))")
#' calc_lipid_mass("PC(16:0/18:1(9Z))")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#'
#' @export
calc_lipid_mass <- function(lipid) {
  
  # get main class
  lipid_mainclass <- lipidomicsUtils::get_lipid_mainclass(lipid)
  
  # check class and calculate lipid mass
  if(lipid_mainclass == "FA") {
    
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- sum(unlist(lapply(fatty_acyls, calc_intact_acyl_mass)))
    
  } else if(lipid_mainclass == "CoA") {
    
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils::coa_mass + 
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_mainclass == "NAE") {
    
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils::ethanolamine_mass + 
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_mainclass == "PNAE") {
    
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils::ethanolamine_mass + 
      lipidomicsUtils::h3po4_mass -
      lipidomicsUtils::water_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_mainclass == "GPNAE") {
    
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils::ethanolamine_mass + 
      lipidomicsUtils::h3po4_mass -
      lipidomicsUtils::water_mass +
      lipidomicsUtils::glycerol_mass -
      lipidomicsUtils::water_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_mainclass == "MG") {
    
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils::glycerol_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_mainclass == "DG") {
    
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils:::glycerol_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_mainclass == "TG") {
    
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils:::glycerol_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_mainclass == "PC") {
    
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils:::gpc_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_mainclass == "PE") {
    
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils:::gpe_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_mainclass == "PS") {
    
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils:::gps_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_mainclass == "PG") {
    
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils:::gpg_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_mainclass == "PGP") {
    
  } else if(lipid_mainclass == "PI") {
    
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils:::gpi_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_mainclass == "PIP") {
    
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils:::gpi_mass +
      lipidomicsUtils::h3po4_mass -
      lipidomicsUtils::water_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_mainclass == "PIP2") {
    
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils:::gpi_mass +
      2 * lipidomicsUtils::h3po4_mass -
      2 * lipidomicsUtils::water_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_mainclass == "PIP3") {
    
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils:::gpi_mass +
      3 * lipidomicsUtils::h3po4_mass -
      3 * lipidomicsUtils::water_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_mainclass == "PA") {
    
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils:::pg_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_mainclass == "PPA") {
    
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils:::pg_mass +
      lipidomicsUtils::h3po4_mass -
      lipidomicsUtils::water_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_mainclass == "CL") {
    
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils:::cl_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_mainclass == "CDP-DG") {
    
  } else if(lipid_mainclass == "NAPE") {
    
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils:::gpe_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_mainclass == "SPH") {
    
    sphingoid_base <- lipidomicsUtils::isolate_sphingoid_base(lipid)
    
    lipid_mass <- lipidomicsUtils::calc_sphingoid_mass(sphingoid_base)
    
  } else if(lipid_mainclass == "Cer") {
    
    sphingoid_base <- lipidomicsUtils::isolate_sphingoid_base(lipid)
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils::calc_sphingoid_mass(sphingoid_base) +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_mainclass == "S1P") {
    
    sphingoid_base <- lipidomicsUtils::isolate_sphingoid_base(lipid)
    
    lipid_mass <- lipidomicsUtils::calc_sphingoid_mass(sphingoid_base) +
      lipidomicsUtils:::h3po4_mass -
      lipidomicsUtils:::water_mass
    
  } else if(lipid_mainclass == "CerP") {
    
    sphingoid_base <- lipidomicsUtils::isolate_sphingoid_base(lipid)
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils::calc_sphingoid_mass(sphingoid_base) +
      lipidomicsUtils:::h3po4_mass -
      lipidomicsUtils:::water_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_mainclass == "SM") {
    
    sphingoid_base <- lipidomicsUtils::isolate_sphingoid_base(lipid)
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils::calc_sphingoid_mass(sphingoid_base) +
      lipidomicsUtils:::pc_mass -
      lipidomicsUtils:::water_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_mainclass == "GlcCer") {
    
    sphingoid_base <- lipidomicsUtils::isolate_sphingoid_base(lipid)
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils::calc_sphingoid_mass(sphingoid_base) +
      lipidomicsUtils:::hexose_mass -
      lipidomicsUtils:::water_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_mainclass == "GalCer") {
    
    sphingoid_base <- lipidomicsUtils::isolate_sphingoid_base(lipid)
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils::calc_sphingoid_mass(sphingoid_base) +
      lipidomicsUtils:::hexose_mass -
      lipidomicsUtils:::water_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
    
  } else if(lipid_mainclass == "LacCer") {
    
    sphingoid_base <- lipidomicsUtils::isolate_sphingoid_base(lipid)
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils::calc_sphingoid_mass(sphingoid_base) +
      lipidomicsUtils:::dihexose_mass -
      lipidomicsUtils:::water_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
    
  } else if(lipid_mainclass == "HexCer") {
    
    sphingoid_base <- lipidomicsUtils::isolate_sphingoid_base(lipid)
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils::calc_sphingoid_mass(sphingoid_base) +
      lipidomicsUtils:::hexose_mass -
      lipidomicsUtils:::water_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  } else if(lipid_mainclass == "Hex2Cer") {
    
    sphingoid_base <- lipidomicsUtils::isolate_sphingoid_base(lipid)
    fatty_acyls <- lipidomicsUtils::isolate_fatty_acyls(lipid)
    
    lipid_mass <- lipidomicsUtils::calc_sphingoid_mass(sphingoid_base) +
      lipidomicsUtils:::dihexose_mass -
      lipidomicsUtils:::water_mass +
      sum(unlist(lapply(fatty_acyls, calc_residue_acyl_mass)))
    
  }
  
  return(lipid_mass)
  
}
