#' @title Calculation of mass of intact lipids
#' 
#' This function calcluates the mass of an intact lipid. Supported modifications are currently hydroxy groups (OH), hydroperoxy groups (OOH), keto groups (O) and amino groups (NH2).
#' 
#' @param lipid Lipid for which the mass shall be calculated.
#' @examples 
#' library(lipidomicsUtils)
#' calc_lipid_formulae("CoA(18:1(9Z))")
#' calc_lipid_formulae("PC(16:0/18:1(9Z))")
#' 
#' @author Michael Witting, \email{michael.witting@@helmholtz-muenchen.de}
#' 
#' @import MetaboCoreUtils
#'
#' @export
calc_lipid_formulae <- function(lipids) {
  
  sapply(lipids, FUN = .calc_lipid_formula_helper)
  
}

#'
#'
#' @noRd
.calc_lipid_formula_helper <- function(lipid) {
  
  # get main class
  lipid_mainclass <- lipidomicsUtils::get_lipid_mainclass(lipid)
  
  # check class and calculate lipid mass
  if(lipid_mainclass == "FA") {
    
    fatty_acyls <- unlist(lipidomicsUtils::isolate_radyls(lipid))
    
    lipid_formula <- calc_intact_acyl_formula(fatty_acyls[1])
    
  } else if(lipid_mainclass == "CoA") {
    
    fatty_acyls <- unlist(lipidomicsUtils::isolate_radyls(lipid))
    
    lipid_formula <- addElements(lipidomicsUtils:::coa_formula,
                                 unlist(lapply(fatty_acyls, calc_residue_acyl_formula)))

    
  } else if(lipid_mainclass == "NAE") {
    
    fatty_acyls <- unlist(lipidomicsUtils::isolate_radyls(lipid))
    
    lipid_formula <- addElements(lipidomicsUtils:::ethanolamine_formula,
                                 unlist(lapply(fatty_acyls, calc_residue_acyl_formula)))
    
  } else if(lipid_mainclass == "PNAE") {
    
    fatty_acyls <- unlist(lipidomicsUtils::isolate_radyls(lipid))
    
    lipid_formula <- addElements(lipidomicsUtils:::ethanolamine_formula,
                                 c(unlist(lapply(fatty_acyls, calc_residue_acyl_formula)),
                                   lipidomicsUtils:::h3po4_formula))
    
    lipid_formula <- subtractElements(lipid_formula,
                                      lipidomicsUtils:::water_formula)
    
  } else if(lipid_mainclass == "GPNAE") {
    
    fatty_acyls <- unlist(lipidomicsUtils::isolate_radyls(lipid))
    
    lipid_formula <- addElements(lipidomicsUtils:::ethanolamine_formula,
                                 c(unlist(lapply(fatty_acyls, calc_residue_acyl_formula)),
                                   lipidomicsUtils:::h3po4_formula,
                                   lipidomicsUtils:::glycerol_formula))
    
    lipid_formula <- subtractElements(lipid_formula,
                                      c(lipidomicsUtils:::water_formula,
                                        lipidomicsUtils:::water_formula))
    
  } else if(lipid_mainclass %in% c("MG", "DG", "TG")) {
    
    fatty_acyls <- unlist(lipidomicsUtils::isolate_radyls(lipid))
    
    lipid_formula <- addElements(lipidomicsUtils:::glycerol_formula,
                                 unlist(lapply(fatty_acyls, calc_residue_acyl_formula)))
    
  } else if(lipid_mainclass == "PC") {
    
    fatty_acyls <- unlist(lipidomicsUtils::isolate_radyls(lipid))
    
    lipid_formula <- addElements(lipidomicsUtils:::gpc_formula,
                                 unlist(lapply(fatty_acyls, calc_residue_acyl_formula)))
    
  } else if(lipid_mainclass == "PE") {
    
    fatty_acyls <- unlist(lipidomicsUtils::isolate_radyls(lipid))
    
    lipid_formula <- addElements(lipidomicsUtils:::gpe_formula,
                                 unlist(lapply(fatty_acyls, calc_residue_acyl_formula)))
    
  } else if(lipid_mainclass == "PS") {
    
    fatty_acyls <- unlist(lipidomicsUtils::isolate_radyls(lipid))
    
    lipid_formula <- addElements(lipidomicsUtils:::gps_formula,
                                 unlist(lapply(fatty_acyls, calc_residue_acyl_formula)))
    
  } else if(lipid_mainclass == "PG") {
    
    fatty_acyls <- unlist(lipidomicsUtils::isolate_radyls(lipid))
    
    lipid_formula <- addElements(lipidomicsUtils:::gpg_formula,
                                 unlist(lapply(fatty_acyls, calc_residue_acyl_formula)))
    
  } else if(lipid_mainclass == "PGP") {
    
  } else if(lipid_mainclass == "PI") {
    
    fatty_acyls <- unlist(lipidomicsUtils::isolate_radyls(lipid))
    
    lipid_formula <- addElements(lipidomicsUtils:::gpi_formula,
                                 unlist(lapply(fatty_acyls, calc_residue_acyl_formula)))
    
  } else if(lipid_mainclass == "PIP") {
    
    fatty_acyls <- unlist(lipidomicsUtils::isolate_radyls(lipid))
    
    lipid_formula <- addElements(lipidomicsUtils:::gpi_formula,
                                 c(unlist(lapply(fatty_acyls, calc_residue_acyl_formula)),
                                   lipidomicsUtils:::h3po4_formula))
    
    lipid_formula <- subtractElements(lipid_formula,
                                      lipidomicsUtils:::water_formula)
    
  } else if(lipid_mainclass == "PIP2") {
    
    fatty_acyls <- unlist(lipidomicsUtils::isolate_radyls(lipid))
    
    lipid_formula <- addElements(lipidomicsUtils:::gpi_formula,
                                 c(unlist(lapply(fatty_acyls, calc_residue_acyl_formula)),
                                   lipidomicsUtils:::h3po4_formula,
                                   lipidomicsUtils:::h3po4_formula))
    
    lipid_formula <- subtractElements(lipid_formula,
                                      c(lipidomicsUtils:::water_formula,
                                        lipidomicsUtils:::water_formula))
    
  } else if(lipid_mainclass == "PIP3") {
    
    fatty_acyls <- unlist(lipidomicsUtils::isolate_radyls(lipid))
    
    lipid_formula <- addElements(lipidomicsUtils:::gpi_formula,
                                 c(unlist(lapply(fatty_acyls, calc_residue_acyl_formula)),
                                   lipidomicsUtils:::h3po4_formula,
                                   lipidomicsUtils:::h3po4_formula,
                                   lipidomicsUtils:::h3po4_formula))
    
    lipid_formula <- subtractElements(lipid_formula,
                                      c(lipidomicsUtils:::water_formula,
                                        lipidomicsUtils:::water_formula,
                                        lipidomicsUtils:::water_formula))
    
  } else if(lipid_mainclass == "PA") {
    
    fatty_acyls <- unlist(lipidomicsUtils::isolate_radyls(lipid))
    
    lipid_formula <- addElements(lipidomicsUtils:::pg_formula,
                                 unlist(lapply(fatty_acyls, calc_residue_acyl_formula)))
    
  } else if(lipid_mainclass == "PPA") {
    
    fatty_acyls <- unlist(lipidomicsUtils::isolate_radyls(lipid))
    
    lipid_formula <- addElements(lipidomicsUtils:::pg_formula,
                                 c(unlist(lapply(fatty_acyls, calc_residue_acyl_formula)),
                                   lipidomicsUtils:::h3po4_formula))
    
    lipid_formula <- subtractElements(lipid_formula,
                                      lipidomicsUtils:::water_formula)
    
  } else if(lipid_mainclass == "CL") {
    
    fatty_acyls <- unlist(lipidomicsUtils::isolate_radyls(lipid))
    
    lipid_formula <- addElements(lipidomicsUtils:::cl_formula,
                                 unlist(lapply(fatty_acyls, calc_residue_acyl_formula)))
    
  } else if(lipid_mainclass == "CDP-DG") {
    
  } else if(lipid_mainclass == "NAPE") {
    
    fatty_acyls <- unlist(lipidomicsUtils::isolate_radyls(lipid))
    
    lipid_formula <- addElements(lipidomicsUtils:::gpe_formula,
                                 unlist(lapply(fatty_acyls, calc_residue_acyl_formula)))
    
  } else if(lipid_mainclass == "SPH") {
    
    sphingoid_base <- unlist(lipidomicsUtils::isolate_sphingoids(lipid))

    lipid_formula <- lipidomicsUtils::calc_sphingoid_formula(sphingoid_base)
    
  } else if(lipid_mainclass == "Cer") {
    
    sphingoid_base <- unlist(lipidomicsUtils::isolate_sphingoids(lipid))
    fatty_acyls <- unlist(lipidomicsUtils::isolate_radyls(lipid))
    
    lipid_formula <- addElements(lipidomicsUtils::calc_sphingoid_formula(sphingoid_base),
                                 unlist(lapply(fatty_acyls, calc_residue_acyl_formula)))
    
  } else if(lipid_mainclass == "S1P") {
    
    sphingoid_base <- unlist(lipidomicsUtils::isolate_sphingoids(lipid))
    
    lipid_formula <- addElements(lipidomicsUtils::calc_sphingoid_formula(sphingoid_base),
                                 lipidomicsUtils:::h3po4_formula)
    
    lipid_formula <- subtractElements(lipid_formula,
                                      lipidomicsUtils:::water_formula)
    
  } else if(lipid_mainclass == "CerP") {
    
    sphingoid_base <- unlist(lipidomicsUtils::isolate_sphingoids(lipid))
    fatty_acyls <- unlist(lipidomicsUtils::isolate_radyls(lipid))
    
    lipid_formula <- addElements(lipidomicsUtils::calc_sphingoid_formula(sphingoid_base),
                                 c(unlist(lapply(fatty_acyls, calc_residue_acyl_formula)),
                                   lipidomicsUtils:::h3po4_formula))
    
    lipid_formula <- subtractElements(lipid_formula,
                                      lipidomicsUtils:::water_formula)
    
  } else if(lipid_mainclass == "SM") {
    
    sphingoid_base <- unlist(lipidomicsUtils::isolate_sphingoids(lipid))
    fatty_acyls <- unlist(lipidomicsUtils::isolate_radyls(lipid))
    
    lipid_formula <- addElements(lipidomicsUtils::calc_sphingoid_formula(sphingoid_base),
                                 c(unlist(lapply(fatty_acyls, calc_residue_acyl_formula)),
                                   lipidomicsUtils:::pc_formula))
    
    lipid_formula <- subtractElements(lipid_formula,
                                      lipidomicsUtils:::water_formula)
    
  } else if(lipid_mainclass == "GlcCer") {
    
    sphingoid_base <- unlist(lipidomicsUtils::isolate_sphingoids(lipid))
    fatty_acyls <- unlist(lipidomicsUtils::isolate_radyls(lipid))
    
    lipid_formula <- addElements(lipidomicsUtils::calc_sphingoid_formula(sphingoid_base),
                                 c(unlist(lapply(fatty_acyls, calc_residue_acyl_formula)),
                                   lipidomicsUtils:::hexose_formula))
    
    lipid_formula <- subtractElements(lipid_formula,
                                      lipidomicsUtils:::water_formula)
    
  } else if(lipid_mainclass == "GalCer") {
    
    sphingoid_base <- unlist(lipidomicsUtils::isolate_sphingoids(lipid))
    fatty_acyls <- unlist(lipidomicsUtils::isolate_radyls(lipid))
    
    lipid_formula <- addElements(lipidomicsUtils::calc_sphingoid_formula(sphingoid_base),
                                 c(unlist(lapply(fatty_acyls, calc_residue_acyl_formula)),
                                   lipidomicsUtils:::hexose_formula))
    
    lipid_formula <- subtractElements(lipid_formula,
                                      lipidomicsUtils:::water_formula)
    
  } else if(lipid_mainclass == "LacCer") {
    
    sphingoid_base <- unlist(lipidomicsUtils::isolate_sphingoids(lipid))
    fatty_acyls <- unlist(lipidomicsUtils::isolate_radyls(lipid))
    
    lipid_formula <- addElements(lipidomicsUtils::calc_sphingoid_formula(sphingoid_base),
                                 c(unlist(lapply(fatty_acyls, calc_residue_acyl_formula)),
                                   lipidomicsUtils:::dihexose_formula))
    
    lipid_formula <- subtractElements(lipid_formula,
                                      lipidomicsUtils:::water_formula)
    
    
  } else if(lipid_mainclass == "HexCer") {
    
    sphingoid_base <- unlist(lipidomicsUtils::isolate_sphingoids(lipid))
    fatty_acyls <- unlist(lipidomicsUtils::isolate_radyls(lipid))
    
    lipid_formula <- addElements(lipidomicsUtils::calc_sphingoid_formula(sphingoid_base),
                                 c(unlist(lapply(fatty_acyls, calc_residue_acyl_formula)),
                                   lipidomicsUtils:::hexose_formula))
    
    lipid_formula <- subtractElements(lipid_formula,
                                      lipidomicsUtils:::water_formula)
    
  } else if(lipid_mainclass == "Hex2Cer") {
    
    sphingoid_base <- unlist(lipidomicsUtils::isolate_sphingoids(lipid))
    fatty_acyls <- unlist(lipidomicsUtils::isolate_radyls(lipid))
    
    lipid_formula <- addElements(lipidomicsUtils::calc_sphingoid_formula(sphingoid_base),
                                 c(unlist(lapply(fatty_acyls, calc_residue_acyl_formula)),
                                   lipidomicsUtils:::dihexose_formula))
    
    lipid_formula <- subtractElements(lipid_formula,
                                      lipidomicsUtils:::water_formula)
    
  } else if(lipid_mainclass == "Mar") {
    
    fatty_acyls <- unlist(lipidomicsUtils::isolate_radyls(lipid))

    lipid_formula <- addElements(lipidomicsUtils:::dihexose_formula,
                                 unlist(lapply(fatty_acyls, calc_residue_acyl_formula)))
    
  }
  
  return(lipid_formula)
  
}