## atom masses -----------------------------------------------------------------
# get atom masses
c_mass <- rcdk::get.formula("C")@mass
h_mass <- rcdk::get.formula("H")@mass
o_mass <- rcdk::get.formula("O")@mass
n_mass <- rcdk::get.formula("N")@mass
p_mass <- rcdk::get.formula("P")@mass
s_mass <- rcdk::get.formula("S")@mass

## general masses --------------------------------------------------------------
# general base masses
coa_mass <- rcdk::get.formula("C21H36N7O16P3S")@mass
water_mass <- rcdk::get.formula("H2O")@mass
h3po4_mass <- rcdk::get.formula("H3PO4")@mass
h2so4_mass <- rcdk::get.formula("H2SO4")@mass
hexose_mass <- rcdk::get.formula("C6H12O6")@mass
dihexose_mass <- rcdk::get.formula("C12H22O11")@mass

## general formulae ------------------------------------------------------------
# general base formula
coa_formula <- "C21H36N7O16P3S"
water_formula <- "H2O"
h3po4_formula <- "H3PO4"
h2so4_formula <- "H2SO4"
hexose_formula <- "C6H12O6"
dihexose_formula <- "C12H22O11"

## ion masses ------------------------------------------------------------------
proton_mass <- rcdk::get.formula("H", charge = 1)@mass
sodium_ion_mass <- rcdk::get.formula("Na", charge = 1)@mass

## Gylcero- and Glycerophospholipid base masses --------------------------------
# get base masses (PCs)
gpc_mass <- rcdk::get.formula("C8H20NO6P")@mass
pc_mass <- rcdk::get.formula("C5H14NO4P")@mass
choline_mass <- rcdk::get.formula("C5H13NO")@mass

# get base masses (PEs)
gpe_mass <- rcdk::get.formula("C5H14NO6P")@mass
pe_mass <- rcdk::get.formula("C2H8NO4P")@mass
ethanolamine_mass <- rcdk::get.formula("C2H7NO")@mass

# get base masses (PSs)
gps_mass <- rcdk::get.formula("C6H14NO8P")@mass
ps_mass <- rcdk::get.formula("C3H8NO6P")@mass
serine_mass <- rcdk::get.formula("C3H7NO3")@mass

# get base masses (PGs)
gpg_mass <- rcdk::get.formula("C6H15O8P")@mass
pg_mass <- rcdk::get.formula("C3H9O6P")@mass
glycerol_mass <- rcdk::get.formula("C3H8O3")@mass

# get base masses (PIs)
gpi_mass <- rcdk::get.formula("C9H19O11P")@mass
pi_mass <- rcdk::get.formula("C6H13O9P")@mass
inositol_mass <- rcdk::get.formula("C6H12O6")@mass

# get base masses (PIPs)
gpip_mass <- rcdk::get.formula("C9H20O14P2")@mass
pip_mass <- rcdk::get.formula("C6H14O12P2")@mass

# get base masses (PIP2s)
gpipp_mass <- rcdk::get.formula("C9H21O17P3")@mass
pipp_mass <- rcdk::get.formula("C6H15O15P3")@mass

# get base masses (CLs)
cl_mass <- rcdk::get.formula("C9H22O13P2")@mass