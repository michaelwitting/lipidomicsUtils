% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Parser_functional_groups.R
\name{get_hydroxy_groups}
\alias{get_hydroxy_groups}
\title{Get all hydroxy groups

This function returns all hydroxy groups in a acyl, alkyl, alkenyl, sphingoid base etc.}
\usage{
get_hydroxy_groups(x)
}
\arguments{
\item{lipid}{Lipid for which the mass shall be calculated.}
}
\description{
Get all hydroxy groups

This function returns all hydroxy groups in a acyl, alkyl, alkenyl, sphingoid base etc.
}
\examples{
library(lipidomicsUtils)
get_double_bonds("d16:1(1OH,3OH,15Me)")

}
\seealso{
\code{\link{get_methyl_branches}}

\code{\link{get_double_bonds}}

\code{\link{get_keto_groups}}

\code{\link{get_peroxy_groups}}

\code{\link{get_amino_groups}}
}
\author{
Michael Witting, \email{michael.witting@helmholtz-muenchen.de}
}
