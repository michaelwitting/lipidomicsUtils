% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Parser_lipid_category.R
\name{get_lipid_mainclass}
\alias{get_lipid_mainclass}
\title{Determine the lipid main class

This function determines the lipid main class to which a lipid given as shorthand notation belongs}
\usage{
get_lipid_mainclass(lipid)
}
\arguments{
\item{lipid}{Lipid for which the main class shall be determined.}
}
\description{
Determine the lipid main class

This function determines the lipid main class to which a lipid given as shorthand notation belongs
}
\examples{
library(lipidomicsUtils)
get_lipid_category("PC(16:0/18:1(9Z)")

}
\seealso{
\code{\link{get_lipid_category}}

\code{\link{get_lipid_subclass}}
}
\author{
Michael Witting, \email{michael.witting@helmholtz-muenchen.de}
}
