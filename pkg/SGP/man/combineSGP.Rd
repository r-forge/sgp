\name{combineSGP}
\alias{combineSGP}
\title{Combine student data and SGP results}
\description{
  Utility function/exemplar used to merge student long data with results from studentGrowthPercentiles and/or studentGrowthProjections calculations.
}

\usage{
combineSGP(sgp_object) 
}

\arguments{
  \item{sgp_object}{A list containing both \code{Student} (from \code{prepareSGP}) and \code{SGP} (from \code{analyzeSGP}) slots. 
  }
}

\value{Function returns a list containing the input long data set in the \code{Student} slot as a data.table keyed using \code{VALID_CASE}, \code{CONTENT_AREA}, 
\code{YEAR}, \code{ID}. 
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} }

\seealso{\code{\link{prepareSGP}}, \code{\link{analyzeSGP}}}

\examples{
\dontrun{
## combineSGP is Step 3 of 5
DEMO_Data <- prepareSGP(sgpData_LONG)
DEMO_Data <- analyzeSGP(DEMO_Data)
DEMO_Data <- combineSGP(DEMO_Data)
}  
}

\keyword{documentation}
