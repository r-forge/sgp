\name{analyzeSGP}
\alias{analyzeSGP}
\title{Analyze student data to produce student growth percentiles and student growth projections}
\description{
  Utility function/exemplar used to produce student growth percentiles and student growth projections using long formatted data like that provided by \code{\link{prepareSGP}}.
}

\usage{
analyzeSGP(sgp_object=sgpData_LONG,
           years,
           content_areas,
           grades,
           state="DEMO",
           sgp.percentiles=TRUE,
           sgp.projections=TRUE,
           sgp.projections.lagged=TRUE,
           sgp.config) 
}

\arguments{
  \item{sgp_object}{A list containing long formatted data in the \code{Student} (from \code{prepareSGP}) slot. 
  }
  \item{years}{A vector indicating years in which to produce student growth percentiles and/or student growth projections/trajectories. If left missing the 
function will use the data to infer the years based upon the assumption of having at least three years of panel data for analyses.
  }
  \item{content_areas}{A vector indicating content areas in which to produce student growth percentiles and/or student growth projections/trajectories. 
If left missing the function will use the data to infer the content areas available for analyses.
  }
  \item{grades}{A vector indicating grades for which to calculate student growth percentiles and/or student growth projections/trajectories. 
If left missing the function will use the data to infer all the grade progressions for student growth percentile and student growth projections/trajectories analyses.
  }
  \item{state}{Acronym indicating state associated with the district for access to embedded knot and boundaries and cutscores.
  }
  \item{sgp.percentiles}{Boolean variable indicating whether to calculate student growth percentiles. Defaults to TRUE.
  }
  \item{sgp.projections}{Boolean variable indicating whether to calcualte student growth projections. Defaults to TRUE.
  }
  \item{sgp.projections.lagged}{Boolean variable indicating whether to calculate lagged student growth projections often used for growth to standard analyses. Defaults to TRUE.
  }
  \item{sgp.config}{If \code{years}, \code{content_areas}, and \code{grades} are missing, user can directly specify a list containing three vectors: \code{sgp.content.areas}, 
\code{sgp.panel.years}, and \code{sgp.grade.sequences}. This advanced option is helpful for analysis of non-traditional grade progressions and other special cases. 
See examples for use cases.
  }
}

\value{Function returns a list containing the long data set in the \code{Student} slot as a data.table keyed using \code{VALID_CASE}, \code{CONTENT_AREA}, 
\code{YEAR}, \code{ID} and the student growth percentile and/or student growth projection/trajectory results in the \code{SGP} slot. 
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} }

\seealso{\code{\link{prepareSGP}}, \code{\link{combineSGP}}}

\examples{
\dontrun{
## analyzeSGP is Step 2 of 5
DEMO_Data <- prepareSGP(sgpData_LONG)
DEMO_Data <- analyzeSGP(DEMO_Data)
DEMO_Data <- combineSGP(DEMO_Data)
}  
}

\keyword{documentation}
