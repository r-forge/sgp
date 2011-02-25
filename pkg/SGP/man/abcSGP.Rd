\name{abcSGP}
\alias{abcSGP}
\title{Perform 5 step sequence: prepareSGP, analyzeSGP, combineSGP, summarizeSGP, and reportSGP}
\description{
  Utility function to perform sequence of steps going from data preparation, \code{\link{prepareSGP}}, SGP data analysis, \code{\link{analyzeSGP}},
  data combining, \code{\link{combineSGP}}, data summarization, \code{\link{summarizeSGP}}, and data reporting, \code{\link{reportSGP}}. Calculating and using
  student growth percentiles is as easy as ABC.
}

\usage{
abcSGP(sgp_object=sgpData_LONG,
       years,
       content_areas,
       grades,
       state="DEMO",
       groups,
       sgp.summaries,
       confidence.interval.groups) 
}

\arguments{
  \item{sgp_object}{A panel data set in long form or an SGP object with embedded long data in the \code{Student} slot. See embedded \code{\link{sgpData_LONG}} 
data set for an exemplar. 
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
  \item{state}{The acronym for the state from which the data is derived. Analyses will access various embedded information about state data to conduct analyses and produce
  reports.
  }
  \item{groups}{A list consisting of 5 types of gruops across which all summaries are taken: \code{institution}, \code{content}, \code{time}, \code{institution_level},
\code{demographic}, and \code{institution_inclusion}. Summaries generated in summarizeSGP are for all posible combinations of the 5 types of group.  
  }
  \item{sgp.summaries}{A list giving the summaries requeted for each group analyzed based upon the \code{group} argument.
  }
  \item{confidence.interval.groups}{A subset of the groups provided in the groups argument indicating which groups to provide confidence intervals for.
  }
}

\value{Function returns a list containing the input long data set in the \code{Student} slot as a data.table keyed using \code{VALID_CASE}, \code{CONTENT_AREA}, 
\code{YEAR}, \code{ID}, SGP results including student growth percentile and student growth projections/trajectories in the SGP slot, summary results in the 
Summary slot, and folders containing multiple pdf reports. 
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org}}

\seealso{\code{\link{sgpData_LONG}}, \code{\link{prepareSGP}}, \code{\link{analyzeSGP}}, \code{\link{combineSGP}}, \code{\link{summarizeSGP}}, 
\code{\link{studentGrowthPercentiles}}, \code{\link{studentGrowthProjections}} and \code{\link{reportSGP}}}

\examples{
\dontrun{
DEMO_Data <- abcSGP(sgpData_LONG)
}  
}

\keyword{documentation}
