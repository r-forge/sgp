\name{stateData}
\alias{stateData}
\docType{data}
\title{State assessment program data including performance level cutscores for large scale state assessments}
\description{
A list containing state specific information on testing systems. Currently the list contains achievement level cutscores and labels for the state assessments, 
assessment name and abbreviation, growth cutscores and labels, information on vertical scaling, and knots and boundaries currently being used for SGP analyses
at the state level. The cutscores, in particular, are currently used by the \code{\link{studentGrowthProjections}} function to calculate growth-to-standard/projection 
values by supplying a state's two letter acronym to the argument \code{performance.level.cutscores}. In the future, this list will expand to include other relevant data 
about the state assessment system. States currently included in the data set are Arizona (AZ), Colorado (CO), Connecticut (CT), Idaho (ID), Indiana (IN), 
Maine (ME), Massachusetts (MA), Missouri (MO), Nevada (NV), New Hampshire (NH), New York (NY), Rhode Island (RI), Vermont (VT), Virginia (VA), Wisconsin (WI) and
Demonstration (DEMO).  
}
\usage{stateData}

\seealso{\code{\link{studentGrowthProjections}}, \code{\link{studentGrowthPercentiles}}}

\source{State assessment data and technical assessment documentation}
\keyword{datasets}
