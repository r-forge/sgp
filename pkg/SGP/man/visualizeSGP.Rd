\name{visualizeSGP}
\alias{visualizeSGP}
\title{Visualize data from SGP analyses}
\description{
Utility function to produce a variety of graphical displays associated with student growth percentile/percentile growth trajectory results. 
Function currently includes facility to produce individual student growth and achievement plots, interactive bubble plots depicting summary growth 
and achievement data, and growth and achievement charts showing system level growth and achievement data as shown on the cover of \emph{Educational Measurement:
Issues and Practice} as part of Betebenner (2009).
}

\usage{
visualizeSGP(
           sgp_object,
           plot.types=c("bubblePlot", "studentGrowthPlot", "growthAchievementPlot"),
           state,
           bPlot.years=NULL,
           bPlot.content_areas=NULL,
           bPlot.districts=NULL,
           bPlot.schools=NULL,
           bPlot.styles=c(1),
           bPlot.levels=NULL,
           bPlot.full.academic.year=TRUE,
           bPlot.minimum.n=10,
           bPlot.anonymize=FALSE,
           bPlot.prior.achievement=TRUE, 
           bPlot.draft=FALSE,
           bPlot.format="print",
           bPlot.folder="Visualizations/bubblePlots",
           sgPlot.save.sgPlot.data=FALSE,
           sgPlot.years=NULL,
           sgPlot.content_areas=NULL,
           sgPlot.districts=NULL,
           sgPlot.schools=NULL,
           sgPlot.students=NULL,
           sgPlot.reports.by.student=FALSE,
           sgPlot.header.footer.color="#4CB9CC",
           sgPlot.front.page=NULL,
           sgPlot.folder="Visualizations/studentGrowthPlots",
           sgPlot.folder.names="number",
           sgPlot.fan=TRUE,  
           sgPlot.anonymize=FALSE,
           sgPlot.cleanup=TRUE,
           sgPlot.demo.report=FALSE,
           sgPlot.produce.plots=TRUE,
           sgPlot.parallel.config=list(TYPE="FOREACH", OPTIONS=list(preschedule=FALSE)),
           gaPlot.years=NULL,
           gaPlot.content_areas=NULL,
           gaPlot.students=NULL,
           gaPlot.format="print",
           gaPlot.folder="Visualizations/growthAchievementPlots") 
}

\arguments{
  \item{sgp_object}{An object of class SGP containing long formatted data in the \code{Data} slot that will be used for the production of student growth 
and achievement plots and system growth and achievement plots, summary data from \code{\link{summarizeSGP}} in the \code{Summary} slot for bubble plots.  
  }
  \item{plot.types}{A character vector indicating what types of plots to construct. Currently available plots include \code{\link{bubblePlot}}, 
\code{\link{studentGrowthPlot}}, and \code{\link{growthAchievementPlot}}.
  }
  \item{state}{Acronym indicating state associated with the summaries for access to assessment program information embedded in \code{\link{stateData}}.
  }
  \item{bPlot.years}{A vector indicating year(s) in which to produce \code{\link{bubblePlot}}s using data available in \code{sgp_object}. If missing the 
function will use the last year available in the data to produce \code{\link{bubblePlot}}s.
  }
  \item{bPlot.content_areas}{A vector indicating content area(s) to produce \code{\link{bubblePlot}}s using data available in \code{sgp_object}. If missing the 
function will produce plots for all available content areas provided in the data. 
  }
  \item{bPlot.districts}{A vector indicating districts to produce \code{\link{bubblePlot}}s using data available in \code{sgp_object}. Consult \code{\link{bubblePlot}} 
styles to determine which \code{\link{bubblePlot}}s styles accept specification for districts. Default is to produce plots for all available districts in the data. 
  }
  \item{bPlot.schools}{A vector indicating schools to produce \code{\link{bubblePlot}}s using data available in \code{sgp_object}. Consult \code{\link{bubblePlot}} 
styles to determine which \code{\link{bubblePlot}} styles accept specification for schools. Default is to produce plots for all available schools in the data. 
  }
  \item{bPlot.styles}{A vector of positive integers indicating the types of \code{\link{bubblePlot}}s to produce using data available in \code{sgp_object}. See 
associated documentation for example plots.  
  }
  \item{bPlot.levels}{A character vector of levels to be used to color bubbles in the \code{\link{bubblePlot}}. See  associated documentation for example plots.
  }
  \item{bPlot.full.academic.year}{A boolean argument (defaults to TRUE) indicating whether \code{\link{bubblePlot}}s should use full academic year results if available. 
  }
  \item{bPlot.minimum.n}{A positive integer (defaults to 10) indicating the minimum size for summary values to be displayed in the \code{\link{bubblePlot}}s. 
  }
  \item{bPlot.anonymize}{A boolean argument (defaults to FALSE) indicating whether to anonymize \code{\link{bubblePlot}}s school and district names that appear in the
plots and data tips of the plots.  For student level anonymization, the function utilizes the randomNames package to produce gender and ethnic correct names based 
upon gender and ethnicity codes available in \code{sgp_object@Data}.
  }
  \item{bPlot.prior.achievement}{A boolean argument (defaults to TRUE) indicating whether to produce \code{\link{bubblePlot}}s using prior achievement as well as current
achievement as the vertical dimension of the \code{\link{bubblePlot}}. 
  }
  \item{bPlot.draft}{A boolean argument (defaults to FALSE) indicating whether to put an indicator on the chart noting that the results are draft and to not distribute. 
  }
  \item{bPlot.format}{Either "print" or "presentation" indicating whether to optimize the plot for print form (light background) or presentation form (dark background).
  }
  \item{bPlot.folder}{Character vector indicating where \code{\link{bubblePlot}}s should be placed. Default folder is "Visualizations/bubblePlots".
  }
  \item{sgPlot.save.sgPlot.data}{A boolean argument indicating whether wide formatted data should be save as part of indivividual student report production. For operational
student report production construction, using a wide formatted file for a large state can save in having to reshape the same long file multiple times.
  }
  \item{sgPlot.years}{A vector indicating year for which to produce \code{\link{studentGrowthPlot}}s. The supplied year indicates the \emph{final} year associated with each
student's \code{\link{studentGrowthPlot}}. If missing the function will use the last year available in the data to produce \code{\link{studentGrowthPlot}}s.
  }
  \item{sgPlot.content_areas}{A vector indicating the content areas for which to produce \code{\link{studentGrowthPlot}}s. If missing, the function will utilize all available
content areas.
  }
  \item{sgPlot.districts}{A vector indicating which districts to produce \code{\link{studentGrowthPlot}}s for. If missing the function will use \emph{all} available 
districts in the data to produce \code{\link{studentGrowthPlot}}s.
  }
  \item{sgPlot.schools}{A vector indicating which schools to produce \code{\link{studentGrowthPlot}}s for. If missing the function will use \emph{all} available 
schools in the data to produce \code{\link{studentGrowthPlot}}s. If both \code{sgPlot.districts} and \code{sgPlot.schools} are provided the function produces 
\code{\link{studentGrowthPlot}}s for ALL students in the districts and schools provided. 
  }
  \item{sgPlot.students}{A vector of student \code{ID}s indicating which students to produce \code{\link{studentGrowthPlot}}s for. If missing the function will 
use \emph{all} available students in the data to produce \code{\link{studentGrowthPlot}}s. 
  }
  \item{sgPlot.reports.by.student}{A boolean variable indicating whether the reports should be collated as single reports in a single folder or deposited into a district/school/grade folder 
hierarchy. The default is FALSE which puts the reports into their appropriate district/school/grade slot. 
  }
  \item{sgPlot.header.footer.color}{Character vector (default is blue) indicating the color of the header/footer associated with the 
\code{\link{studentGrowthPlot}}. Another good color is \code{goldenrod2}.
  }
  \item{sgPlot.front.page}{A path to a PDF to be used as the front page to the \code{\link{studentGrowthPlot}}. The default is missing so that no front page is
attached to the \code{\link{studentGrowthPlot}}. 
  }
  \item{sgPlot.folder}{Character vector indicating where \code{\link{studentGrowthPlot}}s should be placed. Note that \code{\link{studentGrowthPlot}}s are placed 
within nested folders within this folder. Default folder is "Visualizations/studentGrowthPlots". 
  }
  \item{sgPlot.folder.names}{Either "name" or "number" (the default) indicating how the nested folder structure will be labeled that holds the 
\code{\link{studentGrowthPlot}}s.   
  }
  \item{sgPlot.fan}{A boolean argument (defaults to TRUE) indicating whether to produce projection fan on \code{\link{studentGrowthPlot}}s.  
  }
  \item{sgPlot.anonymize}{A boolean argument (defaults to FALSE) indicating whether to anonymize \code{\link{studentGrowthPlot}}s student, school and district names. 
For student level anonymization, the function utilizes the \code{randomNames} package to produce gender and ethnicity based names based upon gender and ethnicity 
codes available in \code{sgp_object@Data}.
  }
  \item{sgPlot.cleanup}{A boolean argument (defaults to TRUE) indicating whether to remove files produced by pdfLaTeX to produce \code{\link{studentGrowthPlot}} catalogs.  
  }
  \item{sgPlot.demo.report}{A boolean argument (defaults to TRUE) indicating whether to just produce a sample \code{\link{studentGrowthPlot}} catalogs. Note: When producing
\code{\link{studentGrowthPlot}}s for an entire state, considerable resources are required to produce this many reports. We are actively working on parallelizing this functionality
to reduce report production time by two orders of magnitude.  
  }
  \item{sgPlot.produce.plots}{A boolean argument (defaults to TRUE) indicating whether to produce \code{\link{studentGrowthPlot}}s. Useful when one just wants to produce
wide formatted data without the actual student growth plots. 
  }
  \item{sgPlot.parallel.config}{A list indicating the \code{TYPE} of back end used for the parallel computation and \code{OPTIONS} that apply to that parallel back end. Default is to use 
\code{FOREACH} for the back end with \code{OPTIONS=list(preschedule=FALSE)}
  }
  \item{gaPlot.years}{A vector indicating year for which to produce \code{\link{growthAchievementPlot}}s. If missing the function will use the last year available 
in the data to produce \code{\link{growthAchievementPlot}}s.
  }
  \item{gaPlot.content_areas}{A vector indicating content area(s) to produce \code{\link{growthAchievementPlot}}s using data available in \code{sgp_object}. If missing the 
function will produce plots for all available content areas provided in the data. 
  }
  \item{gaPlot.students}{A vector of student \code{ID}s indicating which students to produce \code{\link{growthAchievementPlot}}s for. If missing the function will 
use \emph{all} available students in the data to produce \code{\link{growthAchievementPlot}}s. 
  }
  \item{gaPlot.format}{Either "print" or "presentation" indicating whether to optimize the plot for print form (light background) or presentation form (dark background). 
  }
  \item{gaPlot.folder}{Character vector indicating where \code{\link{growthAchievementPlot}}s should be placed. Default folder is "Visualizations/growthAchievementPlots".  
  }
}

\value{Function produces \emph{numerous} (potentially hundreds of thousands) of pdf files in a folder structure specified by the user and supplied through arguments 
to the function.
}


\references{
Betebenner, D. W. (2009). Norm- and criterion-referenced student growth. 
Educational Measurement: Issues and Practice, 28(4):42-51.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} and Adam Van Iwaarden \email{Vaniwaarden@colorado.edu}}

\seealso{\code{\link{bubblePlot}}, \code{\link{bubblePlot_Styles}}, \code{\link{studentGrowthPlot}}, \code{\link{growthAchievementPlot}}}

\examples{
\dontrun{
## visualizeSGP is Step 5 of 5 of abcSGP
Demonstration_Data <- sgpData_LONG
Demonstration_Data <- prepareSGP(Demonstration_Data)
Demonstration_Data <- analyzeSGP(Demonstration_Data)
Demonstration_Data <- combineSGP(Demonstration_Data)
Demonstration_Data <- summarizeSGP(Demonstration_Data)
visualizeSGP(Demonstration_Data)

## Production of sample student growth and achievement plots

visualizeSGP(
	sgp_object=Demonstration_Data, 
	plot.types="studentGrowthPlot",
	state="DEMO",
	sgPlot.districts=470,
	sgPlot.schools=c(6418, 8008),
	sgPlot.header.footer.color="#4CB9CC")
}  
}

\keyword{documentation}
