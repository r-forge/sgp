\name{growthAchievementPlot}
\alias{growthAchievementPlot}
\title{growthAchievementPlot for SGP}
\description{
Function to create growth and achievement plots depicting system level results associated with student growth percentile results. The charts show, simultaneously, norm- and criterion-referneced
student achievement (i.e., status) as well as norm- and criterion referenced student growth. These charts are those shown on the cover of \emph{Educational Measurement: Issues and Practice} 
as part of Betebenner (2009).
}
\usage{
growthAchievementPlot(
        gaPlot.sgp_object,
        gaPlot.students,
        gaPlot.percentile_trajectories,
        gaPlot.achievement_percentiles=c(.01, seq(.05, .95, by=.05), .99),
        gaPlot.show.scale.transformations=TRUE,
        gaPlot.grade_range,
        state,
        content_area,
        year,
        format="print",
        pdf.folder,
        assessment.name)
}

\arguments{
	\item{gaPlot.sgp_object}{
        The sgp_object containing system information for constructing the growth and achievement plot. Object is calculated using \code{\link{abcSGP}} or (at least) \code{\link{prepareSGP}}
        followed by \code{\link{analyzeSGP}}.
	}
	\item{gaPlot.students}{
        Either NULL (the default) or a list of student IDs for whom one wishes to generate growth and achievement plots. 
	}
	\item{gaPlot.percentile_trajectories}{
	A vector indicating the growth percentile trajectories to be depicted on the plot. If missing, the percentile trajectories will be the trajectories associated with the state supplied. If no
        state is supplied, the percentile trajectories will be 10, 35, 50, 65, 90.
	}
	\item{gaPlot.achievement_percentiles}{
        A vector of percentiles that achievement (i.e., status) percentiles will be depicted across the range of grades.
        }
        \item{gaPlot.show.scale.transformations}{
        A boolean arguments (defaults to TRUE) indicating whether to show the scale as a vertical axis if a scale transformation is applied to the supplied data.
        }
	\item{gaPlot.grade_range}{
	The grade range for which to demonstrate plot. If missing, function uses supplied state to derive grade range.	
	}
        \item{state}{
        A two letter acronym for the state associated with the supplied data.
        }
        \item{content_area}{
        A character vector indicating the content area to produce the growth and achievement chart. Note that the supplied content area must match that used in the internal labelling of content
	area for the sgp_object.
	}
	\item{year}{
	The year for which to produce the growth and achievement plots.
	}
	\item{format}{
	Either "print" (the default) for light background or "presentation" for dark background slides.
	}
	\item{pdf.folder}{
	A character vector indicating where to put the produced growth and achievement plot.
	}
	\item{assessment.name}{
	A character vector indicating the assessment.name. If missing, the supplied state is used to determine the assessment.name.
	} 

}

\details{Typical use of the function is as part of \code{visualizeSGP} function. However, function can be used to produce growth and achievement charts. 
}

\value{Function creates a growth and achievement chart and writes the result as a PDF to \code{pdf.folder}.} 

\references{
Betebenner, D. W. (2009). Norm- and criterion-referenced student growth.
Educational Measurement: Issues and Practice, 28(4):42-51.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org}}

\keyword{misc}
\keyword{models}
