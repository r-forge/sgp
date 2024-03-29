`growthAchievementPlot` <- 
   function(
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
	assessment.name) { 

	CUTLEVEL <- GRADE <- YEAR <- ID <- SCALE_SCORE <- level_1_curve <- NULL ## To prevent R CMD check warnings
	content_area <- toupper(content_area)
        number.achievement.level.regions <- length(stateData[[state]][["Student_Report_Information"]][["Achievement_Level_Labels"]])

        if (state %in% state.abb)  state.name.label <- state.name[state.abb==state]
        if (state=="DEMO") state.name.label <- "Demonstration"

	## Create folder for plots

	dir.create(pdf.folder, recursive=TRUE, showWarnings=FALSE)

	## Create default values

	if (missing(gaPlot.grade_range)) {
		gaPlot.grade_range <- range(stateData[[state]][["Student_Report_Information"]][["Grades_Reported"]])
	}

	if (!missing(state) & missing(gaPlot.percentile_trajectories)) {
		gaPlot.percentile_trajectories <- round(sort(c(10, 50, 90, stateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]]))/5) * 5
	} 
	if (missing(state) & missing(gaPlot.percentile_trajectories)) {
		gaPlot.percentile_trajectories <- c(10, 35, 50, 65, 90)
	}

	tmp.smooth.grades <- seq(gaPlot.grade_range[1], gaPlot.grade_range[2], by=0.01)
	tmp.unique.grades <- gaPlot.grade_range[1]:gaPlot.grade_range[2]
	growthAchievementPlot.data <- gaPlot.sgp_object@Data[J("VALID_CASE", content_area), list(ID, YEAR, GRADE, SCALE_SCORE)][
		GRADE %in% tmp.unique.grades & !is.na(SCALE_SCORE)]

	if (missing(assessment.name) & missing(state)) {
		assessment.name <- NULL
	} else {
		assessment.name <- stateData[[state]][["Assessment_Program_Information"]][["Assessment_Abbreviation"]]
	}


	## Utility functions

	# Functions to create good endpoints for scale score axis

	pretty_year <- function(x) sub("_", "-", x)

	capwords <- function(x) {
		special.words <- c("ELA", "EMH", "II", "III", "IV")
		if (x %in% special.words) return(x)
		s <- sub("_", " ", x)
		s <- strsplit(s, split=" ")[[1]]
		s <- paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)), sep="", collapse=" ")
		s <- strsplit(s, split="-")[[1]]
		paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse="-")
	}

	myround_up <- function(x) {
		temp <- x/10^floor(log(x, 10))
		roundup <- function(y) trunc(y+0.5)
		if (roundup(temp) <= temp) z <- roundup(temp)+0.5
		else z <- roundup(temp)
		return(z*10^floor(log(x,10)))
	}

	myround_down <- function(x) {
		temp <- x/10^floor(log(x, 10))
		roundup <- function(y) trunc(y+0.5)
		if (roundup(temp) >= temp) z <- roundup(temp)-0.5
		else z <- roundup(temp)
		return(z*10^floor(log(x,10)))
	}

	get.my.cutscore.year <- function(state, content_area, year) {
		tmp.cutscore.years <- sapply(strsplit(names(stateData[[state]][["Achievement"]][["Cutscores"]])[grep(content_area, names(stateData[[state]][["Achievement"]][["Cutscores"]]))], "[.]"),
			function(x) x[2])
		if (any(!is.na(tmp.cutscore.years))) {
		if (year %in% tmp.cutscore.years) {
                  return(paste(content_area, year, sep="."))
               } else {
                  if (year==sort(c(year, tmp.cutscore.years))[1]) {
                     return(content_area)
                  } else {
                     return(paste(content_area, rev(sort(tmp.cutscore.years))[1], sep="."))
                  }
               }
             } else {
                  return(content_area)
             }
	}

      create.long.cutscores <- function(state, content_area, year) {
        number.achievement.level.regions <- length(stateData[[state]][["Student_Report_Information"]][["Achievement_Level_Labels"]])
        my.content_area <- get.my.cutscore.year(state, content_area, year)
        if (!content_area %in% names(stateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]])) {
           tmp.grades <- as.numeric(matrix(unlist(strsplit(names(stateData[[state]][["Achievement"]][["Cutscores"]][[my.content_area]]), "_")),
                                          ncol=2, byrow=TRUE)[,2])
           tmp.cutscores <- matrix(unlist(stateData[[state]][["Achievement"]][["Cutscores"]][[my.content_area]]),
                                  ncol=number.achievement.level.regions-1, byrow=TRUE)
           tmp.list <- list()
           for (i in seq(number.achievement.level.regions-1)) {
             tmp.list[[i]] <- data.frame(GRADE=c(min(tmp.grades,na.rm=TRUE)-1, tmp.grades, max(tmp.grades,na.rm=TRUE)+1),
                                        CUTLEVEL=rep(i, length(tmp.grades)+2),
                                        CUTSCORES=c(extendrange(tmp.cutscores[,i], f=0.15)[1], tmp.cutscores[,i], extendrange(tmp.cutscores[,i], f=0.15)[2]))
          }
          subset(do.call(rbind, tmp.list), CUTLEVEL %in% 1:(number.achievement.level.regions-1))
        } else {
          tmp.grades <- as.numeric(matrix(unlist(strsplit(names(stateData[[state]][["Achievement"]][["Cutscores"]][[my.content_area]]), "_")),
                                          ncol=2, byrow=TRUE)[,2])
          tmp.list <- list()
          for (i in seq(number.achievement.level.regions-1)) {
            tmp.list[[i]] <- data.frame(GRADE=c(min(tmp.grades, na.rm=TRUE)-1, tmp.grades, max(tmp.grades, na.rm=TRUE)+1),
                                        CUTLEVEL=rep(i, length(tmp.grades)+2),
                                        CUTSCORES=rep(stateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]][[content_area]][i+1],
                                          length(tmp.grades)+2))
          }
          do.call(rbind, tmp.list)
        }
      } ## END create.long.cutscores

      piecewise.transform <- function(scale_score, state, content_area, year, grade, output.digits=1) {
        if (content_area %in% names(stateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]]) &
            grade %in% as.numeric(matrix(unlist(strsplit(names(stateData[[state]][["Achievement"]][["Knots_Boundaries"]][[content_area]]), "_")), ncol=2, byrow=TRUE)[,2])) {
             tmp.loss.hoss <- stateData[[state]][["Achievement"]][["Knots_Boundaries"]][[content_area]][[paste("loss.hoss_", grade, sep="")]]
             my.content_area <- get.my.cutscore.year(state, content_area, year)
             tmp.old.cuts <- c(tmp.loss.hoss[1], stateData[[state]][["Achievement"]][["Cutscores"]][[my.content_area]][[paste("GRADE_", grade, sep="")]], tmp.loss.hoss[2])
           tmp.new.cuts <- stateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]][[content_area]]
           tmp.index <- findInterval(scale_score, tmp.old.cuts, rightmost.closed=TRUE)
           tmp.diff <- diff(tmp.new.cuts)/diff(tmp.old.cuts)
           round(tmp.new.cuts[tmp.index] + (scale_score - tmp.old.cuts[tmp.index]) * (diff(tmp.new.cuts)/diff(tmp.old.cuts))[tmp.index], digits=output.digits)
        } else {
           as.numeric(scale_score)
        }
      } ## END piecewise.transform

	## Function that produces a smoothed Percentile Trajectory

	gaPlot.percentile_trajectories_Internal <- function(tmp.df, percentile, content_area, year, state) {

		gaPlot.sgp_object@SGP$Panel_Data <- tmp.df
		gaPlot.sgp_object@SGP$SGProjections <- NULL
		tmp.grades <- as.numeric(tmp.df[1,2:((dim(tmp.df)[2]+1)/2)])

		studentGrowthProjections(
			panel.data=gaPlot.sgp_object@SGP,
			sgp.labels=list(my.year=year, my.subject=content_area),
			projcuts.digits=2,
			projection.unit="GRADE",
			percentile.trajectory.values=percentile,
			grade.progression=tmp.grades,
			max.forward.progression.grade=gaPlot.grade_range[2],
			print.time.taken=FALSE)[["SGProjections"]][[paste(content_area, year, sep=".")]][,-1]
	}

	smoothPercentileTrajectory <- function(tmp.df, percentile, content_area, year, state) {
                tmp.trajectories <- gaPlot.percentile_trajectories_Internal(tmp.df, percentile, content_area, year, state)
		trajectories <- c(tail(as.numeric(tmp.df), (dim(tmp.df)[2]-1)/2), as.numeric(tmp.trajectories))
		grade.sequence <- c(as.numeric(tmp.df[1,2:((dim(tmp.df)[2]+1)/2)]), sapply(strsplit(names(tmp.trajectories), "_"), function(x) tail(x, 1)))


		if (content_area %in% names(stateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]])) {
			tmp.spline.fun <- splinefun(grade.sequence, trajectories)
			tmp.function <- function(grades) {
				sapply(grades, function(x) piecewise.transform(tmp.spline.fun(x), state, as.character(content_area), as.character(year), as.character(x)))
			}
			return(splinefun(grade.sequence, tmp.function(grade.sequence)))
		} else {
			return(splinefun(grade.sequence, trajectories))
		}
	}


	## Calculate Scale Transformations (if required) 

	key(growthAchievementPlot.data) <- "GRADE"
	growthAchievementPlot.data$TRANSFORMED_SCALE_SCORE <- 
		growthAchievementPlot.data[, piecewise.transform(SCALE_SCORE, state, as.character(content_area), as.character(year), as.character(GRADE[1])), by=list(GRADE)]$V1
	if (content_area %in% names(stateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]])) {
		gaPlot.show.scale.transformations <- FALSE
	}


	### Calculate ACHIEVEMENT percentiles


	if (!is.null(gaPlot.achievement_percentiles)) {

		## Creating the points used by lines to construct unconditional percentile curves

		temp_uncond_frame <- matrix(nrow=length(gaPlot.achievement_percentiles), ncol=length(tmp.smooth.grades))
		rownames(temp_uncond_frame) <- gaPlot.achievement_percentiles
		colnames(temp_uncond_frame) <- tmp.smooth.grades
		temp_uncond_frame <- as.data.frame(temp_uncond_frame)

		for (i in gaPlot.achievement_percentiles) {
			temp_achievement_curve <- splinefun(tmp.unique.grades, as.vector(by(growthAchievementPlot.data[YEAR==year]$TRANSFORMED_SCALE_SCORE, 
				growthAchievementPlot.data[YEAR==year]$GRADE, quantile, probs=i, na.rm=TRUE)), method="monoH.FC")
			temp_uncond_frame[as.character(i),] <- temp_achievement_curve(tmp.smooth.grades)
		}
	}


	################################################
	###
	### Code for producing the chart
	###
	################################################

	if (format=="print") {
		format.colors.background <- rgb(0.985, 0.985, 1.0)
		format.colors.region <- paste("grey", round(seq(62, 91, length=number.achievement.level.regions)), sep="")
                format.colors.font <- "grey20"
	} else {
		format.colors.background <- rgb(0.48, 0.48, 0.52)
		format.colors.region <- c("#4D98C1", "#6BAED6", "#9ECAE1", "#C6DBEF", "#DEEBF7")[seq(number.achievement.level.regions)]
                format.colors.font <- rgb(0.985, 0.985, 1.0)
	}

	xscale.range <- c(gaPlot.grade_range[1]-0.5, gaPlot.grade_range[2]+0.5)

	temp_cutscores <- subset(create.long.cutscores(state, as.character(content_area), as.character(year)), GRADE %in% tmp.unique.grades) 

	## Create data sets to be used for plot production

	if (is.null(gaPlot.students)) {
		my.content_area <- get.my.cutscore.year(state, content_area, year)
		start.cuts <- stateData[[state]][["Achievement"]][["Cutscores"]][[my.content_area]]
	 	tmp1.df <- data.frame(ID=seq_along(start.cuts[[1]]),
			GRADE=as.numeric(as.character(tail(unlist(strsplit(names(start.cuts)[1], "_")), 1))),
			SCALE_SCORE=start.cuts[[1]])
	} else {
		tmp1.df <- growthAchievementPlot.data[ID %in% gaPlot.students]
	}

	## Start loop over students or starting scores

	for (j in unique(tmp1.df$ID)) {
		tmp2.df <- subset(tmp1.df, ID==j)
		tmp.df <- data.frame(matrix(c(as.numeric(as.character(tmp2.df$ID[1])), tmp2.df$GRADE, tmp2.df$SCALE_SCORE), nrow=1))
		pdf(file=paste(pdf.folder, "/", state.name.label, "_State_Growth_and_Achievement_Plot_", capwords(content_area), "_", year, "_Level_", j, ".pdf", sep=""), 
			width=8.5, height=11, bg=format.colors.background)


	## Define axis ranges based (ranges contingent upon starting score)

	gp.axis.range <- c(smoothPercentileTrajectory(tmp.df, min(gaPlot.percentile_trajectories), content_area, year, state)(gaPlot.grade_range[2]),
		smoothPercentileTrajectory(tmp.df, max(gaPlot.percentile_trajectories), content_area, year, state)(gaPlot.grade_range[2]))
	yscale.range <- c(min(gp.axis.range[1], quantile(growthAchievementPlot.data[YEAR==year]$SCALE_SCORE, prob=.005, na.rm=TRUE)), 
		max(gp.axis.range[2], quantile(growthAchievementPlot.data[YEAR==year]$SCALE_SCORE, prob=.995, na.rm=TRUE)))
	ach.per.axis.range <- (temp_uncond_frame[,1])[temp_uncond_frame[,1] >= yscale.range[1] & temp_uncond_frame[,1] <= yscale.range[2]]
	ach.per.axis.labels <- formatC(100*as.numeric(rownames(temp_uncond_frame)[temp_uncond_frame[,1] >= yscale.range[1] & temp_uncond_frame[,1] <= yscale.range[2]]), 
		digits=0, format="f")
	ss.axis.range <- c(myround_up(yscale.range[1]), myround_down(yscale.range[2]))


##
## Create viewports
##

if (is.null(stateData[[state]][["Achievement"]][["College_Readiness_Cutscores"]])) {
	growth.achievement.vp <- viewport(layout = grid.layout(6, 3, widths = unit(c(1.5, 6.5, 0.5), rep("inches", 3)), 
		heights = unit(c(0.25, 1.15, 0.35, 8.25, 0.5, 0.35), rep("inches", 6))))
} else {
	growth.achievement.vp <- viewport(layout = grid.layout(6, 3, widths = unit(c(1.4, 6.5, 0.6), rep("inches", 3)), 
		heights = unit(c(0.25, 1.15, 0.35, 8.25, 0.5, 0.35), rep("inches", 6))))
}

chart.vp <- viewport(name="chart.vp",
                    layout.pos.row=4, layout.pos.col=2,
                    xscale=xscale.range,
                    yscale=yscale.range,
                    clip="on",
                    gp=gpar(fill="transparent"))

left.axis.vp <- viewport(name="left.axis.vp",
                    layout.pos.row=4, layout.pos.col=1,
                    xscale=c(0,1),
                    yscale=yscale.range,
                    gp=gpar(fill="transparent"))

right.axis.vp <- viewport(name="right.axis.vp",
                    layout.pos.row=4, layout.pos.col=3,
                    xscale=c(0,1),
                    yscale=yscale.range,
                    gp=gpar(fill="transparent"))

bottom.axis.vp <- viewport(name="bottom.axis.vp",
                    layout.pos.row=5, layout.pos.col=2,
                    xscale=xscale.range,
                    yscale=c(0,1),
                    gp=gpar(fill="transparent"))

title.vp <- viewport(name="title.vp",
                    layout.pos.row=2, layout.pos.col=1:3,
                    xscale=c(0,1),
                    yscale=c(0,1),
                    gp=gpar(fill="transparent"))


##
## Push growth.achievement.vp
##

pushViewport(growth.achievement.vp)


##
## Push chart.vp
##

pushViewport(chart.vp)


##
## Code for coloring performance level areas
##

##
## Create spline functions to calculate boundary values for each cutlevel
##

for (i in 1:max(temp_cutscores$CUTLEVEL)){
     assign(paste("level_", i, "_curve", sep=""), splinefun(gaPlot.grade_range[1]:gaPlot.grade_range[2], subset(temp_cutscores, CUTLEVEL==i)$CUTSCORES))
}


##
## Create variables for boundaries and plotting
##

x.boundary.values.1 <- c(gaPlot.grade_range[1], seq(gaPlot.grade_range[1], gaPlot.grade_range[2], length=40), gaPlot.grade_range[2])
for (i in 2:max(temp_cutscores$CUTLEVEL)){
assign(paste("x.boundary.values.", i, sep=""), c(seq(gaPlot.grade_range[1], gaPlot.grade_range[2], length=40), seq(gaPlot.grade_range[2], gaPlot.grade_range[1], length=40)))
}
assign(paste("x.boundary.values.", max(temp_cutscores$CUTLEVEL)+1, sep=""), c(gaPlot.grade_range[1], seq(gaPlot.grade_range[1], gaPlot.grade_range[2], length=40), gaPlot.grade_range[2]))


y.boundary.values.1 <- c(yscale.range[1], level_1_curve(seq(gaPlot.grade_range[1], gaPlot.grade_range[2], length=40)), yscale.range[1])
for (i in 2:max(temp_cutscores$CUTLEVEL)){
assign(paste("y.boundary.values.", i, sep=""), c(eval(parse(text=paste("level_", i-1, "_curve(seq(gaPlot.grade_range[1], gaPlot.grade_range[2], length=40))", sep=""))),
                                                 eval(parse(text=paste("level_", i, "_curve(seq(gaPlot.grade_range[2], gaPlot.grade_range[1], length=40))", sep="")))))
}
assign(paste("y.boundary.values.", max(temp_cutscores$CUTLEVEL)+1, sep=""), c(yscale.range[2],
                                                                           eval(parse(text=paste("level_", max(temp_cutscores$CUTLEVEL) , "_curve(seq(gaPlot.grade_range[1], gaPlot.grade_range[2], length=40))", sep=""))),
                                                                           yscale.range[2]))


##
## Create colored (grey-scale) regions
##

for (i in 1:(1+max(temp_cutscores$CUTLEVEL))){
grid.polygon(x=get(paste("x.boundary.values.", i, sep="")),
             y=get(paste("y.boundary.values.", i, sep="")),
             default.units="native",
             gp=gpar(fill=format.colors.region[i], lwd=0.1, col="grey85"))
}


	## Code for producing the achievement percentile curves

	if (!is.null(gaPlot.achievement_percentiles)){

		for (i in rownames(temp_uncond_frame)) {
			grid.lines(tmp.smooth.grades, temp_uncond_frame[i,], gp=gpar(lwd=0.3, col="white"), default.units="native")
		}
	}


	## Code for producing percentile growth trajectories

	if (!is.null(gaPlot.percentile_trajectories)){

		for (i in gaPlot.percentile_trajectories) {
			grid.lines(tmp.smooth.grades, (smoothPercentileTrajectory(tmp.df, i, content_area, year, state))(tmp.smooth.grades), 
				gp=gpar(lwd=1.2, col="black"), default.units="native")
		}
	}

	popViewport() ## pop chart.vp


##
## Left Axis Viewport
##

pushViewport(left.axis.vp)

if (gaPlot.show.scale.transformations) {
         grid.lines(0.6, ss.axis.range, gp=gpar(lwd=1.5, col=format.colors.font), default.units="native")
         tmp.diff <- ss.axis.range[2]-ss.axis.range[1]
         if (tmp.diff < 2.5) my.by <- 0.25
         if (tmp.diff >= 2.5 & tmp.diff < 5) my.by <- 0.5
         if (tmp.diff >= 5 & tmp.diff < 25) my.by <- 2.5
         if (tmp.diff >= 25 & tmp.diff < 50) my.by <- 5
         if (tmp.diff >= 50 & tmp.diff < 250) my.by <- 25
         if (tmp.diff >= 250 & tmp.diff < 1000) my.by <- 50
         if (tmp.diff >= 1000 & tmp.diff < 5000) my.by <- 250
         for (i in seq(ss.axis.range[1], ss.axis.range[2], by=my.by)) {
         grid.lines(c(0.5, 0.6), i, gp=gpar(lwd=1.5, col=format.colors.font), default.units="native")
         grid.text(x=0.45, y=i, formatC(i, digits=0, format="f"), gp=gpar(col=format.colors.font, cex=0.8), just="right", default.units="native")
         }
      grid.text(x=unit(0.15, "native"), y=0.5, "Scale Score", gp=gpar(col=format.colors.font, cex=1.0), rot=90)
}

grid.lines(0.95, ach.per.axis.range, gp=gpar(lwd=1.5, col=format.colors.font), default.units="native")

for (i in 1:length(ach.per.axis.range)) {
grid.lines(c(0.95, 1.05), ach.per.axis.range[i], gp=gpar(lwd=1.5, col=format.colors.font), default.units="native")
grid.text(x=1.2, y=ach.per.axis.range[i], ach.per.axis.labels[i], gp=gpar(col=format.colors.font, cex=0.65), just="right", default.units="native")
}

grid.text(x=unit(0.8, "native"), y=unit(median(growthAchievementPlot.data[GRADE==tmp.unique.grades[1]]$SCALE_SCORE), "native"), 
	paste(pretty_year(year), "Achievement Percentile"), gp=gpar(col=format.colors.font, cex=0.9), rot=90)

popViewport() ## pop left.axis.vp


##
## Right Axis Viewport
##

pushViewport(right.axis.vp)

if (is.null(stateData[[state]][["Achievement"]][["College_Readiness_Cutscores"]])) {
	grid.lines(0.1, gp.axis.range, gp=gpar(lwd=1.5, col=format.colors.font), default.units="native")

	for (i in gaPlot.percentile_trajectories){
		grid.lines(c(-0.1, 0.1), smoothPercentileTrajectory(tmp.df, i, content_area, year, state)(gaPlot.grade_range[2]), 
			gp=gpar(lwd=1.5, col=format.colors.font), default.units="native")
		grid.text(x=unit(-0.55, "native"), y=smoothPercentileTrajectory(tmp.df, i, content_area, year, state)(gaPlot.grade_range[2]), i, 
			gp=gpar(col=format.colors.font, cex=0.8), just="left", default.units="native")
	}

#	grid.text(x=0.5, y=smoothPercentileTrajectory(tmp.df, 50, content_area, year, state)(gaPlot.grade_range[2]), "Percentile Growth Trajectory", 
#		gp=gpar(col=format.colors.font, cex=1.0), rot=90, default.units="native")
	grid.text(x=0.5, y=(gp.axis.range[1]+gp.axis.range[2])/2, "Percentile Growth Trajectory", 
		gp=gpar(col=format.colors.font, cex=1.0), rot=90, default.units="native")
} else {
	tmp.cut <- as.numeric(stateData[[state]][["Achievement"]][["College_Readiness_Cutscores"]][[content_area]])
	grid.polygon(x=c(0.05, 0.05, 0.35, 0.35), y=c(gp.axis.range[1], tmp.cut, tmp.cut, gp.axis.range[1]), default.units="native", 
		gp=gpar(col=format.colors.font, fill="red", lwd=1.5))
	grid.text(x=0.2, y=(gp.axis.range[1]+tmp.cut)/2, "Not College Ready", gp=gpar(col=format.colors.font, cex=0.5), rot=90, default.units="native")
	grid.polygon(x=c(0.05, 0.05, 0.35, 0.35), y=c(gp.axis.range[2], tmp.cut, tmp.cut, gp.axis.range[2]), default.units="native", 
		gp=gpar(col=format.colors.font, fill="green3", lwd=1.5))
	grid.text(x=0.2, y=(gp.axis.range[2]+tmp.cut)/2, "College Ready", gp=gpar(col=format.colors.font, cex=0.5), rot=90, default.units="native")

	for (i in gaPlot.percentile_trajectories){
		grid.lines(c(-0.15, 0.05), smoothPercentileTrajectory(tmp.df, i, content_area, year, state)(gaPlot.grade_range[2]), 
			gp=gpar(lwd=1.5, col=format.colors.font), default.units="native")
		grid.text(x=unit(-0.5, "native"), y=smoothPercentileTrajectory(tmp.df, i, content_area, year, state)(gaPlot.grade_range[2]), i, 
			gp=gpar(col=format.colors.font, cex=0.8), just="left", default.units="native")
	}

#	grid.text(x=0.65, y=smoothPercentileTrajectory(tmp.df, 50, content_area, year, state)(gaPlot.grade_range[2]), "Percentile Growth Trajectory to College Readiness", 
#		gp=gpar(col=format.colors.font, cex=1.0), rot=90, default.units="native")
	grid.text(x=0.65, y=(gp.axis.range[1]+gp.axis.range[2])/2, "Percentile Growth Trajectory to College Readiness", 
		gp=gpar(col=format.colors.font, cex=1.0), rot=90, default.units="native")
}
popViewport() ## pop right.axis.vp

##
## Bottom Axis Viewport
##

pushViewport(bottom.axis.vp)

grid.lines(gaPlot.grade_range, 0.8, gp=gpar(lwd=1.5, col=format.colors.font), default.units="native")
for (i in gaPlot.grade_range[1]:gaPlot.grade_range[2]){
grid.lines(i, c(0.5, 0.8), gp=gpar(lwd=1.5, col=format.colors.font), default.units="native")
grid.text(x=i, y=0.25, paste("Grade", i), gp=gpar(col=format.colors.font, cex=1.0), default.units="native")
}

popViewport() ## pop bottom.axis.vp


##
## Top Viewport
##

pushViewport(title.vp)

grid.roundrect(width=unit(0.95, "npc"), r=unit(0.025, "snpc"), gp=gpar(col=format.colors.font, lwd=1.6))
grid.text(x=0.5, y=0.675, paste(state.name.label, ": ", pretty_year(year), " ", capwords(content_area), sep=""), 
	gp=gpar(col=format.colors.font, fontface=2, fontfamily="Helvetica-Narrow", cex=3.0), default.units="native")
if (is.null(stateData[[state]][["Achievement"]][["College_Readiness_Cutscores"]])) {
	grid.text(x=0.5, y=0.275, "Norm & Criterion Referenced Growth & Achievement", 
		gp=gpar(col=format.colors.font, fontface=2, fontfamily="Helvetica-Narrow", cex=2.25), default.units="native")
} else {
	grid.text(x=0.5, y=0.275, "Norm & Criterion Referenced Growth to College Readiness", 
		gp=gpar(col=format.colors.font, fontface=2, fontfamily="Helvetica-Narrow", cex=2.0), default.units="native")
}

popViewport() ## pop title.vp


##
## End Viewport Creation
##

popViewport()


dev.off() 

} ## End loop over starting scores or students
} ## End growthAchievementPlot function
