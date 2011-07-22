`bubblePlot_Styles` <- 
	function(sgp_object,
		state=state,
		bPlot.years,
		bPlot.content_areas,
		bPlot.districts,
		bPlot.schools,
		bPlot.styles=c(1),
		bPlot.full.academic.year=TRUE,
		bPlot.minimum.n=10,
		bPlot.anonymize=FALSE,
		bPlot.prior.achievement=TRUE, 
		bPlot.draft=FALSE,
		bPlot.format="print",
		bPlot.folder="Visualizations/bubblePlots") {


	DISTRICT_NUMBER <- DISTRICT_NAME <- SCHOOL_NUMBER <- SCHOOL_NAME <- SCHOOL_ENROLLMENT_STATUS <- YEAR <- CONTENT_AREA <- MEDIAN_SGP_COUNT <- NULL ## To prevent R CMD check warnings

        ### Define relevant quantities

	if (missing(bPlot.years)) bPlot.years <- NULL
	if (missing(bPlot.content_areas)) bPlot.content_areas <- NULL
	if (missing(bPlot.districts)) bPlot.districts <- NULL
	if (missing(bPlot.schools)) bPlot.schools <- NULL

        # State stuff

        if (state %in% state.abb)  state.name.label <- state.name[state.abb==state]
        if (state=="DEMO") state.name.label <- "Demonstration"

        # draft message

        if (bPlot.draft) {
                bPlot.message <- c("grid.text(x=unit(50, 'native'), y=unit(50, 'native'), 'DRAFT - DO NOT DISTRIBUTE', rot=-30, gp=gpar(col='grey80', cex=2.9, alpha=0.8, fontface=2))",
          "grid.lines(x=unit(50, 'native'), y=c(0,1), gp=gpar(col='grey40', lwd=1.5, lty=2, alpha=0.5))")
        } else {
                bPlot.message <- NULL
        }


	### Utility functions	

	"%w/o%" <- function(x,y) x[!x %in% y]

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

	create.bPlot.labels <- function(year.iter, y.variable.iter, bubblePlot_LEVEL) {
		pretty_year <- function(x) sub("_", "-", x)
		my.labels <- list()
		my.labels$x.year.label <- pretty_year(year.iter)
		if (length(grep("PRIOR", y.variable.iter)) > 0) {
			y.year <- paste(as.numeric(unlist(strsplit(year.iter, "_")))-1, collapse="-")
			if (bubblePlot_LEVEL=="Summary") my.labels$y.year.label <- paste(y.year, "Percent at/above Proficient")
			if (bubblePlot_LEVEL=="Individual") my.labels$y.year.label <- paste(y.year, "Achievement Level")
			if (bubblePlot_LEVEL=="Summary") my.labels$main.title <- "Growth and Prior Achievement"
			if (bubblePlot_LEVEL=="Individual") my.labels$main.title <- "Student Growth and Prior Achievement"
			if (bubblePlot_LEVEL=="Summary") my.labels$pdf.title <- "Bubble_Plot_(Prior_Achievement)"
			if (bubblePlot_LEVEL=="Individual") my.labels$pdf.title <- "Student_Bubble_Plot_(Prior_Achievement)"
		} else {
			y.year <- pretty_year(year.iter)
			if (bubblePlot_LEVEL=="Summary") my.labels$y.year.label <- paste(y.year, "Percent at/above Proficient")
			if (bubblePlot_LEVEL=="Individual") my.labels$y.year.label <- paste(y.year, "Achievement Level")
			if (bubblePlot_LEVEL=="Summary") my.labels$main.title <- "Growth and Achievement"
			if (bubblePlot_LEVEL=="Individual") my.labels$main.title <- "Student Growth and Achievement"
			if (bubblePlot_LEVEL=="Summary") my.labels$pdf.title <- "Bubble_Plot_(Current_Achievement)"
			if (bubblePlot_LEVEL=="Individual") my.labels$pdf.title <- "Student_Bubble_Plot_(Current_Achievement)"
		}
		return(my.labels)
	}

	names.merge <- function(tmp.data, bPlot.anonymize) {
		if ("SCHOOL_NUMBER" %in% names(tmp.data)) {
			tmp.names <- unique(data.table(sgp_object@Data[!is.na(DISTRICT_NUMBER) & !is.na(SCHOOL_NUMBER), 
				list(DISTRICT_NUMBER, DISTRICT_NAME, SCHOOL_NUMBER, SCHOOL_NAME)], key="SCHOOL_NUMBER"))
			if (bPlot.anonymize) {
				tmp.names$SCHOOL_NAME <- paste("School", tmp.data$SCHOOL_NUMBER+14)
				tmp.names$DISTRICT_NAME <- paste("District", tmp.data$NUMBER+23)
			}
			key(tmp.data) <- "SCHOOL_NUMBER"
		}
		if ("DISTRICT_NUMBER" %in% names(tmp.data)) {
			tmp.names <- unique(data.table(sgp_object@Data[!is.na(DISTRICT_NUMBER), 
				list(DISTRICT_NUMBER, DISTRICT_NAME)], key="DISTRICT_NUMBER"))
			if (bPlot.anonymize) {
				tmp.names$DISTRICT_NAME <- paste("District", tmp.data$NUMBER+23)
			}
			key(tmp.data) <- "DISTRICT_NUMBER"
		}
		tmp.names[tmp.data, mult="last"]
	}
           

	get.my.iters <- function(bPlot.data, bubblePlot_LEVEL, ...) {
		my.iters <- list()

	        # Year Stuff

		if (is.null(bPlot.years)) {
			my.iters$tmp.years <- tail(sort(unique(bPlot.data$YEAR)), 1)
		} else {
			my.iters$tmp.years <- bPlot.years
			if (is.factor(bPlot.data$YEAR)) my.iters$tmp.years <- as.factor(my.iters$tmp.years)
		}

		# Content Area Stuff

		if (is.null(bPlot.content_areas)) {
			my.iters$tmp.content_areas <- unique(bPlot.data$CONTENT_AREA) %w/o% NA
		} else {
			my.iters$tmp.content_areas <- bPlot.content_areas
			if (is.factor(bPlot.data$CONTENT_AREA)) my.iters$tmp.content_areas <- as.factor(my.iters$tmp.content_areas)
		}

		# Reconcile choice of District and Schools

		if (is.null(bPlot.schools) & is.null(bPlot.districts)) {
			my.iters$tmp.districts <- sort(unique(bPlot.data[YEAR==tmp.last.year]$DISTRICT_NUMBER)) %w/o% NA
			my.iters$tmp.schools <- sort(unique(bPlot.data[YEAR==tmp.last.year]$SCHOOL_NUMBER)) %w/o% NA
		}

		if (is.null(bPlot.schools) & !is.null(bPlot.districts)) {
         		my.iters$tmp.districts <- bPlot.districts
         		if (is.factor(bPlot.data$DISTRICT_NUMBER)) my.iters$tmp.districts <- as.factor(my.iters$tmp.districts)
			my.iters$tmp.schools <- unique(bPlot.data$SCHOOL_NUMBER[bPlot.data$DISTRICT_NUMBER %in% my.iters$tmp.districts]) %w/o% NA
		}

		if (!is.null(bPlot.schools) & is.null(bPlot.districts)) {
         		my.iters$tmp.schools <- bPlot.schools 
			if (is.factor(bPlot.data$SCHOOL_NUMBER)) my.iters$tmp.schools <- as.factor(my.iters$tmp.schools)
			my.iters$tmp.districts <- unique(bPlot.data$DISTRICT_NUMBER[bPlot.data$SCHOOL_NUMBER %in% my.iters$tmp.schools]) %w/o% NA
		}

		if (!is.null(bPlot.schools) & !is.null(bPlot.districts)) {
         		my.iters$tmp.districts <- bPlot.districts
         		my.iters$tmp.schools <- bPlot.schools 
			my.iters$tmp.schools <- unique(c(my.iters$tmp.schools, bPlot.data$SCHOOL_NUMBER[bPlot.data$DISTRICT_NUMBER %in% my.iters$tmp.districts])) %w/o% NA
			my.iters$tmp.districts <- unique(c(my.iters$tmp.districts, bPlot.data$DISTRICT_NUMBER[bPlot.data$SCHOOL_NUMBER %in% my.iters$tmp.schools])) %w/o% NA
         		if (is.factor(bPlot.data$DISTRICT_NUMBER)) my.iters$tmp.districts <- as.factor(my.iters$tmp.districts)
			if (is.factor(bPlot.data$SCHOOL_NUMBER)) my.iters$tmp.schools <- as.factor(my.iters$tmp.schools)
		}

		# y.variable (include/not include prior achievement)

		if (bPlot.prior.achievement & length(grep("PRIOR", names(bPlot.data))) > 0) {
			if (bubblePlot_LEVEL=="Summary") my.iters$tmp.y.variable <- c("PERCENT_AT_ABOVE_PROFICIENT", "PERCENT_AT_ABOVE_PROFICIENT_PRIOR")
			if (bubblePlot_LEVEL=="Individual") my.iters$tmp.y.variable <- c("SCALE_SCORE", "SCALE_SCORE_PRIOR")
		} else {
			if (bubblePlot_LEVEL=="Summary") my.iters$tmp.y.variable <- "PERCENT_AT_ABOVE_PROFICIENT"
			if (bubblePlot_LEVEL=="Individual") my.iters$tmp.y.variable <- "SCALE_SCORE"
		}
		return(my.iters)
	}


#################################################################################################################
####
#### Summary Level bubblePlots
####
#################################################################################################################

### < 100 are @Summary level bubblePlots

bubblePlot_LEVEL <- "Summary"
 

###################################################################
### bubblePlot style 1 (State level bubblePlots of Schools)
###################################################################

if (1 %in% bPlot.styles) {

		### Data sets and relevant quantities used for bubblePlots

		if (bPlot.full.academic.year) {
			bPlot.data <- sgp_object@Summary[["SCHOOL_NUMBER"]][["SCHOOL_NUMBER__CONTENT_AREA__YEAR__SCHOOL_ENROLLMENT_STATUS"]][
				SCHOOL_ENROLLMENT_STATUS=="Enrolled School: Yes"]
		} else {
			bPlot.data <- sgp_object@Summary[["SCHOOL_NUMBER"]][["SCHOOL_NUMBER__CONTENT_AREA__YEAR"]]
		}

		# Merge in school and district names and anonymize school names (if requested)

		bPlot.data <- names.merge(bPlot.data, bPlot.anonymize)

		### Get tmp.years, tmp.content_areas, and tmp.y.variable

		my.iters <- get.my.iters(bPlot.data, bubblePlot_LEVEL)

		### Start loops for bubblePlots

		for (year.iter in my.iters$tmp.years) {  ### Loop over year
		for (content_area.iter in my.iters$tmp.content_areas) { ### Loop over content areas
		for (y.variable.iter in my.iters$tmp.y.variable) {  ### Loop over CURRENT and PRIOR achievement (if requested)

		# Subset data

		tmp.data <- bPlot.data[YEAR==year.iter & CONTENT_AREA==content_area.iter & MEDIAN_SGP_COUNT >= bPlot.minimum.n]

		# Create labels

		bPlot.labels <- create.bPlot.labels(year.iter, y.variable.iter, bubblePlot_LEVEL) 


		### Create bubblePlot ###

		bubblePlot(
			bubble_plot_data.X=tmp.data[["MEDIAN_SGP"]],
			bubble_plot_data.Y=tmp.data[[y.variable.iter]],
			bubble_plot_data.SUBSET=NULL, 
			bubble_plot_data.INDICATE=NULL,
			bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
			bubble_plot_data.SIZE=tmp.data[["MEDIAN_SGP_COUNT"]],
			bubble_plot_data.LEVELS=NULL, 
			bubble_plot_data.BUBBLE_TIPS_LINES=list(paste(tmp.data[["MEDIAN_SGP"]], " (", tmp.data[["MEDIAN_SGP_COUNT"]], ")", sep=""),
				paste(tmp.data[[y.variable.iter]], " (", tmp.data[[paste(y.variable.iter, "_COUNT", sep="")]], ")", sep="")),
			bubble_plot_labels.X=c("Growth", paste(bPlot.labels$x.year.label, "Median Student Growth Percentile")),
			bubble_plot_labels.Y=c("Achievement", bPlot.labels$y.year.label),
			bubble_plot_labels.SIZE=c(50, 100, 250, 500),
			bubble_plot_labels.LEVELS=NULL, #levels(bubblePlot[["subset.factor"]]),
			bubble_plot_labels.BUBBLE_TIPS_LINES=list(paste(bPlot.labels$x.year.label, "Median SGP (Count)"),
				paste(bPlot.labels$y.year.label, " (Count)")),
			bubble_plot_labels.BUBBLE_TITLES=tmp.data[["SCHOOL_NAME"]],
			bubble_plot_titles.MAIN=bPlot.labels$main.title,
			bubble_plot_titles.SUB1=paste(state.name.label, "School Performance"),
			bubble_plot_titles.SUB2=paste(bPlot.labels$x.year.label, capwords(content_area.iter)),
			bubble_plot_titles.LEGEND1="School Size",
			bubble_plot_titles.LEGEND2_P1=NULL,
			bubble_plot_titles.LEGEND2_P2=NULL,

			bubble_plot_configs.BUBBLE_MIN_MAX=c(0.04, 0.11),
			bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
			bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5)),
			bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
			bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.01,
			bubble_plot_configs.BUBBLE_COLOR="deeppink2",
			bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
			bubble_plot_configs.BUBBLE_TIPS="TRUE",
			bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
			bubble_plot_configs.BUBBLE_PLOT_FORMAT=bPlot.format,
			bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
			bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
			bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
			bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste(state.name.label, year.iter, capwords(content_area.iter), "State", bPlot.labels$pdf.title, sep="_"), ".pdf", sep=""),
			bubble_plot_configs.BUBBLE_PLOT_PATH=file.path(bPlot.folder, year.iter, "State"),
			bubble_plot_pdftk.CREATE_CATALOG=FALSE)

		} ## END loop over y.variable.iter
		} ## End loop over content_area.iter
		} ## End loop over year.iter

} ## END bubblePlot style 1



#################################################################################################################
####
#### Individual Level bubblePlots
####
#################################################################################################################

### >= 100 are @Data level bubblePlots

bubblePlot_LEVEL <- "Individual"
 

###################################################################
### bubblePlot style 101 (Individual Student within Grade Chart)
###################################################################

if (101 %in% bPlot.styles) {

		### Create Prior Scale Score (if requested)

		if (bPlot.prior.achievement) {
			sgp_object@Data$YEAR_INTEGER_TMP <- as.integer(sgp_object@Data$YEAR) ## To convert YEAR, when factor, to integer
			key(sgp_object@Data) <- c("ID", "CONTENT_AREA", "YEAR_INTEGER_TMP", "VALID_CASE") ## CRITICAL that VALID_CASE is last in group
			sgp_object@Data$SCALE_SCORE_PRIOR <- sgp_object@Data[SJ(ID, CONTENT_AREA, YEAR_INTEGER_TMP-1), mult="last"][,SCALE_SCORE]
			sgp_object@Data$YEAR_INTEGER_TMP <- NULL
		}

		### Get tmp.years, tmp.content_areas, and tmp.y.variable

		my.iters <- get.my.iters(sgp_object@Data, bubblePlot_LEVEL)

		### Start loops for bubblePlots

		for (year.iter in my.iters$tmp.years) {  ### Loop over year
		for (content_area.iter in my.iters$tmp.content_areas) { ### Loop over content areas
		for (district.iter in my.iters$tmp.distrists) { ### Loop over districts
		for (school.iter in my.iters$tmp.schools) { ### Loop over districts

		# Subset data

		tmp.data <- sgp_object@Data[YEAR==year.iter & CONTENT_AREA==content_area.iter & DISTRICT_NUMBER==district.iter & SCHOOL_NUMBER==school.iter & !is.na(MEDIAN_SGP)]

		


		
		for (y.variable.iter in my.iters$tmp.y.variable) {  ### Loop over CURRENT and PRIOR achievement (if requested)

		

		# Merge in school and district names and anonymize school names (if requested)

		bPlot.data <- names.merge(bPlot.data, bPlot.anonymize)


		# Create labels

		bPlot.labels <- create.bPlot.labels(year.iter, y.variable.iter, bubblePlot_LEVEL) 


		### Create bubblePlot ###

		bubblePlot(
			bubble_plot_data.X=tmp.data,
			bubble_plot_data.Y=tmp.data,
			bubble_plot_data.SUBSET=NULL,
			bubble_plot_data.INDICATE=NULL,
			bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
			bubble_plot_data.SIZE=tmp.data[["MEDIAN_SGP_COUNT"]],
			bubble_plot_data.LEVELS=NULL, 
			bubble_plot_data.BUBBLE_TIPS_LINES=list(paste(tmp.data[["SGP"]], " (", tmp.data[["SGP_TARGET"]], ")", sep=""),
				paste(tmp.data[[y.variable.iter]], " (", tmp.data[[paste(y.variable.iter, "_COUNT", sep="")]], ")", sep="")),
			bubble_plot_labels.X=c("Growth", paste(bPlot.labels$x.year.label, "Student Growth Percentile")),
			bubble_plot_labels.Y=c("Achievement", bPlot.labels$y.year.label),
			bubble_plot_labels.SIZE=NULL,
			bubble_plot_labels.LEVELS=NULL, #levels(bubblePlot[["subset.factor"]]),
			bubble_plot_labels.BUBBLE_TIPS_LINES=list(paste(bPlot.labels$x.year.label, "Student Growth Percentile (Target)"),
				paste(bPlot.labels$y.year.label, " (Count)")),
			bubble_plot_labels.BUBBLE_TITLES=tmp.data[["SCHOOL_NAME"]],
			bubble_plot_titles.MAIN=bPlot.labels$main.title,
			bubble_plot_titles.SUB1=paste(state.name.label, "School Performance"),
			bubble_plot_titles.SUB2=paste(bPlot.labels$x.year.label, capwords(content_area.iter)),
			bubble_plot_titles.LEGEND1="",
			bubble_plot_titles.LEGEND2_P1=NULL,
			bubble_plot_titles.LEGEND2_P2=NULL,

			bubble_plot_configs.BUBBLE_MIN_MAX=c(0.03, 0.03),
			bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
			bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5)),
			bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
			bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.01,
			bubble_plot_configs.BUBBLE_COLOR="deeppink2",
			bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
			bubble_plot_configs.BUBBLE_TIPS="TRUE",
			bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
			bubble_plot_configs.BUBBLE_PLOT_FORMAT=bPlot.format,
			bubble_plot_configs.BUBBLE_PLOT_LEGEND="FALSE",
			bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
			bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
			bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste(state.name.label, year.iter, capwords(content_area.iter), "State", bPlot.labels$pdf.title, sep="_"), ".pdf", sep=""),
			bubble_plot_configs.BUBBLE_PLOT_PATH=file.path(bPlot.folder, year.iter, "State"),
			bubble_plot_pdftk.CREATE_CATALOG=FALSE)







##                               SUBSET=which(SUPER_0910==i),
##                               INDICATE=which(SUPER_0910==i & SCHNUM_0910 %in% School_List_101210$schnum),
                                 SIZE=MEDIAN_SGP_COUNT,
                                 LEVELS=NULL,
                                 BUBBLE_TIPS_LINES=list(paste(MEDIAN_SGP, " (", MEDIAN_SGP_COUNT, ")", sep=""),
                                                              round(PERCENT_MEETS_EXCEEDS_PROFICIENT),
                                                              round(PERCENT_FREE_REDUCED_LUNCH))),
            bubble_plot_labels=list(X=c("Poverty", paste(my.years[[j]], "Percent Free/Reduced Lunch at School")),
                                    Y=c("Growth", paste(my.years[[j]], "Median Student Growth Percentile")),
                                    SIZE=c(50, 100, 500, 1000),
                                    LEVELS=NULL,
                                    BUBBLE_TIPS_LINES=list("Median SGP (Count)",
                                                           "Percent meets/exceeds Proficient",
                                                           "Percent Free/Reduced Lunch"),
                                    BUBBLE_TITLES=SCHOOL_NAME),
                 bubble_plot_titles=list(MAIN="Growth and School Poverty",
                                         SUB1="New York State School Performance",
                                         SUB2=paste(my.years[[j]], capwords(i), "Growth & School Poverty"),
                                         LEGEND1="School Size",
                                         LEGEND2_P1="",
                                         LEGEND2_P2=""),
                 bubble_plot_configs=list(BUBBLE_MIN_MAX=c(0.03, 0.03),
                                          BUBBLE_X_TICKS=seq(0,100,10),
                                          BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5)),
                                          BUBBLE_Y_TICKS=seq(0,100,10),
                                          BUBBLE_COLOR="deeppink2",
                                          BUBBLE_TIPS="TRUE",
                                          BUBBLE_PLOT_DEVICE="PDF",
                                          BUBBLE_PLOT_FORMAT="print",
                                          BUBBLE_PLOT_LEGEND="FALSE",
                                          BUBBLE_PLOT_TITLE="TRUE",
                                          BUBBLE_PLOT_BACKGROUND_LABELS=c("Poverty", "Growth"),
                                          BUBBLE_PLOT_EXTRAS=list("grid.segments(-2, 50, 102, 50, default.units='native', gp=gpar(lty=2, col='grey40', lwd=1.3))",
                                                                  "grid.segments(-2, lm(MEDIAN_SGP ~ PERCENT_FREE_REDUCED_LUNCH)$coefficients %*% c(1,-2),
                                                                                 102, lm(MEDIAN_SGP ~ PERCENT_FREE_REDUCED_LUNCH)$coefficients %*% c(1,102),
                                                                                 default.units='native', gp=gpar(lty=5, col='white', lwd=1.3))"),
                                          BUBBLE_PLOT_NAME=paste("New York State ", i, " ", j, " Growth and School Poverty.pdf", sep=""),
                                          BUBBLE_PLOT_PATH=paste("../Figures/Bubble_Plots/State/", j, "/Growth_and_Poverty", sep="")))



		} ## END loop over y.variable.iter
		} ## End loop over content_area.iter
		} ## End loop over district.iter
		} ## End loop over school.iter
		} ## End loop over year.iter

} ## END bubblePlot style 101












} ## END bubblePlot_Styles function
