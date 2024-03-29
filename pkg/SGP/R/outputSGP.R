`outputSGP` <- 
function(sgp_object,
	state,
	output.type="SchoolView_WIDE",
	baseline.sgps=FALSE,
	outputSGP.years=NULL,
	outputSGP.content_areas=NULL,
	outputSGP.anonymize=FALSE) {


	### Create state (if missing) from sgp_object (if possible)

        if (missing(state)) {
                tmp.name <- gsub("_", " ", deparse(substitute(sgp_object)))
                if (any(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name)))==1) {
                        state <- c(state.abb, "DEMO")[which(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name))==1)]
                }
        }


if ("SchoolView_WIDE" %in% output.type) {

	### Define varaibles (to prevent R CMD check warnings)

	SCALE_SCORE <- CONTENT_AREA <- YEAR <- GRADE <- ID <- ETHNICITY <- GENDER <- LAST_NAME <- FIRST_NAME <- VALID_CASE <- NULL


	### Utility functions

	rbind.all <- function(.list, ...) {
		if (length(.list)==1) return (.list[[1]])
		Recall(c(list(rbind(.list[[1]], .list[[2]], ...)), .list[-(1:2)]), ...)
	}

	.year.increment <- function(year, increment) {
		paste(as.numeric(unlist(strsplit(as.character(year), "_")))+increment, collapse="_")
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

	piecewise.transform <- function(scale_score, state, content_area, year, grade, output.digits=1) {
		if (content_area %in% names(stateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]]) &
			grade %in% as.numeric(matrix(unlist(strsplit(names(stateData[[state]][["Achievement"]][["Knots_Boundaries"]][[content_area]]), "_")), 
				ncol=2, byrow=TRUE)[,2])) {

			tmp.loss.hoss <- stateData[[state]][["Achievement"]][["Knots_Boundaries"]][[content_area]][[paste("loss.hoss_", grade, sep="")]]
			my.content_area <- get.my.cutscore.year(state, content_area, year)
			tmp.old.cuts <- c(tmp.loss.hoss[1], stateData[[state]][["Achievement"]][["Cutscores"]][[my.content_area]][[paste("GRADE_", grade, sep="")]], 
				tmp.loss.hoss[2])
			tmp.new.cuts <- stateData[[state]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]][[content_area]]
			tmp.index <- findInterval(scale_score, tmp.old.cuts, rightmost.closed=TRUE)
			tmp.diff <- diff(tmp.new.cuts)/diff(tmp.old.cuts)
			round(tmp.new.cuts[tmp.index] + (scale_score - tmp.old.cuts[tmp.index]) * (diff(tmp.new.cuts)/diff(tmp.old.cuts))[tmp.index], digits=output.digits)
		} else {
			as.numeric(scale_score)
		}
	} ## END piecewise.transform

	"%w/o%" <- function(x,y) x[!x %in% y]


	#### Set key

	long.key <- c("VALID_CASE", "YEAR", "CONTENT_AREA", "DISTRICT_NUMBER", "SCHOOL_NUMBER")
	key(sgp_object@Data) <- long.key


	#### Year stuff 

	tmp.years <- sort(unique(sgp_object@Data[J("VALID_CASE")][["YEAR"]]))
	tmp.last.year <- tail(tmp.years, 1)


	#### Content area stuff 

	if (is.null(outputSGP.content_areas)) {
		tmp.content_areas <- sort(unique(sgp_object@Data[J("VALID_CASE", tmp.last.year)][["CONTENT_AREA"]]))
	} else {
		tmp.content_areas <- as.factor(outputSGP.content_areas)
	}


	### Get data

	tmp.table <- sgp_object@Data

	### Create transformed scale scores (NOT necessary if wide data is provided)

	key(tmp.table) <- c("CONTENT_AREA", "YEAR", "GRADE")
	tmp.table$TRANSFORMED_SCALE_SCORE <- tmp.table[,
		piecewise.transform(SCALE_SCORE, state, as.character(CONTENT_AREA[1]), as.character(YEAR[1]), as.character(GRADE[1])), 
			by=list(CONTENT_AREA, YEAR, GRADE)]$V1


	#### Anonymize (if requested) (NOT necessary if wide data is provided)
     
	if (outputSGP.anonymize) {
		require(randomNames)
                if (!"ETHNICITY" %in% names(tmp.table)) tmp.table[["ETHNICITY"]] <- 1
                if (!"GENDER" %in% names(tmp.table)) tmp.table[["GENDER"]] <- round(runif(dim(tmp.table)[1], min=0, max=1))
		tmp.dt <- tmp.table[,list(ID, ETHNICITY, GENDER)]
		key(tmp.dt) <- "ID"
		tmp.dt <- tmp.dt[!duplicated(tmp.dt),]

		tmp.dt$LAST_NAME <- randomNames(gender=tmp.dt$GENDER, ethnicity=tmp.dt$ETHNICITY, which.names="last")
		tmp.dt$FIRST_NAME <- randomNames(gender=tmp.dt$GENDER, ethnicity=tmp.dt$ETHNICITY, which.names="first")

		names.dt <- tmp.dt[,list(ID, LAST_NAME, FIRST_NAME)]
		key(names.dt) <- "ID"

		key(tmp.table) <- "ID"
		tmp.table <- names.dt[tmp.table]
	} ## END if (outputSGP.anonymize)


	### Reshape data set

	variables.to.keep <- c("VALID_CASE", "ID", "LAST_NAME", "FIRST_NAME", "CONTENT_AREA", "YEAR", "GRADE", "EMH_LEVEL", 
		"SCALE_SCORE", "TRANSFORMED_SCALE_SCORE", "ACHIEVEMENT_LEVEL", "SGP", "SGP_TARGET", "SCHOOL_NUMBER", "DISTRICT_NUMBER",
		"GENDER", "ETHNICITY", "FREE_REDUCED_LUNCH_STATUS", "ELL_STATUS", "IEP_STATUS", "GIFTED_AND_TALENTED_PROGRAM_STATUS",
		"SCHOOL_ENROLLMENT_STATUS", "DISTRICT_ENROLLMENT_STATUS", "STATE_ENROLLMENT_STATUS")

	outputSGP.data <- reshape(tmp.table[VALID_CASE=="VALID_CASE", variables.to.keep, with=FALSE],
		idvar=c("ID", "CONTENT_AREA"),
		timevar="YEAR",
		drop=c("VALID_CASE"),
		direction="wide")


	#### Merge in 1, 2, and 3 year projections 

	for (j in 1:3) {
		tmp.proj.names <- paste(tmp.content_areas, tmp.last.year, sep=".")
		if (all(tmp.proj.names %in% names(sgp_object@SGP[["SGProjections"]]))) {
			key(outputSGP.data) <- c("ID", "CONTENT_AREA")
			tmp.list <- list()
			for (i in tmp.proj.names) {
				tmp.list[[i]] <- data.table(CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
					sgp_object@SGP[["SGProjections"]][[i]][,c(1, grep(paste("PROJ_YEAR", j, sep="_"), names(sgp_object@SGP[["SGProjections"]][[i]])))])
			}
			outputSGP.data <- data.table(rbind.all(tmp.list), key=paste(key(outputSGP.data), collapse=","))[outputSGP.data]
			tmp.grade.name <- paste("GRADE", tmp.last.year, sep=".")
			tmp.year.name <- .year.increment(tmp.last.year, j)
			key(outputSGP.data) <- c("CONTENT_AREA", tmp.grade.name)
			for (proj.iter in grep(paste("PROJ_YEAR", j, sep="_"), names(outputSGP.data))) {
			tmp.scale_score.name <- names(outputSGP.data)[proj.iter]
			outputSGP.data[[proj.iter]] <- outputSGP.data[,
				piecewise.transform(get(tmp.scale_score.name), state, as.character(CONTENT_AREA[1]), tmp.year.name, get(tmp.grade.name)[1]+1), 
				by=list(CONTENT_AREA, outputSGP.data[[tmp.grade.name]])]$V1 
			}
		}
	}


	#### Rename variables (needs to be improved)

	tmp.order <- c("CY", "PY1", "PY2", "PY3", "PY4", "PY5", "PY6", "PY7")

	
	## Rename variables to keep

	names(outputSGP.data)[which(names(outputSGP.data)=="ID")] <- "STATE_ASSIGNED_ID"
	names(outputSGP.data)[which(names(outputSGP.data)==paste("LAST_NAME", tmp.last.year, sep="."))] <- "LAST_NAME"
	names(outputSGP.data)[which(names(outputSGP.data)==paste("FIRST_NAME", tmp.last.year, sep="."))] <- "FIRST_NAME"
	names(outputSGP.data)[which(names(outputSGP.data)==paste("DISTRICT_NUMBER", tmp.last.year, sep="."))] <- "DISTRICT_NUMBER"
	names(outputSGP.data)[which(names(outputSGP.data)==paste("SCHOOL_NUMBER", tmp.last.year, sep="."))] <- "SCHOOL_NUMBER"
	names(outputSGP.data)[which(names(outputSGP.data)==paste("EMH_LEVEL", tmp.last.year, sep="."))] <- "EMH_LEVEL"
	names(outputSGP.data)[which(names(outputSGP.data)==paste("ETHNICITY", tmp.last.year, sep="."))] <- "ETHNICITY"
	names(outputSGP.data)[which(names(outputSGP.data)==paste("GENDER", tmp.last.year, sep="."))] <- "GENDER"
	names(outputSGP.data)[which(names(outputSGP.data)==paste("IEP_STATUS", tmp.last.year, sep="."))] <- "IEP_CODE"
	names(outputSGP.data)[which(names(outputSGP.data)==paste("FREE_REDUCED_LUNCH_STATUS", tmp.last.year, sep="."))] <- "FRL_CODE"
	names(outputSGP.data)[which(names(outputSGP.data)==paste("GIFTED_AND_TALENTED_PROGRAM_STATUS", tmp.last.year, sep="."))] <- "GIFTED_CODE"
	names(outputSGP.data)[which(names(outputSGP.data)==paste("ELL_STATUS", tmp.last.year, sep="."))] <- "LANGUAGE_PROFICIENCY"
	names(outputSGP.data)[which(names(outputSGP.data)==paste("SCHOOL_ENROLLMENT_STATUS", tmp.last.year, sep="."))] <- "SCHOOL_ENROLLMENT_STATUS"
	names(outputSGP.data)[which(names(outputSGP.data)==paste("DISTRICT_ENROLLMENT_STATUS", tmp.last.year, sep="."))] <- "DISTRICT_ENROLLMENT_STATUS"
	names(outputSGP.data)[which(names(outputSGP.data)==paste("STATE_ENROLLMENT_STATUS", tmp.last.year, sep="."))] <- "STATE_ENROLLMENT_STATUS"

	for (i in seq_along(tmp.years)) {	
		names(outputSGP.data)[grep(paste("GRADE", rev(tmp.years)[i], sep="."), names(outputSGP.data))] <- paste("GRADE_LEVEL", tmp.order[i], sep="_")
		names(outputSGP.data)[grep(paste("SCALE_SCORE", rev(tmp.years)[i], sep="."), names(outputSGP.data)) %w/o% 
			grep(paste("TRANSFORMED_SCALE_SCORE", rev(tmp.years)[i], sep="."), names(outputSGP.data))] <- 
			paste("SCALE_SCORE", tmp.order[i], sep="_")
		names(outputSGP.data)[grep(paste("TRANSFORMED_SCALE_SCORE", rev(tmp.years)[i], sep="."), names(outputSGP.data))] <- paste("TRANSFORMED_SCALE_SCORE", tmp.order[i], sep="_")
		names(outputSGP.data)[grep(paste("SGP_TARGET", rev(tmp.years)[i], sep="."), names(outputSGP.data))] <- paste("GROWTH_TARGET", tmp.order[i], sep="_")
		names(outputSGP.data)[grep(paste("SGP", rev(tmp.years)[i], sep="."), names(outputSGP.data))] <- paste("GROWTH_PERCENTILE", tmp.order[i], sep="_")
		names(outputSGP.data)[grep(paste("ACHIEVEMENT_LEVEL", rev(tmp.years)[i], sep="."), names(outputSGP.data))] <- paste("PERFORMANCE_LEVEL", tmp.order[i], sep="_")
	}
	
	## NULLify variable to be removed

	for (i in head(tmp.years, -1)) {
		outputSGP.data[[paste("LAST_NAME", i, sep=".")]] <- NULL
		outputSGP.data[[paste("FIRST_NAME", i, sep=".")]] <- NULL
		outputSGP.data[[paste("DISTRICT_NUMBER", i, sep=".")]] <- NULL
		outputSGP.data[[paste("SCHOOL_NUMBER", i, sep=".")]] <- NULL
		outputSGP.data[[paste("EMH_LEVEL", i, sep=".")]] <- NULL
		outputSGP.data[[paste("ETHNICITY", i, sep=".")]] <- NULL
		outputSGP.data[[paste("GENDER", i, sep=".")]] <- NULL
		outputSGP.data[[paste("IEP_STATUS", i, sep=".")]] <- NULL
		outputSGP.data[[paste("FREE_REDUCED_LUNCH_STATUS", i, sep=".")]] <- NULL
		outputSGP.data[[paste("GIFTED_AND_TALENTED_PROGRAM_STATUS", i, sep=".")]] <- NULL
		outputSGP.data[[paste("ELL_STATUS", i, sep=".")]] <- NULL
		outputSGP.data[[paste("SCHOOL_ENROLLMENT_STATUS", i, sep=".")]] <- NULL
		outputSGP.data[[paste("DISTRICT_ENROLLMENT_STATUS", i, sep=".")]] <- NULL
		outputSGP.data[[paste("STATE_ENROLLMENT_STATUS", i, sep=".")]] <- NULL
	}


	## Create missing variables

	outputSGP.data[["YEAR"]] <- tmp.last.year
	outputSGP.data[["STUDENT_GROWTH_ID"]] <- seq(dim(outputSGP.data)[1])
	outputSGP.data[["MIDDLE_NAME"]] <- as.character(NA)
	outputSGP.data[["HLS_CODE"]] <- as.character(NA)
	outputSGP.data[["OCTOBER_ENROLLMENT_STATUS"]] <- as.character(NA)

	if (length(tmp.years) < length(tmp.order)) {
		for (i in tmp.order[(length(tmp.years)+1):length(tmp.order)]) {
			outputSGP.data[[paste("GRADE_LEVEL", i, sep="_")]] <- NA
			outputSGP.data[[paste("SCALE_SCORE", i, sep="_")]] <- NA
			outputSGP.data[[paste("TRANSFORMED_SCALE_SCORE", i, sep="_")]] <- NA
			outputSGP.data[[paste("GROWTH_TARGET", i, sep="_")]] <- NA
			outputSGP.data[[paste("GROWTH_PERCENTILE", i, sep="_")]] <- NA
			outputSGP.data[[paste("PERFORMANCE_LEVEL", i, sep="_")]] <- NA
		}
	}	

	if (length(names(outputSGP.data)[grep("YEAR_1", names(outputSGP.data))]) == 4) {
		for (i in 1:3) {
			names(outputSGP.data)[grep(paste("YEAR", i, sep="_"), names(outputSGP.data))] <- paste("CUT", c(1, 35, 65, 99), "YEAR", i, sep="_")
			for (j in c(20, 40, 60, 80)) {
				outputSGP.data[[paste("CUT", j, "YEAR", i, sep="_")]] <- NA	
			}
		}
	}
	if (length(names(outputSGP.data)[grep("YEAR_1", names(outputSGP.data))]) == 6) {
		for (i in 1:3) {
			names(outputSGP.data)[grep(paste("YEAR", i, sep="_"), names(outputSGP.data))] <- paste("CUT", c(1, 20, 40, 60, 80, 99), "YEAR", i, sep="_")
			for (j in c(35, 65)) {
				outputSGP.data[[paste("CUT", j, "YEAR", i, sep="_")]] <- NA	
			}
		}
	}

	## Rearrange variables

	tmp.variable.names <- c("STUDENT_GROWTH_ID", "STATE_ASSIGNED_ID", "LAST_NAME", "FIRST_NAME", "MIDDLE_NAME", 
		"CONTENT_AREA", "YEAR", "DISTRICT_NUMBER", "SCHOOL_NUMBER", "EMH_LEVEL", 
		"ETHNICITY", "GENDER", "IEP_CODE", "FRL_CODE", "HLS_CODE", "GIFTED_CODE", "LANGUAGE_PROFICIENCY",
		"OCTOBER_ENROLLMENT_STATUS", "SCHOOL_ENROLLMENT_STATUS", "DISTRICT_ENROLLMENT_STATUS", "STATE_ENROLLMENT_STATUS",
		"GRADE_LEVEL_CY", "SCALE_SCORE_CY", "TRANSFORMED_SCALE_SCORE_CY", "PERFORMANCE_LEVEL_CY", "GROWTH_PERCENTILE_CY", "GROWTH_TARGET_CY",
		"GRADE_LEVEL_PY1", "SCALE_SCORE_PY1", "TRANSFORMED_SCALE_SCORE_PY1", "PERFORMANCE_LEVEL_PY1", "GROWTH_PERCENTILE_PY1", "GROWTH_TARGET_PY1",
		"GRADE_LEVEL_PY2", "SCALE_SCORE_PY2", "TRANSFORMED_SCALE_SCORE_PY2", "PERFORMANCE_LEVEL_PY2", "GROWTH_PERCENTILE_PY2", "GROWTH_TARGET_PY2",
		"GRADE_LEVEL_PY3", "SCALE_SCORE_PY3", "TRANSFORMED_SCALE_SCORE_PY3", "PERFORMANCE_LEVEL_PY3", "GROWTH_PERCENTILE_PY3", "GROWTH_TARGET_PY3",
		"GRADE_LEVEL_PY4", "SCALE_SCORE_PY4", "TRANSFORMED_SCALE_SCORE_PY4", "PERFORMANCE_LEVEL_PY4", "GROWTH_PERCENTILE_PY4", "GROWTH_TARGET_PY4",
		"GRADE_LEVEL_PY5", "SCALE_SCORE_PY5", "TRANSFORMED_SCALE_SCORE_PY5", "PERFORMANCE_LEVEL_PY5", "GROWTH_PERCENTILE_PY5", "GROWTH_TARGET_PY5",
		"GRADE_LEVEL_PY6", "SCALE_SCORE_PY6", "TRANSFORMED_SCALE_SCORE_PY6", "PERFORMANCE_LEVEL_PY6", "GROWTH_PERCENTILE_PY6", "GROWTH_TARGET_PY6",
		"GRADE_LEVEL_PY7", "SCALE_SCORE_PY7", "TRANSFORMED_SCALE_SCORE_PY7", "PERFORMANCE_LEVEL_PY7", "GROWTH_PERCENTILE_PY7", "GROWTH_TARGET_PY7",
		"CUT_1_YEAR_1", "CUT_99_YEAR_1", "CUT_35_YEAR_1", "CUT_65_YEAR_1", "CUT_20_YEAR_1", "CUT_40_YEAR_1", "CUT_60_YEAR_1", "CUT_80_YEAR_1",
		"CUT_1_YEAR_2", "CUT_99_YEAR_2", "CUT_35_YEAR_2", "CUT_65_YEAR_2", "CUT_20_YEAR_2", "CUT_40_YEAR_2", "CUT_60_YEAR_2", "CUT_80_YEAR_2",
		"CUT_1_YEAR_3", "CUT_99_YEAR_3", "CUT_35_YEAR_3", "CUT_65_YEAR_3", "CUT_20_YEAR_3", "CUT_40_YEAR_3", "CUT_60_YEAR_3", "CUT_80_YEAR_3")

	outputSGP.data[,tmp.variable.names, with=FALSE]

} ## End if SchoolView_WIDE %in% output.type



} ## END outputSGP

