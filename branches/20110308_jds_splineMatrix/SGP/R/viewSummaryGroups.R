`viewSummaryGroups` <- 
function(
	summary.groups=list(institution=c("STATE", "SCHOOL_NUMBER"),
		content="CONTENT_AREA",
		time="YEAR",
		institution_level="GRADE",
		demographic=c("GENDER", "ETHNICITY", "FREE_REDUCED_LUNCH_STATUS", "ELL_STATUS", "CATCH_KEEP_UP"),
		institution_inclusion=list(STATE="OCTOBER_ENROLLMENT_STATUS", SCHOOL_NUMBER="OCTOBER_ENROLLMENT_STATUS")),
	confidence.interval.groups=list(institution="SCHOOL_NUMBER",
		content="CONTENT_AREA",
		time="YEAR",
		institution_level= NULL,
		demographic=NULL,
		institution_inclusion=list(STATE=NULL, SCHOOL_NUMBER="OCTOBER_ENROLLMENT_STATUS")),
	confidence.interval.groups.only=FALSE) {

	## Functions
	group.format <- function(my.group) {
		if (is.null(my.group)) c("")
		else {
			c("", unlist(lapply(my.group, function(x) paste(", ", x, sep=""))))
		}
	}

	## Summary tables to be produced and corresponding confidence intervals
	groups.to.summarize <- data.frame(Summary_Groups = NULL, Confidence_Interval_Calculated = NULL)
	for (i in summary.groups$institution) {
		sgp.groups <- do.call(paste, c(expand.grid(i,
			group.format(summary.groups[["content"]]),
			group.format(summary.groups[["time"]]),
			group.format(summary.groups[["institution_level"]]),
			group.format(summary.groups[["institution_inclusion"]][[i]]),
			group.format(summary.groups[["demographic"]])), sep=""))

		if (!is.null(confidence.interval.groups)) {
			ci.groups <- do.call(paste, c(expand.grid(i,
				group.format(confidence.interval.groups[["content"]]),
				group.format(confidence.interval.groups[["time"]]),
				group.format(confidence.interval.groups[["institution_level"]]),
				group.format(confidence.interval.groups[["institution_inclusion"]][[i]]),
				group.format(confidence.interval.groups[["demographic"]])), sep=""))
		}
		tmp <- data.frame(Summary_Groups = sgp.groups, Confidence_Interval_Calculated = sgp.groups %in% ci.groups)
		groups.to.summarize <- rbind(groups.to.summarize, tmp)
	} ## END summary.groups$institution
	if(confidence.interval.groups.only) groups.to.summarize <- groups.to.summarize[groups.to.summarize$Confidence_Interval_Calculated==TRUE,]
return(groups.to.summarize)
} ## END summarizeSGP function
