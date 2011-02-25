`SGP` <- 
function(sgp_object=sgpData_LONG,
	years,
	content_areas,
	grades,
	state="DEMO",
	groups=list(institution=c("STATE", "SCHOOL_NUMBER"),
		content="CONTENT_AREA",
		time="YEAR",
		institution_level="GRADE",
		demographic=c("GENDER", "ETHNICITY", "FREE_REDUCED_LUNCH_STATUS", "ELL_STATUS", "CATCH_KEEP_UP"),
		institution_inclusion=list(STATE="OCTOBER_ENROLLMENT_STATUS", SCHOOL_NUMBER="OCTOBER_ENROLLMENT_STATUS")),
	sgp.summaries=list(MEDIAN_SGP="median_na(SGP)",
		MEDIAN_SGP_COUNT="num_non_missing(SGP)",
		PERCENT_AT_ABOVE_PROFICIENT="percent_in_category(PROFICIENCY_LEVEL_CODE, list(1:2), list(1:5))",
		PERCENT_AT_ABOVE_PROFICIENT_COUNT="num_non_missing(PROFICIENCY_LEVEL_CODE)"),
	confidence.interval.groups="SCHOOL_NUMBER") {

}

