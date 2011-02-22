`combineSGP` <- 
function(sgp_object) {
	tmp.list <- list() 
        tmp.names <- names(sgp_object[["SGP"]][["SGPercentiles"]])
	for (i in tmp.names) {
		tmp.list[[i]] <- data.table(SCH_YR=unlist(strsplit(tmp.names, "[.]"))[2],
			SUBJECT_CODE=unlist(strsplit(tmp.names, "[.]"))[1],
			sgp_object[["SGPercentiles"]][[tmp.names]])
	}
	sgp_object[["Student"]] <- data.table(do.call(rbind, tmp.list), VALID_CASE=factor(1, levels=1:2, labels=c("Valid_Case", "Invalid_Case")), 
		key=paste(key(sgp_object[["Student"]]), collapse=","))[sgp_object[["Student"]]]
}
