`combineSGP` <- 
function(sgp_object) {
	tmp.list <- list() 
        tmp.names <- names(sgp_object[["SGP"]][["SGPercentiles"]])
	for (i in tmp.names) {
		tmp.list[[i]] <- data.table(YEAR=unlist(strsplit(i, "[.]"))[2],
			CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
			sgp_object[["SGP"]][["SGPercentiles"]][[i]])
	}
	sgp_object[["Student"]] <- data.table(do.call(rbind, tmp.list), VALID_CASE=factor(1, levels=1:2, labels=c("VALID_CASE", "INVALID_CASE")), 
		key=paste(key(sgp_object[["Student"]]), collapse=","))[sgp_object[["Student"]]]
	return(sgp_object)
}
