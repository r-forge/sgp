`summarizeSGP` <- 
function(sgp_object) {

	## Functions


	rbind.all <- function(.list, ...){
		if(length(.list)==1) return(.list[[1]])
		Recall(c(list(rbind(.list[[1]], .list[[2]], ...)), .list[-(1:2)]), ...)
	}

	group.format <- function(my.group) {
		c("", unlist(lapply(my.group, function(x) paste(", ", x, sep=""))))
	}

	median_na <- function(x) median(x, na.rm=TRUE)
	mean_na <- function(x, result.digits=1) round(mean(x, na.rm=TRUE), digits=result.digis)
	num_non_missing <- function(x) sum(!is.na(x))

	percent_in_category <- function(x, in.categories, of.categories, result.digits=1) { ## NOTE: x must be a factor and categories levels
		if (!is.list(in.categories)) in.categories <- list(in.categories)
		if (!is.list(of.categories)) of.categories <- list(of.categories)
		tmp.result <- list()
		tmp <- summary(x[!is.na(x)])
		for (i in seq(length(in.categories))) {
			tmp.result[[i]] <-  round(100*sum(tmp[in.categories[[i]]])/sum(tmp[of.categories[[i]]]), digits=result.digits)
		}
	return(unlist(tmp.result))
	}

	sgpSummary <- function(sgp.groups.to.summarize) {
		ListExpr <- parse(text=paste("quote(as.list(c(",paste(unlist(sgp.summaries), collapse=", "),")))",sep=""))
		ByExpr <- parse(text=paste("quote(list(", paste(sgp.groups.to.summarize$sgp.groups, collapse=", "), "))", sep=""))
		tmp <- tmp.dt[, eval(eval(ListExpr)), by=eval(eval(ByExpr))]
		names(tmp)[-seq(length(unlist(strsplit(as.character(sgp.groups.to.summarize$sgp.groups), ", "))))] <- unlist(strsplit(names(sgp.summaries), "[.]"))
		if (sgp.groups.to.summarize$confidence.intervals) {
			SIM_ByExpr <- parse(text=paste("quote(list(", paste(sgp.groups.to.summarize$sgp.groups, collapse=", "), ", SIM_ITERATION))", sep=""))
			tmp.sim <- data.table(tmp.simulation.dt, tmp.dt[tmp.simulation.dt, eval(eval(ByExpr))])[,
				median(SIM_SGP), by=eval(eval(SIM_ByExpr))][,
				as.list(quantile(V1, probs=c(0.025, 0.975))), by=eval(eval(ByExpr))]
			names(tmp.sim)[(dim(tmp.sim)[2]-1):dim(tmp.sim)[2]] <- c("LOWER_MEDIAN_SGP_95_CONF_BOUND", "UPPER_MEDIAN_SGP_95_CONF_BOUND")
			tmp <- data.table(tmp, tmp.sim[,(dim(tmp.sim)[2]-1):dim(tmp.sim)[2]])
		}
		print(paste("Finished with", sgp.groups.to.summarize$sgp.groups))
		return(tmp)
	}

	mySgpGroups <- function(sgp.groups, confidence.interval.groups) {
		if (missing(confidence.interval.groups)) {
			return(data.frame(sgp.groups=sgp.groups, confidence.intervals=FALSE))
		}
		if (toupper(confidence.interval.groups)=="ALL") {
			return(data.frame(sgp.groups=sgp.groups, confidence.intervals=TRUE))
		} else {
			tmp <- data.frame(sgp.groups=sgp.groups, confidence.intervals=FALSE)
			tmp$confidence.intervals[sgp.groups %in% confidence.interval.groups] <- TRUE
			return(tmp)
		}
	}

	combineSims <- function(tmp_sgp_object) {
		tmp.list <- list()
		tmp.names <- names(tmp_sgp_object[["SGP"]][["Simulated_SGPs"]]) 
		for(i in tmp.names) {
			tmp.list[[i]] <- data.frame(data.frame(ID=rep(tmp_sgp_object[["SGP"]][["Simulated_SGPs"]][[i]][,1], 
				dim(tmp_sgp_object[["SGP"]][["Simulated_SGPs"]][[i]])[2]-1),
				stack(tmp_sgp_object[["SGP"]][["Simulated_SGPs"]][[i]][,-1])),
				YEAR=unlist(strsplit(i, "[.]"))[2],
				CONTENT_AREA=unlist(strsplit(i, "[.]"))[1])
		}
		tmp.simulation.dt <- rbind.all(tmp.list)
		names(tmp.simulation.dt)[2:3] <- c("SGP_SIM", "SGP_SIM_ITERATION")
		tmp.simulation.dt <- data.table(tmp.simulation.dt, VALID_CASE=factor(1, levels=1:2, labels=c("VALID_CASE", "INVALID_CASE")))
		key(tmp.simulation.dt) <- key(tmp.dt)
		return(tmp.simulation.dt)
	}


	## Take subset of data

	tmp.dt <- data.table(STATE=state, sgp_object[["Student"]][CJ("VALID_CASE", content_areas, years), mult="all"])
	key(tmp.dt) <- c("VALID_CASE", "ID", "CONTENT_AREA", "YEAR")

	if (!is.null(confidence.interval.groups)) {
		long.sim.data <- combineSims(sgp_object)
	}

	## Create summary tables

	for (i in groups$institution) {
		sgp.groups <- data.frame(sgp.groups=do.call(paste, c(expand.grid(i,
			group.format(groups[["content"]]),
			group.format(groups[["time"]]),
			group.format(groups[["institution_level"]]),
			group.format(groups[["institution_inclusion"]][[i]]),
			group.format(groups[["demographic"]])), sep="")))

	foreach(i=iter(mySgpGroups(sgp.groups, confidence.interval.groups), by="row"), 
		.options.multicore = mc.options, .packages="data.table", .inorder=FALSE) %dopar% {return(sgpSummary(i))}
	names(sgp_object[["SGP"]][["Summary"]][[i]]) <- sgp.groups
	}



}





