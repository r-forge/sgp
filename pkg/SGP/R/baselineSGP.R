`baselineSGP` <-
	function(sgp_object,
			 state,
			 years,
			 content_areas,
			 grade.sequences,
			 baseline.max.order,
			 return.matrices.only=FALSE,
			 ...) {

    started.at <- proc.time()
    message(paste("Started baselineSGP", date()))

	### Create state (if missing) from sgp_object (if possible)

	if (missing(state)) {
		tmp.name <- gsub("_", " ", deparse(substitute(sgp_object)))
		if (any(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name)))==1) {
			state <- c(state.abb, "DEMO")[which(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name))==1)]
		}
	}

	rbind.all <- function(.list, ...){
		if(length(.list)==1) return(.list[[1]])
		Recall(c(list(rbind(.list[[1]], .list[[2]], ...)), .list[-(1:2)]), ...)
	}

	test.year.sequence <- function(years, grades) { #bd-i had to rewrite it to get it to work with properly formatted years
		seq(min(grades),max(grades))->grade.span
		match(grades,grade.span)->index
		length(grade.span)->tmp.length
		tmp<-list()
		for (i in 1:(length(years)-tmp.length+1)) years[i:(i+tmp.length-1)][index]->tmp[[i]]
		tmp
	}

	.mergeSGP <- function(list_1, list_2) {
		for (j in c("Coefficient_Matrices", "Cutscores", "Goodness_of_Fit", "Knots_Boundaries", "SGPercentiles", "SGProjections", "Simulated_SGPs")) {
			list_1[[j]] <- c(list_1[[j]], list_2[[j]])[!duplicated(names(c(list_1[[j]], list_2[[j]])))]
		}
		for (j in c("SGPercentiles", "SGProjections", "Simulated_SGPs")) {
			if (all(names(list_2[[j]]) %in% names(list_1[[j]])) & !identical(list_1[[j]], list_2[[j]])) { #all(), not identical
				for (k in names(list_1[[j]])) {
					list_1[[j]][[k]] <- rbind.fill(list_1[[j]][[k]], list_2[[j]][[k]][!list_2[[j]][[k]][["ID"]] %in% list_1[[j]][[k]][["ID"]],]);gc()
				}
			}
		}
		for (j in c("Coefficient_Matrices", "Goodness_of_Fit", "Knots_Boundaries")) {
			for (k in names(list_1[[j]])) {
				list_1[[j]][[k]] <- c(list_1[[j]][[k]], list_2[[j]][[k]])[!duplicated(names(c(list_1[[j]][[k]], list_2[[j]][[k]])))]
			}
		}
	list_1
	}

	### Create Relevant argument-variables if missing
	key(sgp_object@Data) <- c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID")
	
	if (missing(content_areas)) content_areas <- unique(sgp_object@Data["VALID_CASE"][["CONTENT_AREA"]]) 
	if (missing(years)) years <- sort(unique(sgp_object@Data[J("VALID_CASE", content_areas)][["YEAR"]]))
	if (missing(baseline.max.order)) baseline.max.order <- length(years)-2 # AVI -- want to combine at least two cohorts of data
	if (missing(grade.sequences)) {
		valid.grades <- sort(unique(sgp_object@Data[J("VALID_CASE", content_areas)][["GRADE"]]))
		grade.sequences <- lapply(valid.grades[-1], function(x) tail(valid.grades[valid.grades <= x], (baseline.max.order+1)))    #deals with 'holes'
	}
	for (g in seq_along(grade.sequences)) {
		grade.sequences[[g]] <- grade.sequences[[g]][tail(grade.sequences[[g]],1)-grade.sequences[[g]] <= baseline.max.order] #deals with 'holes'
	}
		
	acceptable.grade.sequences <- list()
	for (a in seq_along(grade.sequences))  acceptable.grade.sequences[[a]] <- 
		eval(parse(text=paste("list(", paste("tail(grade.sequences[[", a, "]],", length(grade.sequences[[a]]):2, ")", collapse=", "), ")")))

	### Restructure data based on acceptable.grade.sequences and calculate coefficient matrices
	key(sgp_object@Data) <- c("VALID_CASE", "YEAR", "GRADE", "CONTENT_AREA") # Simpler lookup for cases to include, identical results
	
	tmp_sgp_object <- list()
	
	if (!is.null(sgp_object@SGP)) {
		for (i in names(sgp_object@SGP)) {
			tmp_sgp_object[[i]] <- sgp_object@SGP[[i]]
		}
	}

	for (h in content_areas) {
		for (i in seq_along(grade.sequences)) {
			tmp.uber.list <- list()
			for (j in acceptable.grade.sequences[[i]]) {
				tmp.year.sequence <- test.year.sequence(years, j)
				tmp.list <- list()
				for (k in seq_along(tmp.year.sequence)) {
					tmp.lookup <- data.table(CJ("VALID_CASE", tmp.year.sequence[[k]]), j, h) # Simpler lookup for cases to include, identical results
					key(tmp.lookup) <- names(tmp.lookup) <- c("VALID_CASE", "YEAR", "GRADE","CONTENT_AREA")
					tmp.list[[k]] <- reshape(sgp_object@Data[tmp.lookup, nomatch=0],
											idvar="ID", 
											timevar="GRADE", 
											direction="wide",
											drop=c("VALID_CASE", "CONTENT_AREA", "YEAR"))
					tmp.list[[k]] <- tmp.list[[k]][!apply(is.na(tmp.list[[k]]), 1, any)][,c("ID", paste("SCALE_SCORE.", j, sep="")), with=FALSE]
				}
				tmp.uber.list[[head(j,1)]] <- rbind.all(tmp.list) # kind of weird naming convention (head(j,1) but puts the data w/ most columns first for rbind.fill. 
			} ## END loop over acceptable.grade.sequences
			sgp.uber.data <- data.table(do.call(rbind.fill, tmp.uber.list))
			sgp.uber.data <- data.table(sgp.uber.data$ID, 
				as.data.frame(matrix(rep(grade.sequences[[i]], each=dim(sgp.uber.data)[1]), ncol=length(grade.sequences[[i]]))), sgp.uber.data[,-1, with=FALSE])
			if (length(acceptable.grade.sequences[[i]]) > 1) {
				key(sgp.uber.data)<-names(sgp.uber.data); key(sgp.uber.data)<-"V1" #sorts all duplicates from lowest-order to highest-order of priors
				sgp.uber.data <- sgp.uber.data[-(which(duplicated(sgp.uber.data))-1),]# removes all lower-order priors after sorting
			}
			tmp_sgp_object[["Panel_Data"]] <- data.frame(sgp.uber.data) #function doesn't take data.tables ???  thought we fixed that...

			tmp_sgp_object <- studentGrowthPercentiles(
						panel.data = tmp_sgp_object,
						sgp.labels = list(my.year="BASELINE", my.subject=h), 
						use.my.knots.boundaries = state,
						calculate.sgps = FALSE,
						drop.nonsequential.grade.progression.variables = FALSE, #always taken care of in data reshape above.
						grade.progression = grade.sequences[[i]],
						...)	
		} ## END loop over grade.sequences
	} ## END loop over content
	
    message(paste("Finished baselineSGP", date(), "in", timetaken(started.at), "\n"))
	if (return.matrices.only) {
		return(tmp_sgp_object)
	} else {
	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp_sgp_object)
	key(sgp_object@Data) <- c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID")
	return(sgp_object)
	}
  } ## END baselineSGP Function
