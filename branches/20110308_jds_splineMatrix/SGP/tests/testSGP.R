testEnv <- new.env()
setPackageName('SGP', testEnv)
load(file.path(getwd(),'pkg/SGP/data/sgpData.rda'), envir=testEnv)

.setUp <- function() {
  sys.source(file.path(getwd(), 'pkg/SGP/R/splineMatrix.R'), envir=testEnv)
  attach(testEnv)
}

.tearDown <- function() {
  removeGeneric(getGenerics(testEnv), where=testEnv)
  detach(testEnv)
}

test.test <- function() {
	checkEquals(sum(sgpData, na.rm=T), 439533703192)
}

test.splineMatrix.create.knots.boundaries <- function() {
}
