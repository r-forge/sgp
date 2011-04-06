testEnv <- new.env()
setPackageName('SGP', testEnv)
load(file.path(getwd(),'data/sgpData.rda'), envir=testEnv)

.setUp <- function() {
  sys.source(file.path(getwd(), 'R/splineMatrix.R'), envir=testEnv)
  attach(testEnv)
}

.tearDown <- function() {
  removeGeneric(getGenerics(testEnv), where=testEnv)
  detach(testEnv)
}

test.test <- function() {
	checkEquals(sum(sgpData, na.rm=T), 439533703192)
}

test.splineMatrix.constructor <- function() {
  sm = new("splineMatrix")
  checkEquals(get.knots(sm), numeric(0))
  checkEquals(get.boundaries(sm), numeric(0))
  checkEquals(get.loss.hoss(sm), numeric(0))
  checkEquals(get.qrmatrix(sm), matrix())
}

test.splineMatrix.create.knots.boundaries <- function() {
  sm = new("splineMatrix")
  
  tmp.stack = data.table(grades=Reduce(append, sgpData[2:6]), scores=Reduce(append, sgpData[7:11]))
  data = tmp.stack[tmp.stack$grade == 4,]$scores
  
  sm = create.knots.boundaries(sm, data)
  checkEquals(get.knots(sm), c(544, 581, 608, 635))
  checkEquals(get.boundaries(sm), c(117.4, 868.6))
  checkEquals(get.loss.hoss(sm), c(180, 806))
}

test.splineMatrix.create.coefficient.matrices <- function() {
  sm = new("splineMatrix")

  tmp.stack = data.table(grades=Reduce(append, sgpData[2:6]), scores=Reduce(append, sgpData[7:11]))
  data = tmp.stack[tmp.stack$grade == 4,]$scores
}