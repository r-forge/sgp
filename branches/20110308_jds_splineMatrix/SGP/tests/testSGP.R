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
  checkEquals(get.knots(sm), matrix())
  checkEquals(get.boundaries(sm), matrix())
  checkEquals(get.loss.hoss(sm), matrix())
  checkEquals(get.qrmatrix(sm), matrix())
}

test.splineMatrix.create.knots.boundaries <- function() {
  sm = new("splineMatrix")
  
  tmp.stack = data.table(grades=Reduce(append, sgpData[2:6]), scores=Reduce(append, sgpData[7:11]))
  data = list(
    tmp.stack[tmp.stack$grade == 3,]$scores,
    tmp.stack[tmp.stack$grade == 4,]$scores)
  
  sm = create.knots.boundaries(sm, data)
  checkEquals(get.knots(sm), matrix(c(504, 546, 579, 615, 544, 581, 608, 635),nrow=4))
  checkEquals(get.boundaries(sm), matrix(c(85.5, 859.5, 117.4, 868.6), nrow=2))
  checkEquals(get.loss.hoss(sm), matrix(c(150, 795, 180, 806), nrow=2))
}

test.splineMatrix.create.coefficient.matrix_1_prior <- function() {
  sm = new("splineMatrix")
  
  tmp.stack = data.table(grades=Reduce(append, sgpData[2:6]), scores=Reduce(append, sgpData[7:11]))
  data = list(
    tmp.stack[tmp.stack$grade == 3,]$scores)
  taus = (1:100 - 0.5)/100
    
  sm = create.knots.boundaries(sm, data)
  sm = create.coefficient.matrix(sm, data, taus)
  
  checkEquals(sum(load('tests/rq_matrix1.R'), sum(get.qrmatrix(sm))))
}