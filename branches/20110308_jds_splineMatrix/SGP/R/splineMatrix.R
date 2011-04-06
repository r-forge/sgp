##########################################################
###
### Class Definition and Constructor
###
##########################################################

setClass(
	'splineMatrix',
	representation = representation(
    knots = "numeric",
    boundaries = "numeric",
    loss.hoss = "numeric",
    qrmatrix = "matrix"),
	prototype = NULL
)

setMethod(
	'initialize',
	'splineMatrix',
	function(.Object) {
    .Object@knots = numeric(0)
    .Object@boundaries = numeric(0)
    .Object@loss.hoss = numeric(0)
    .Object@qrmatrix = matrix()
    
    .Object
	}
)

##########################################################
###
### Accessor Functions
###
##########################################################

if (!isGeneric('get.knots')) {
	setGeneric('get.knots', 
    function(.Object) standardGeneric('get.knots'))
}

setMethod(
	'get.knots',
	'splineMatrix',
	function(.Object) {
		.Object@knots
	}
)

if (!isGeneric('get.boundaries')) {
  setGeneric('get.boundaries', 
    function(.Object) standardGeneric('get.boundaries'))
}

setMethod(
	'get.boundaries',
	'splineMatrix',
	function(.Object) {
		.Object@boundaries
	}
)

if (!isGeneric('get.loss.hoss')) {
  setGeneric('get.loss.hoss', 
    function(.Object) standardGeneric('get.loss.hoss'))
}

setMethod(
	'get.loss.hoss',
	'splineMatrix',
	function(.Object) {
		.Object@loss.hoss
	}
)

if (!isGeneric('get.qrmatrix')) {
  setGeneric('get.qrmatrix', 
    function(.Object) standardGeneric('get.qrmatrix'))
}

setMethod(
  'get.qrmatrix',
	'splineMatrix',
	function(.Object) {
		.Object@qrmatrix
	}
)

##########################################################
###
### Methods
###
##########################################################

if (!isGeneric('create.knots.boundaries')) {
  setGeneric('create.knots.boundaries', 
    function(.Object, data, knot.cut.percentiles=c(0.2,0.4,0.6,0.8)) standardGeneric('create.knots.boundaries'))
}

setMethod(
  'create.knots.boundaries',
	'splineMatrix',
	function(.Object, data, knot.cut.percentiles=c(0.2,0.4,0.6,0.8)) {
		.Object@knots = round(as.vector(quantile(data, probs=knot.cut.percentiles, na.rm=TRUE)), digits=3)
    .Object@boundaries <- round(as.vector(extendrange(data, f=0.1)), digits=3)  
    .Object@loss.hoss <- round(as.vector(extendrange(data, f=0.0)), digits=3)  
    
    .Object
	}
)