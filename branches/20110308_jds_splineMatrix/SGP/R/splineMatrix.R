##########################################################
###
### Class Definition and Constructor
###
##########################################################

setClass(
	'splineMatrix',
	representation = representation(
    knots = "matrix",
    boundaries = "matrix",
    loss.hoss = "matrix",
    qrmatrix = "matrix"),
	prototype = NULL
)

setMethod(
	'initialize',
	'splineMatrix',
	function(.Object) {
    .Object@knots = matrix()
    .Object@boundaries = matrix()
    .Object@loss.hoss = matrix()
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
		.Object@knots = matrix(unlist(
      Map(function(d) round(as.vector(quantile(d, probs=c(0.2,0.4,0.6,0.8), na.rm=TRUE)), digits=3), data)),
      nrow=length(knot.cut.percentiles))
    .Object@boundaries = matrix(unlist(
      Map(function(d) round(as.vector(extendrange(d, f=0.1)), digits=3), data)),
      nrow=2)
    .Object@loss.hoss = matrix(unlist(
      Map(function(d) round(as.vector(extendrange(d, f=0.0)), digits=3), data)),
      nrow=2)  
    
    .Object
	}
)