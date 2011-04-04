setClass(
	'splineMatrix',
	representation = representation(
		data="data.frame", 
		by.grade="logical"),
	prototype = NULL
)

setMethod(
	'initialize',
	'splineMatrix',
	function(.Object, data, by.grade) {
		.Object@data = data
		.Object
	}
)

if (!isGeneric('get.data')) {
	setGeneric('get.data', function(.Object) standardGeneric('get.data'))
}

setMethod(
	'get.data',
	'splineMatrix',
	function(.Object) {
		.Object@data
	}
)