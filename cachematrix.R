## Functions that store and calculate information on a matrix in a list, including values and the inverse
## Furthermore, contains function to cache results, meaning inverse does not need to be recalculated but merely pulled
## from the cachematrix


## Generates a list of functions from a matrix, containing summary information on the matrix, and it's inverse. On updating, wipes
## stored information


makeCacheMatrix <- function(x = matrix()) {


	## Wipes stored inverse
	inv<-NULL

  	
	set=function(y){
    		x<<-y
    		inv<<-NULL
    	}
  	get=function()x
	setinv=function(inverse) inv<<-inverse
	getinv=function() inv

	## Creates List of functions/information
	list(set=set, get=get, setinv=setinv, getinv=getinv)


}


## Returns the inverse of a matrix : First checks whether matrix exists, in which case is simply returned alongside message
## Otherwise, inverse is calculated and stored


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	inv = x$getinv()
	
	##Check inverse matrix calculated

	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}

	##calculate inverse

	inv.data=x$get()
	inv=solve(inv.data,...)

	x$setinv
  
	return(inv)
}
