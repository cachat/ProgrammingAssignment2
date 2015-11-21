## These functions cache the inverse of a matrix. 
## First, a special matrix called makeCacheMatrix is created for every matrix we want to inverse.
## Then cacheSolve checks if the inverse of a matrix has been calculated before.
## If so, the inverse is returned by retrieving cached data. If not, the functions calculate the inverse and return it.

## makeCacheMatrix creates a special "matrix", which is a really a list containing four functions:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x=matrix()){
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setinverse<-function(solve) m<<-solve
	getinverse<-function() m
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve caculates the inverse of the matrix created above.
## If the inverse has already been calculated before by cacheSolve and the matrix has not changed,
## cacheSolve will retrieve the inverse from the cache. Otherwise, cacheSolve will calculate the inverse and return it.

cacheSolve<-function(x,...){
	m<-x$getinverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data<- x$get()
	m<-solve(data, ...)
	x$setinverse(m)
	m
}
