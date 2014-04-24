## Put comments here that give an overall description of what your
## functions do

## 
## makeCacheMatrix function creates a special "vector" which
## is really a list containing functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverted matrix
## 4. get the value of the inverted matrix
makeCacheMatrix <- function(x = matrix()) {

    im <- NULL
    ##set the value of the matrix
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    
    ##get the value of the matrix
    get <- function() x
    
    ##set the value of the inverted matrix
    ##from the passed in parm z
    setim <- function(z) im <<- z
    
    ##get the value of the inverted matrix   
    getim <- function() im
    
    list(set = set, get = get,
         setim = setim,
         getim = getim)
    
}


## computes the inverse matrix x of the special vector
## created by makeCacheMatrix
## Return cached data if the inverse matrix is already computed
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    im <- x$getim()
    if(!is.null(im)) {
        message("--getting cached data")
        return(im)
    }
    
    ## Data not cached. Compute the inverse matrix via solve(a,b)
    data <- x$get()
    im <- solve(data,data, ...)
    
    ##Caches the solved inverse matrix
    x$setim(im)
    im
}

testCache<-function(c=NULL) {
    ##default 2x2 matrix
    if(is.null(c)){
        c=rbind(c(1, -1/4), c(-1/4, 1))
    }
    
    ##create the list of functions set,get,setim and getim
    message("create list of functions")
    b<-makeCacheMatrix(c)
    
    ##execute the cacheim the first time on object b
    ##should go ahead and compute the inverse matrix
    message("execute the first time")
    cacheSolve(b)
    
    ##execute the cacheim the second time on object b
    ##should use the cached inverse matrix
    message("execute the second time")
    cacheSolve(b)
    
    #creat a new object b
    message("create a new list of functions")
    b<-makeCacheMatrix(c)
    
    ##execute the cacheim the first time on object b
    ##should go ahead and compute the inverse matrix
    message("execute first time on new object")
    cacheSolve(b)
    
}