## Below you will find my code to create a matrix-like object that contains the functions to 
## set the value of the matrix, get the value of the matrix, set the value of the inverse,
## and get the value of the inverse.  These objects combined will allow the second function, 
## cacheSolve, to calculate and produce quickly if need be, the inverse of the matrix.  
## This function assumes the matrix issquare and able to be inversed.  Below you will find 
## the function

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  setinvs <- function(inverse) invs <<- inverse
  getinvs <- function() invs
  list(set = set, get = get,
       setinvs = setinvs,
       getinvs = getinvs)
}


## This function will proceed to first calculate the inverse of the matrix from
## the previous function.  If the matrix is the same as before, it will produce
## the cached value.  If the matrix has changed, this function will calculate the 
## inverse, and cache it, ready to retrieve it again.  Each time, checking to see 
## if the matrix value is the same as before.  If so, the function will go to the 
## cache, saving time and computational resources.

cacheSolve <- function(x, ...) {
  invs <- x$getinvs()
  if(!is.null(invs)) {
    message("getting cached data")
    return(invs)
  }
  data <- x$get()
  invs <- solve(data, ...)
  x$setinvs(invs)
  invs
}
## Below is a test run demonstrating the successful use of this function
## > testm
##      [,1] [,2]
## [1,]  5.0 0.33
## [2,] -0.3 3.68
## > z <- testm
## > a <- makeCacheMatrix(z)
## > a
## $set
## function (y) 
## {
##   x <<- y
##   invs <<- NULL
## }
## <environment: 0x104700240>
##   
##   $get
## function () 
##   x
## <environment: 0x104700240>
##   
##   $setinvs
## function (inverse) 
##   invs <<- inverse
## <environment: 0x104700240>
##   
##   $getinvs
## function () 
##   invs
## <environment: 0x104700240>
##   
## > solve(testm)
##            [,1]       [,2]
## [1,] 0.19892967 -0.0178388
## [2,] 0.01621709  0.2702849
## > cacheSolve(a)
##            [,1]       [,2]
## [1,] 0.19892967 -0.0178388
## [2,] 0.01621709  0.2702849
## > solve(testm) == cacheSolve(a)
## getting cached data
##      [,1] [,2]
## [1,] TRUE TRUE
## [2,] TRUE TRUE
## > 
