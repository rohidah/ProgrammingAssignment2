## This function creates matrices containign a function to 
## 1. set the value of a matrix
## 2. get the value of a matirx
## 3. set the value of an inverse matrix
## 4. get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <-function(y)
  {
    x <<-y
    invm <<- NULL
  }
  get <-function() x
    
  setmatrixinv <- function(inverse) invm <<- inverse
  getmatrixinv <- function () invm
  list(set=set,get=get,setmatrixinv = setmatrixinv, getmatrixinv = getmatrixinv)
  
}


## This function calculates the inverse matrix of the matrix created with 
## makeCachematrix above. It first checks if the inverse has been calculated. 
## If so, it will grab the values from the cache. Otherwise, it will calculate
## the inverse of matrix and set the value in the cache via setmatrixinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getmatrixinv()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setmatrixinv(inv)
  inv
}
