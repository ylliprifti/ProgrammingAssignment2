## Make Cache Matrix is the main class which extends the matrix class 
## It will save the original matrix in a private variable called dt accessible via get() function
## It will also save calculate it's inverse when first time requested and save it into a variable called inv
## The inverse can be accessed via the function getinv(). 
##

##Usage Example: 
##x = matrix(c(1,2,-2,1), 2,2)
##x <- makeCacheMatrix(x)
##cacheSolve(x)
##cacheSolve(x)

##Expected Output: 

##getting cached data
##     [,1] [,2]
##[1,]  0.2  0.4
##[2,] -0.4  0.2


## Cache solve will use the extended matrix. Whenever the cached version is used, a message will be printed to the user

##Matrix class extention
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ##chache the inverse
  dt <- x     ##save the original matrix
  
  ##set the variables
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ##return the original matrix 
  get <- function() dt
  
  ##set the inverse
  setinv <- function(solved) inv <<- solved
  
  ##return the saved inverse
  getinv <- function() inv
  
  ##expose the public properties
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Use the extended matrix to return the cached inverse
cacheSolve <- function(x, ...) {
  ##try read cached inverse
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ##if not cached calculate and save
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}

