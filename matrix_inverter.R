#Let's extend the matrix triology and make a matrix Inverter!
#This program basically creates a matrix_inverse_cache -> inverts the given matrix and caches it to reduce computational time.

#So first I create the function that caches the inverse of a matrice
#The output of this function is a list with 4 elements-getter,setter,getInverse and setInverse
makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  #The initial value is thus set to null
  set <- function(y) {
    x <<- y
    inv <<- NULL
    #Very importantly here <<- is used instead of <- so that when we use 'set' to make a new matrice, 'inv' is set to the 
    #value of null.
  }
  get <- function() x
  #The get function here basically returns the subject matrice
  setinverse <- function(inverse) inv <<- inverse
  #setInverse is the function which creates the cache inverse
  getinverse <- function() inv
  #getInverse returns the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#This creates a list with the format 'name'='value'

#Now we switch to the the cacheSolve function, which first checks to see if there is a cache already for the inverse of the
# given matrix, in which case it returns the same. Otherwise it manually calculates it. Thus we see how computational time is
# reduced.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # Thus the conditional above acts as the check as mentioned above
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
# You are awesome! Thanks for reading the return of the Matrix!! The Matrix Inverter!!
