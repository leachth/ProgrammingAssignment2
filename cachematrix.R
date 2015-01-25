## makeCacheMatrix is a function that create a "special" matrix. This matrix is 
  # a list that stores several fucntions that allow a user to (1) set the value of 
  # that matrix, (2) call the matrix, (3) set the inverse of the matrix, and 
  # (4) call the inverse of the matrix.
## cacheSolve caculates the inverse of special matrix constructed with 
  # makeCacheMatrix. cacheSolve checks to see if the inverse has already been 
  # calculated, if it has is pulls the inverse for the cache, if it has not the 
  # invese is caculated and stored in the special matrix. 
# B  must be a numerical, square matrix. 


makeCacheMatrix <- function(B = matrix()){
    iB <- NULL 
  set <- function(Y) {
              B <<- Y
              iB <<- NULL
        }
  get <- function () B
  setinv <- function(y) {
                iB <<- y
        } 
  getinv <- function () iB
  list( set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(B, ...){
  iB <- B$getinv()
  if(!is.null(iB)){
    message("getting cached data")
    return(iB)
  }
  data <- B$get()
  iB <- solve(data)
  B$setinv(iB)
  iB
}

## example
A = matrix( 
  c(2, 4, 1, 5), 
  nrow=2, 
  ncol=2) 
test = makeCacheMatrix(A)
cacheSolve(test)
# check that the inverse is cached, and cached data called. 
cacheSolve(test)