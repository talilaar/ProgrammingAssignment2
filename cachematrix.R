makeCacheMatrix<- function(X = numeric()) {
  inv_matrix <- NULL
  set <- function(Y) {
    X <<- Y
    inv_matrix <<- NULL
  }
  get <- function() X
  set_inv <- function(inv) inv_matrix <<- inv
  get_inv <- function() inv_matrix
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}



cacheSolve<- function(X, ...) {
  inv_matrix  <- X$get_inv()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix )
  }
  data <- X$get()
  inv_matrix  <- solve(data, ...)
  X$set_inv(inv_matrix )
  inv_matrix 
}

