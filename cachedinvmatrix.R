makeCacheMatrix <- function(x = matrix()){
     kit <- NULL
     set <- function(y){
           x <<- y
           kit <<- NULL
     }
     get <- function() {x}
     setInverse <- function(inverse) {kit <<- inverse}
     getInverse <- function() {kit}
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
      kit <- x$getInverse()
      if(!is.null(kit)){
            message("getting cached data")
            return(kit)
      }
      mat <- x$get()
      kit <- solve(mat, ...)
      x$setInverse(kit)
      kit
}