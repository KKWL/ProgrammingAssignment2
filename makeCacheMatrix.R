makeCacheMatrix <- function(x = matrix()) {
  cache<-NULL
    set <- function( matrix ) {
          x  <<- matrix
            cache<<- NULL
    }


get<-function() x
setmx<-function(inverse)cache<<- inverse
getmx<-function() cache
list(set=set, get=get,
   setmx=setmx,
   getmx=getmx)
}

cacheSolve <- function(x=matrix(), ...) {
    cache<-x$getmx()
    if(!is.null(cache)){
      message("getting cached data")
      return(cache)
    }

    cache<-solve(x$get())
    x$setmx(cache)
    }