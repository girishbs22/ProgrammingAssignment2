## makeCacheMatrix creates a special object that helps

## makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setminv <- function(minv) mi <<- minv
        getminv <- function() mi
        list(set = set, get = get,
             setminv = setminv,
             getminv = getminv)
}


## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        mi <- x$getminv()
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        data <- x$get()
        mi <- solve(data, ...)
        x$setminv(mi)
        mi
}


## example1 for verification
mdat<-makeCacheMatrix(matrix(1:4,2,2))
cacheSolve(mdat)
cacheSolve(mdat)

## example2 for verification
r1<-c(0,0,6,0,1)
r2<-c(4,2,7,5,6)
r3<-c(3,0,5,1,2)
r4<-c(4,2,7,0,4)
r5<-c(1,1,5,1,6)
mdat<-makeCacheMatrix(rbind(r1,r2,r3,r4,r5))
cacheSolve(mdat)
cacheSolve(mdat)
