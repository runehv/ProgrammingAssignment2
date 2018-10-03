#Base function - Creating the cache matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- Null
        set <- function(y){
                x <<- y
                inv <<- Null
        }
        get <- function() {
                print(x)
        }
        setinv <- function(input){
                inv <<- input
        }
        getinv <- function() {
                print(inv)
        }
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Creating cache solve and returning the inverse of x

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                message('getting cached data')
                return(inv)
                
                data <- x$get()
                inv <- solve(data,...)
                x$setinv(inv)
        } else {
                return(inv())
        }                
        print(inv)
}