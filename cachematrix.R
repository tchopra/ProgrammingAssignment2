## Purpose: Purpose of makeCacheMatrix function is to take matrix as an object and cache that matrix
## for future recall. makeCacheMatrix contains full helper function, desecription is given below.

## set: takes an object and set the value of the passed object to the parent object.
## get: returns the object stored in variable x.
## setSolve: cache the inverse of the object (matrix in this case) in m, for future recall.
## getSolve: return the cache object (in this case inverser of the matrix).

## makeCacheMatrix takes an object, in this case a matrix and store it in a cache. If the function
## is instantiated with out an object passed, it will initialize the cache to null. makeCacheMatrix
## four sub functions : set, get, setSolve, getSolve as described above. makeCacheMatrix is also
## <<- operator which can be used to assign a value to an object in an environment that is different
## from the current environment

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ## initiate the matrix m to NULL.
        
        set <- function(y) {
                x <<- y  ## assigned the pased object (matrix in this case) to the parent variable x.
                m <<- NULL ## initiate the cached matrix object to null.
        }
        
        get <- function() x ## get() function will retun the parent object x
        
        setSolve <- function(Solve) m <<- Solve ## setSolve() function will store the inverse of matrix in cache (m).
        
        getSolve <- function() m ## getSolve() will returns the matrix stored in cache (m).
        
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve) ## listing of all the subfunctions in the makeCacheMatrix() parent function

}


## Purpose: Purpose of cacheSolve function is to take matrix x as an argument, first check to see if
## the passed matrix inversed is already cached. If the matrix is already cached, then return it back
## to user. If the inverse of matrix is not stored in cache, then perform the inverse of the matrix,
## store it in cache (for further reference) and return the inverse back to the user.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve() ## first check to see if the inverse of matrix is already cached.
        
        if(!is.null(m)) { ## if the inverese of matrix is cached, then return this matrix back to user
                message("getting cached data")
                return(m)
        }
        
        data <- x$get() ## get the matrix and store it local data variable.
        
        m <- solve(data,...) ## perform the inverse of matrix. Using the solve() in R in order to compute the inverse of matrix.
        
        x$setSolve(m) ## store the inverse of passed matrix in the cache
        
        m ## retun the inverse of matrix.
}
