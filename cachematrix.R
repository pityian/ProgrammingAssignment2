## These functions are used together. We create a special "matrix object
## that can cache its inverse with makeCacheMatrix() and we find its inverse with cacheSolve().

makeCacheMatrix <- function(x = matrix()) {     #the input will be a matrix
        m <- NULL               # m will be an empty object an it's reset
                                # to NULL every time makeCacheMatrix is called
        
        set <- function(y) {    # When a takes a new object is created, takes the input matrix
                x <<-y          # saves the input matrix
                m <<- NULL      # resets inverse to NULL.
        }
        
        get <- function()       # This function returns the original matrix
        
        setInverse <- function(inverse) m <<- inverse   # This is called by cacheSolve() during the first cacheSolve()
                                                        # access and it wil store the value usign superassignment        
        
        getInverse <- function() m              #this wil return the inverse value to cacheSolve() on subsequent accesses

        list(set = set, get = get,              #This is accesed each time makeCacheMatrix() is called and is a list
             setInverse = setInverse,           #of the internal functions ('methods') so a calling function knows how to access those methods.
             getInverse = getInverse)
}

##This function computes the inverse of the special "matrix" returned by
##makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {       #the input x is an object created by makeCacheMatrix()
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()             # assign the inverse of the matrix to m
        
        if(!is.null(m)) {                       # if m is already cached an not NULL 
                message("getting cached data")  # print out the message
                return(m)                       # and return the inverse
        }
        matrix <- x$get()               #if x$getInverse returned NULL
        m <- solve(matrix, ...)         #then we have to find the inverse using the solve() function
        x$setInverse(m)                 #and store the calculated inverse
        
        m                               #return the inverse matrix
}