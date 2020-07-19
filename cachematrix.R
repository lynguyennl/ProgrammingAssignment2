## create the special vector/ list that contains function
makeCacheMatrix <- function(x = matrix()) 
{
        ## initiate inverse 
        s <- NULL
        
        ## set matrix 
        set <- function(y) 
        { 
                x <<- y 
                s <<- NULL
        }
        
        ## get matrix & return matrix 
        get <- function() {x} 
        
        ## set matrix inverse 
        setInverse <- function(solve) {s <<- solve}
        
        ## get matrix inverse 
        getInverse <- function() {s}
        
        ## return list of these methods
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        s <- x$getInverse()
        
        ## if the inverse matrix already existed, print message and return inverse matrix
        if(!is.null(s)) 
        {
                message("get cached data")
                return(s)
        }
        
        ## if the inverse matrix doesn't exist, get matrix from special vector/ list
        data <- x$get()
        
        ## generate inverse matrix 
        s <- solve(data, ...)
        
        ## set inverse matrix to special vector/ list
        x$setInverse(s)
        
        ## return inverse matrix
        s
}

## test my code 
## create special vector/ list containing my matrix
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))

## get my matrix from the list 
my_matrix$get()

## get inverse matrix 
my_matrix$getInverse()

## inverse matrix doesn't exist, calculate and cache inverse matrix 
cacheSolve(my_matrix)

## inverse matrix exists, show that it is cached. 
cacheSolve(my_matrix)