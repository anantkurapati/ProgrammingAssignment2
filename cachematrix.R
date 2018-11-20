## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## matrix and inverse matrix container function
# will return list of function which will do get set operation on matrix
makeCacheMatrix <- function(x = matrix()) {
		inverseOfMatrix <- NULL # initialize inverseOfMatrix to null
        set <- function(tempMatrix) {
                x <<- tempMatrix
                inverseOfMatrix <<- NULL #everytime new matrix is assigned initialize inverseOfMatrix to null
        }
        get <- function() x # return existing matrix assigned
        setInverseOfMatrix <- function(inverse) inverseOfMatrix <<- inverse #store inverseOfMatrix to cache
        getInverseOfMatrix <- function() inverseOfMatrix #get inverseOfMatrix stored
        list(set = set, get = get,
             setInverseOfMatrix = setInverseOfMatrix,
             getInverseOfMatrix = getInverseOfMatrix)
}


## Write a short comment describing this function
#calculate 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		 inverse <- x$getInverseOfMatrix() #get inverseOfMatrix stored
        if(!is.null(inverse)) { #if inverse is not null then getInverseOfMatrix from cache store
                message("getting cached inverseOfMatrix")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data) #make inverse of matrix
        x$setInverseOfMatrix(inverse) #store inverseOfMatrix to cache
        inverse
		
}
