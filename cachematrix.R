## The script define two funtions in order to calculate the inverse of a matrix just once.
## In the next time, the inverse will be taken from the cache



## This function returns a list which contians 4 functions
## The input argument must be a matrix x

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL #Initializing the matrix inverse
        
        #Funtion set: It can be used to set a new matrix "y" in substitution of "x" (and be renamed as "x")
        set <- function(y){
                x <<- y
                inv <- NULL
        }
        
        #Function get: It returns the input argument x
        get <- function() x
        
        #Funtion setInverse: It will store the matrix inverse that we want to keep in cache. NULL by defualt
        setInverse <- function(inverse) inv <<- inverse
        
        #Funtion getInverse: It calls for the matrix invers that is store. NULL by default
        getInverse <- function() inv
        
        #In this part stablish the list that will be the output of this function
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function will return the inverse of the matrix stored in the list "x".
## The input is the list created with the previous function makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## In this part the variable inv will be defined with the inverse stored in the list "x"
        inv <- x$getInverse() #x$getInverse() is null the first time.
        
        
        ## In this part, the function will evaluate if the inv is a not a NULL value.
        #If the inv is not NULL it will return the inverse matrix stored.
        if(!is.null(inv) ) {#&& identical(x,x$getInverse)==TRUE){
                message("getting cached data")
                return(inv) #The inverse stored is set as the output and the function ends.
        }
        #Otherwise, (if inv is a NULL value) the inverse of the matrix will be calculated
        data <- x$get() #Here the matrix is gonna be taken from the list x
        inv <- solve(data) #Here the inverse is gonna be calculated
        x$setInverse(inv) #In this step the inverse is gonna be stored in the list object
        inv #And finally the inverse is set as the output
}
