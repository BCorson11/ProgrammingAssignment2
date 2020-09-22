## The functions together calculate the inverse of a matrix and cache the 
## inverse so that it does not have to be calculated every time that it is 
## called.  


## The makeCacheMatrix (MCM) is a function that contains four other functions.  
## The other functions are set, get, setinverse, and getinverse.  The MCM
## function takes a matrix, x, as an argument.  It also initializes an
## object 'i' to be used later (i for inverse).  The set function takes y as
## its argument and sets x to be equal to that input in the parent environment.  
## It also re-initializes 'i' in the parent environment.  The get function just 
## retrieves x.  The setinverse function sets the value of the inverse to the 
## object "i" in the parent environment.  The getinverse retrieves the inverse.
## The MCM function then creates a list of set, get, setinverse, getinverse).


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve (CS) function takes the matrix x as its argument.  It assigns
## the value of the inverse of x to the object "i."  It then checks whether i
## is null.  If it is not null it will print a message and then the value of the
## inverse.  If i is null the function does sets the matrix x to an object
## called "data" and it then calculates the inverse of that matrix and sets it
## equal to i.  

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("Getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}


## Documentation: I read through the answer Len Greski posted to a 
## stackoverflow thread on 08DEC2017.  The thread can be found here:
## https://stackoverflow.com/questions/24904683/caching-the-mean-of-a-vector-in
## -r#:~:text=The%20first%20function%20in%20the,the%20makeVector()%20object's%
## 20environment.  I read Mr. Greski's answer on 22SEPT2020 and it helped me 
## understand what was going on in the makeVector and cacheMean functions from
## the prompt.  I did not see a requirement to include documentation but I 
## felt it was appropriate.  Also, if this is insufficient please contact me at
## corson.38@osu.edu.