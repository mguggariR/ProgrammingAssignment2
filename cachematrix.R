## As inversing a matrix takes valuable computer time, repeated calculations of same matrix is costly. 
## These two functions, in combination, help retain and use already calculated inverse of a matrix value. 
## This is done through the use of cache memory as well as R language's unique way of lexical scoping


## This function "akeCacheMatrix" creates a space for storing inverse of a metrix,and returns a set of functions to manage the storage
## Note that by changing class of x, one can modify and use it for other objects of different class.

makeCacheMatrix <- function(x = matrix()) {
      
      myAnswer <- NULL                          ## When an object is instantiated, old value is removed 
      
      setNewData <- function(newData)           ## allows to insert new data in to cache while removing old result 
      {
            x <<- newData
            myAnswer <<- NULL
      }
      
      getNewData <- function()                  ## allows to access current matrix data in the original function object 
      {x}
      
      setMyAnswer <- function(myNewAnswer)      ## This function allows the calling function to store new answer  
      {myAnswer <<- myNewAnswer}
      
      getMyAnswer <- function()                 ## This function provides existing (cached) inverse of a Matrix to calling function 
      {myAnswer}
      
      list(setNewData = setNewData,
           getNewData = getNewData,
           setMyAnswer = setMyAnswer,
           getMyAnswer = getMyAnswer)           ## Internal functions are named to help access them easiy with $ subsetting 
      ## and returned as a list  
}


## Following function "cacheSolve" returns inverse of a matrix stored in a cache of the passed "makeCacheMatrix" object x as an argument 
## Using available functions from makeCacheMatrix it also manages the cache by providing a new inverse of a matrix when original matrix is changed. 

cacheSolve <- function(x, ...) {
      
      oldAnswer <- x$getMyAnswer()                    ## This gets existing cached data (inversed matrix) 
      
      if(!is.null(oldAnswer)) {                       ## If no change in matrix, indicated by !Null result, cached inverse matrix is returned 
            message("getting cached data")
            return(oldAnswer)
      }
      
      data <- x$getNewData()                          ## Otherwise, these three statements use internal functions of object x 
      newAnswer <- solve(data)                        ## to obtain new matrix data, calculate the inverse and store the result back in cache
      x$setMyAnswer(newAnswer)
      
      newAnswer                                       ## new calculated inverse of new matrix in x object is returned
      
}
