# makeCacheMatrix
# creates a special matrix
# Saves the matrix to variable x
# Saves inverse of matrix to variable m

makeCacheMatrix <- function(x = matrix()) {
  
  # sets the values of m and y to NULL for default usage
    m <- NULL
    y <- NULL
    
    # function set the matrix
    set <- function(y) { 
      x <<- y #caches the inputted matrix so that cacheSolve can check
      #whether it has changed or is the same
      m <<- NULL
    }
    get <- function() x # returns matrix
    setmatrix <- function(solve) m <<- solve #saves solve value (calculate the inverse)
    getmatrix <- function() m #returns cached inverse value
    list(set = set, get = get,  # creates a list for these functions
         setmatrix = setmatrix,
         getmatrix = getmatrix)
  }


#Function to get the inverse matrix from special matrix
# x is the input argument, checks if the inverse value is already cached
#if it is chached, it returnes the previous value and does not calculate again
# if not, this function calculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m<-x$getmatrix()
  #comparing the matrix to see if it is a new one or the old one
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  #returns cached value with a message to announce that it is calculated before
  }
  #if it is not the same
  matrix<-x$get() #get the value of input matrix
  m<-solve(matrix, ...) #calculate the inverse
  x$setmatrix(m) #set the inverse to cache
  m #returns the inverse value
}
