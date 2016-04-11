## The two functions below take advantage of the scoping rules in R to preserve the state inside of an R ojbect. These functions together create a special matrix and return the inverse of the matrix either from the cache or via calculation. 

## The following function creates a special matrix, which is really
##just a list containing a function to
## 1) set the value of a matrix
## 2) get the value of a matrix
## 3) set the value of the inverse of the original matrix
## 4) get the value of the inverse of the original matrix


makeCacheMatrix <- function(x = matrix()){
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setsolve<-function(solve) m<<- solve
  getsolve<-function() m
  list(set=set, get=get,
       setsolve=setsolve,
       getsolve=getsolve)
}

## The following function calculates the inverse of the special matrix created with the above function. It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise it calculates the inverse from the data and sets the value of the inverse in the cache via the setsolve function. 

cacheSolve <- function(x=matrix(), ...) {
	 ## Return a matrix that is the inverse of 'x'
  m<-x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setsolve(m)
  m
}


