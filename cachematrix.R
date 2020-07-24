## Creating a special matrix that can cache it's inverse

## Create an R object that can store a matix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  if(ncol(x)==nrow(x) && det(x)!=0){
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse <- function() m <<- solve(x)
  getinverse<-function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  }
  else{
    print("Matrix is not invertible")
  }
  }


## This function retrieves the inverse of the matrix stored in makeCacheMatrix's environment

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  m
}

# An example
x <-makeCacheMatrix(matrix(c(1,2,5,4,1,0,4,4,0),ncol=3,nrow=3))
cacheSolve(x)

