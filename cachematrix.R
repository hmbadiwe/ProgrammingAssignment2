## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   cache_inv <- NULL
   my_matrix <- x
   setMatrix <- function(matrix_arg){
     
     if( !( dim( matrix_arg ) == dim( my_matrix) && all( matrix_arg == my_matrix ) ) ){
       my_matrix <<- matrix_arg
       cache_inv <<- NULL
       print( "Matrix looks strangely unsimilar...")
     }
     
   }
  getMatrix <- function(){
    my_matrix
  }
  inverse <- function(){
    if( is.null( cache_inv ) ){
      cache_inv <- solve( my_matrix )
    }
    cache_inv
  }
  list( get=getMatrix, set=setMatrix, inverse=inverse)
}


## Write a short comment describing this function
cachedMatrix <- makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    cachedMatrix$set( x )
    cachedMatrix$inverse()
}
