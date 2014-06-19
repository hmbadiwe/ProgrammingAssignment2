## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix( x = matrix())
## Arguments
##
## x - an invertible matrix. Defaults to an empty matrix
##
## This function creates an object that caches function calls to solve a matrix.
## It returns a list with a set, get and solve function
## The set function, when called with a matrix, will check to see if the matrix argument is equal to it's cached matrix
## If it is not, the cached verson of solve is cleared, prompting a recalculation/re-caching of the solve value the next time solve is invoked

makeCacheMatrix <- function(x = matrix()) {
   cache_inv <- NULL
   my_matrix <- x
   setMatrix <- function(matrix_arg){
     
     if( !( dim( matrix_arg ) == dim( my_matrix) && all( matrix_arg == my_matrix ) ) ){
       my_matrix <<- matrix_arg
       cache_inv <<- NULL
     }
     
   }
  getMatrix <- function(){
    my_matrix
  }
  my_solve <- function(){
    if( is.null( cache_inv ) ){
      cache_inv <- solve( my_matrix )
    }
    cache_inv
  }
  list( get=getMatrix, set=setMatrix, solve=my_solve)
}

dont_touch_my.cachedMatrix <- makeCacheMatrix()

## Write a short comment describing this function

## cacheSolve( x )
##
## Arguments
##
## x - an invertible matrix
## This function provides a memoized inverse function
## if cacheSolve is called with the same matrix more than once consecutively, the actual solve function is called only once.
## the solve function is called if a different matrix is passed to cacheSolve


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  dont_touch_my.cachedMatrix$set( x )
  dont_touch_my.cachedMatrix$solve()
}
