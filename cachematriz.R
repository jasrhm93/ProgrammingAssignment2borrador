# Assignment: Caching the Inverse of a Matrix| Encontrar la inversa de una matriz
# This function creates a special "matrix" object that can cache its inverse.
# Esta funcion crea una matriz a la cual se le puede calcular su inversa

# Creates a matrix that can cache it's inverse
# Crea una matriz y mantiene su inversa en su cache
#
# Args:
#   x: A matrix (Optional)
#
# Returns:
#   A matrix with functions to get/set value & get/set inverse

makeCacheMatrix <- function(x = matrix()){
  #Cached inverse of matrix
  #Inversa de una matriz en la memoria
  inv <- NULL
  #setter for matrix
  #set para la matriz
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  # getter for matrix
  # get para la matriz
  get <- function() {x}
  ## getter/setter for matrix inverse
  #setInverse y getInverse para la matriz inversa
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}





# Computes the inverse of a matrix. If the inverse has already been
# calculated before, the cached inverse is returned.
# La siguiente funcion calcula la matriz inversa. Si la inversa ya se calculo con
# la función anterior, retorna el cache de su inversa
# Args:
#   x: A matrix
#   ...: Extra arguments
#
# Returns:
#   The inverse of the matrix
# Devuelve la inversa de la matriz 
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  # return cached matrix inverse if it's been already computed
  # Devuelve la inversa de la matriz si ya se calculo
  if (!is.null(inv)) {
    message("inverse is cached")
    return(inv)
  }
  
  # compute inverse of matrix 
  # Calcula la inversa de la matriz
  m <- x$get()
  inv <- solve(m, ...)
  
  x$setinv(inv)
  
  # return inverse of matrix
  # Devuelve la inversa de la matriz
  return(inv)
}



