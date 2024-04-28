## Essa função calcula a inversa da matriz (para as inversíveis)

## Armazenando os valores da matriz, e definindo as funções

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        setmatrix <- function(y) {
                x <<- y
                inv <<- NULL
        }      
        getmatrix <- function() x
        setinversa <- function(inversa) inv <<- inversa       
        getinversa <- function() inv
        
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinversa = setinversa,
             getinversa = getinversa)       
        
}


## Nessa função, realiza-se o cálculo da inversa

cacheSolve <- function(x, ...) {
        
        inv <- x$getinversa()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }       
        data <- x$getmatrix()
        inv <- inv(data, ...)
        x$setinversa(inv)
        inv
}