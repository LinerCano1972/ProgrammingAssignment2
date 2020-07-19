# A continuación se han escrito dos funciones, las cuales pueden ser utilizadas
# para crear un "objeto especial" en este caso una "matriz especial" que almacena
# una matriz y almacena en la mempra caché su matriz inversa.

## La primera función, makeCacheMatrix crea una "matriz especial", 
#  que es realmente una lista que contiene una función para:
# 1. Determinar el valor de la matriz
# 2. Obtener el valor de la matriz
# 3. Determinar el valor de la inversa
# 4. Obtener el valor de la inversa

makeCacheMatrix <- function(w = matrix()) {
    k <- NULL
    set <- function(r) {
        w <<- r
        k <<- NULL
    }
    get <- function() w
    setinverse <- function(inverse) k <<- inverse
    getinverse <- function() k
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##La función "cacheSolve" calcula la matriz inversa de la "matriz" especial que ha retornado 
# previamente la función "makeCacheMatrix" escrita arriba.

cacheSolve <- function(w, ...) {
    k <- w$getinverse()
    if (!is.null(k)) {
        message("getting cached data")
        return(k)
    }
    data <- w$get()
    k <- solve(data, ...)
    w$setinverse(k)
    k
}

## Evaluación de las funciones.
# Sea "Matriz" invertible, entonces:

Matriz <- matrix(c(2,3,4,4,3,2,0,1,0),3,3,byrow=TRUE)
Matriz
#
Matriz2 <- makeCacheMatrix(Matriz)
Matriz2
cacheSolve(Matriz2) #Calcula y retorna la "matriz inversa" de "matriz"
#
cacheSolve(Matriz2) #Retornamos la inversa de la memoria caché
