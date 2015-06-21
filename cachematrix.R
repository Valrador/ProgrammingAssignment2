## Functions prevent the calculation of inverted matrix for the given one 
## if it was previousely calculated (if it is needed several times, while 
## input matrix can change over time for example) 
## use: let Y be the given nonsingular matrix, then f<-makeCacheMatrix(Y) creates
## list of function calls, containing: 1) function to store original matrix 
## 2) function to call original matrix from memory 3) store inverted (for given Y) matrix
## 4) call previousely stored matrix. so f should be mentioned as a list of closures(?)
## 

## Creates list of functions over the given matrix input (non singular)

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      SetMatrix<-function(y){
            x<<-y 
            m<<-NULL
      }
      GetMatrix<-function() x
      SetInv<-function(solve) m<<-solve
      GetInv<-function() m
      list(SetMatrix=SetMatrix, GetMatrix=GetMatrix, SetInv=SetInv, GetInv=GetInv)

}


## calls the inverded matrix for input matrix x, but takes the list f<-makeCacheMatrix(x)
## as an argument
cacheSolve <- function(x, ...) {
      m <- x$GetInv()
      if(!is.null(m)) {
            message("getting from cache")
            return(m)
      }
      data <- x$GetMatrix()
      m <- solve(data, ...)
      x$SetInv(m)
      m
}

