#' Create a custom matrix with inverse matrix caching capabilities
#' 
#' \code{customMatrix} returns a custom matrix object with caching 
#' capabilities
#'
#' @param x a matrix
#' @keywords Coursera, R Programming, ProgrammingAssignment2
#' @examples
#' makeCacheMatrix()
#' myMatrix <- matrix(rnorm(1e6), 5,5)
#' myCustomMatrix <- makeCacheMatrix(myMatrix)
#' myCustomMatrix$getinverse()
#' 
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  
  set <- function(y) {
    #checking if they are the same matrix to set to NULL the inverseMatrix
    if (!identical(x,y)) {
      customMatrix$inverseMatrix <<- NULL
    }
    x <<- y
  }
  get <- function() x
  getinverse <- function() {
    customMatrix <<- cacheSolve (customMatrix)
    customMatrix$inverseMatrix
  }
  
  customMatrix <- list(inverseMatrix = inverseMatrix, 
                       set = set, get = get,
                       getinverse = getinverse)
}


#' Check if there is an inverse matrix cached. If so, it calculates the
#' inverse matrix.
#' 
#' \code{customMatrix} returns a custom matrix with the inverse matrix cached
#'
#' @param customMatrix the customMatrix object generated in makeCacheMatrix
#' @keywords Coursera, R Programming, ProgrammingAssignment2
#' @examples
#' cacheSolve(customMatrix)
#' 
cacheSolve <- function(customMatrix) {
  if (is.null(customMatrix$inverseMatrix)) {
    print("Calculating inverse matrix...")
    customMatrix$inverseMatrix <- solve (customMatrix$get())
  }
  customMatrix
}



library(digest)
library(matrixcalc)

#' Reference Class emulating the OOP model in R for this assignment. 
#' \strong{Just for learning purposes}
#' Moreover, instead of using \code{identical} function for checking if the
#' matrix changed or not I use the \code{digest} library, using a hashing 
#' function (md5) to cache the inverse matrix value to know it. Also, the
#' library \code{matrixcalc} is used to check if the matrix passed as an
#' argument is singular or not; in that case the execution will stop or not
#' (because you cannot invert a singular matrix)
#' 
#' @param data the matrix with inverse matrix caching capabilities
#' @keywords Coursera, R Programming, ProgrammingAssignment2
#' @examples
#' myMatrix <- matrix(rnorm(1e6), 5,5)
#' myObj <- MyCustomMatrix$new(data = myMatrix)
#' myObj$getinverse()
MyCustomMatrix <- setRefClass (Class = "MyCustomMatrix",
                             fields = list (
                               data = "matrix",
                               hash = "character",
                               inverse = "matrix"
                             ),
                             methods = list (
                               initialize = function (data) {
                                 #the matrix is singular. We stop the execution
                                 if (is.singular.matrix(data)) {
                                   stop ("The matrix is singular.")
                                 }
                                 #You can use stopifnot function as well but
                                 #without a custom message
                                 #stopifnot (is.non.singular.matrix(data))
                                 .self$data <- data
                                 .self$hash <- ""
                                 .self$inverse <- matrix()
                               },
                               setdata = function (data) {
                                 .self$data <- data
                               },
                               getinverse = function () {
                                 tmpHash <- digest (.self$data, "md5")
                                 if (.self$hash != tmpHash) {
                                   print("Calculating inverse matrix...")
                                   .self$inverse <- solve (.self$data)
                                   .self$hash <- tmpHash
                                 }
                                 .self$inverse
                               }
                             )
)
