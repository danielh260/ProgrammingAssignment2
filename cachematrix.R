## We will be creating two functions.Fist function will take a matrix as input and generate a list of 4 elements 
## $set,$get,$setinverse,$getinverse that will act as an input variable matrix for the second function that will calculate 
## the inverse of a matrix using the solve function.

##Creates a list to be used later in the CacheSolve Function.
makeCacheMatrix <-function(matrix_to_cache=matrix())#Input variable is a matrix
                            {
                              inverseMatrix=NULL #Instantiate the empty Inverse
                                    set<-function(l)    
                                          {
                                          matrix_to_cache<<-l
                                          inverseMatrix<<-NULL
                                          }
                                     get<-function(){
                                                     matrix_to_cache
                                                    }
                              setinverse<-function(solve) 
                                                        {
                                                         inverseMatrix <<- (solve)
                                                        }
                              getinverse<-function() 
                                                      {
                                                       inverseMatrix
                                                      }
                              return(list(set = set, get = get,setinverse = setinverse,getinverse = getinverse))
  
                            }
##From makeCacheMatrix we have a list with elements $set,$get,$setinverse,$getinverse that needs to act as an input variable matrix.
cacheSolve<-function(matrix_To_invert=matrix(), ...) {
                                            inverseMatrix<-matrix_To_invert$getinverse()
                                            if (!is.null(inverseMatrix)){
                                                                          message("getting cached matrix data")
                                                                          return(inverseMatrix)
                                            }
                                            matrix_data<-matrix_To_invert$get()
                                            inverseMatrix<-solve(matrix_data, ...)
                                            matrix_To_invert$setinverse(inverseMatrix)
                                            return(inverseMatrix)
													}