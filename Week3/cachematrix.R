#Question
#Matrix inversion is usually a costly computation and there may be some benefit to caching the
# inverse of a matrix than compute it repeatedly. The following two functions are used to cache the
# inverse of a matrix

#makeCacheMatrix creates a list containing a function to
#1.set the value of a matrix
#2. get the value of the matrix
#3. set the value of inverse of the matrix
#4. get the value of inverse of the matrix

install.packages("matlib")
library(matlib)     # for inv() to find inverse


#Here the  solve and inv function are used in the two different functions to find the inverse of a  square matrix . 
#Inverse is defined only for square matrices.
#Function makeCacheMatrix is used to set & get the value of a matrix, 
#set and get the value of inverse of the matrix.
makeCacheMatrix <-function(x=matrix()){
  inv1<-NULL
  set<-function(y){
    x<<-y   # sets the value of matrix
    inv1<<-NULL  # inverse is initialised to NULL
  }
  get<-function() x  # get the value of matrix
  setinverse <-function(inverse) inv1<<-inverse  # set the inverse
  getinverse <-function() inv1   # get the inverse
  list(set = set, get= get, setinverse= setinverse, getinverse=getinverse)}


# The  function cacheSolver eturns the inverse of the matrix.
#It first checks if the inverse is already  computed . If so, it gets the
#result and skips the computation. If not, it computes the inverse, sets the value 
#in the cache using the setinversefunction.
#This function assumes that the matrix is always inverstible. 
#While setting the matrix, we need to give a squrae matrix as input.

cacheSolve <-function(x,...){
  inv1<- x$getinverse()
  if(!is.null(inv1)){
    message("getting cached data related to inverse of the matrix")
    return(inv1)
  }
  data<-x$get()
  inv1<-solve(data)  # Calculating Inverse using solve function
  x$setinverse(inv1)
  inv1
}


cacheSolve1 <-function(x,...){
  inv1<- x$getinverse()
  if(!is.null(inv1)){
    message("getting cached data related to inverse of the matrix")
    return(inv)
  }
  data<-x$get()
  inv1<-inv(data)    # Calculating Inverse using inv function
  x$setinverse(inv1)
  inv1
}

#Sample Solution 1- Using solve() for calculating inverse
x<-matrix(c(5, 1, 0,
            3,-1, 2,
            4, 0,-1), nrow=3, byrow=TRUE)
x

m<-makeCacheMatrix(x)
m
m$get()

# Retrieving the inverse of the matrix- First run - No Cache
cacheSolve(m)

# Retrieving the inverse of the matrix- Second run - Retriving inverse from Cached data
cacheSolve(m)

#Sample Solution 2- Using inv() for calculating inverse
x1<-matrix(c(5, 1, 0,
             3,-1, 2,
             4, 0,-1), nrow=3, byrow=TRUE)
x1
m1<-makeCacheMatrix(x1)
m1$get()

# Retrieving the inverse of the matrix- First run - No Cache
cacheSolve1(m1)

# Retrieving the inverse of the matrix- Second run - Retriving inverse from Cached data
cacheSolve1(m1)

####Soluion Runs
# makeCacheMatrix <-function(x=matrix()){
#   +    inv<-NULL
#   +    set<-function(y){
#     +      x<<-y   # sets the value of matrix
#     +      inv<<-NULL  # inverse is initialised to NULL
#     +    }
#   +    get<-function() x  # get the value of matrix
#   +    setinverse <-function(inverse) inv<<-inverse  # set the inverse
#   +    getinverse <-function() inv   # get the inverse
#   +    list(set = set, get= get, setinverse= setinverse, getinverse=getinverse)}
# > x<-matrix(c(5, 1, 0,
#               +                3,-1, 2,
#               +                4, 0,-1), nrow=3, byrow=TRUE)
# > x<-matrix(c(5, 1, 0,
#               +                3,-1, 2,
#               +                4, 0,-1), nrow=3, byrow=TRUE)
# >  x
# [,1] [,2] [,3]
# [1,]    5    1    0
# [2,]    3   -1    2
# [3,]    4    0   -1
# > m<-makeCacheMatrix(x)
# > m
# $set
# function(y){
#   x<<-y   # sets the value of matrix
#   inv<<-NULL  # inverse is initialised to NULL
# }
# <environment: 0x7f9a4979eab0>
#   
#   $get
# function() x
# <environment: 0x7f9a4979eab0>
#   
#   $setinverse
# function(inverse) inv<<-inverse
# <environment: 0x7f9a4979eab0>
#   
#   $getinverse
# function() inv
# <environment: 0x7f9a4979eab0>
#   
#   > m$get()
# [,1] [,2] [,3]
# [1,]    5    1    0
# [2,]    3   -1    2
# [3,]    4    0   -1
# > cacheSolve <-function(x,...){
#   +    inv<- x$getinverse()
#   +    if(!is.null(inv)){
#     +      message("getting cached data related to inverse of the matrix")
#     +      return(inv)
#     +    }
#   +      data<-x$get()
#   +      inv<-solve(data)
#   +      x$setinverse(inv)
#   +      inv
#   +  }
# > # Retrieving the inverse of the matrix- First run - No Cache
#   > cacheSolve(m)
# [,1]    [,2]   [,3]
# [1,] 0.0625  0.0625  0.125
# [2,] 0.6875 -0.3125 -0.625
# [3,] 0.2500  0.2500 -0.500
# > # Retrieving the inverse of the matrix- Second run - Retriving inverse from Cached data
#   > cacheSolve(m)
# getting cached data related to inverse of the matrix
# [,1]    [,2]   [,3]
# [1,] 0.0625  0.0625  0.125
# [2,] 0.6875 -0.3125 -0.625
# [3,] 0.2500  0.2500 -0.500
# > makeCacheMatrix <-function(x=matrix()){
#   +    inv1<-NULL
#   +    set<-function(y){
#     +      x<<-y   # sets the value of matrix
#     +      inv1<<-NULL  # inverse is initialised to NULL
#     +    }
#   +    get<-function() x  # get the value of matrix
#   +    setinverse <-function(inverse) inv1<<-inverse  # set the inverse
#   +    getinverse <-function() inv1   # get the inverse
#   +    list(set = set, get= get, setinverse= setinverse, getinverse=getinverse)}
# > cacheSolve <-function(x,...){
#   +    inv1<- x$getinverse()
#   +    if(!is.null(inv1)){
#     +      message("getting cached data related to inverse of the matrix")
#     +      return(inv1)
#     +    }
#   +      data<-x$get()
#   +      inv1<-solve(data)
#   +      x$setinverse(inv1)
#   +      inv1
#   +  }
# > x<-matrix(c(5, 1, 0,
#               +                3,-1, 2,
#               +                4, 0,-1), nrow=3, byrow=TRUE)
# >  x
# [,1] [,2] [,3]
# [1,]    5    1    0
# [2,]    3   -1    2
# [3,]    4    0   -1
# > m<-makeCacheMatrix(x)
# > m
# $set
# function(y){
#   x<<-y   # sets the value of matrix
#   inv1<<-NULL  # inverse is initialised to NULL
# }
# <environment: 0x7f9a4fb63698>
#   
#   $get
# function() x
# <environment: 0x7f9a4fb63698>
#   
#   $setinverse
# function(inverse) inv1<<-inverse
# <environment: 0x7f9a4fb63698>
#   
#   $getinverse
# function() inv1
# <environment: 0x7f9a4fb63698>
#   
#   > m$get()
# [,1] [,2] [,3]
# [1,]    5    1    0
# [2,]    3   -1    2
# [3,]    4    0   -1
# > # Retrieving the inverse of the matrix- First run - No Cache
#   > cacheSolve(m)
# [,1]    [,2]   [,3]
# [1,] 0.0625  0.0625  0.125
# [2,] 0.6875 -0.3125 -0.625
# [3,] 0.2500  0.2500 -0.500
# > cacheSolve <-function(x,...){
#   +    inv1<- x$getinverse()
#   +    if(!is.null(inv1)){
#     +      message("getting cached data related to inverse of the matrix")
#     +      return(inv)
#     +    }
#   +    data<-x$get()
#   +    inv1<-inv(data)
#   +    x$setinverse(inv1)
#   +    inv1
#   +  }
# > cacheSolve1 <-function(x,...){
#   +    inv1<- x$getinverse()
#   +    if(!is.null(inv1)){
#     +      message("getting cached data related to inverse of the matrix")
#     +      return(inv)
#     +    }
#   +    data<-x$get()
#   +    inv1<-inv(data)
#   +    x$setinverse(inv1)
#   +    inv1
#   +  }
# > x1<-matrix(c(5, 1, 0,
#                +             3,-1, 2,
#                +             4, 0,-1), nrow=3, byrow=TRUE)
# > x1
# [,1] [,2] [,3]
# [1,]    5    1    0
# [2,]    3   -1    2
# [3,]    4    0   -1
# > m1<-makeCacheMatrix(x1)
# > m1$get()
# [,1] [,2] [,3]
# [1,]    5    1    0
# [2,]    3   -1    2
# [3,]    4    0   -1
# > # Retrieving the inverse of the matrix- First run - No Cache
#   > cacheSolve1(m1)
# [,1]    [,2]   [,3]
# [1,] 0.0625  0.0625  0.125
# [2,] 0.6875 -0.3125 -0.625
# [3,] 0.2500  0.2500 -0.500
# > # Retrieving the inverse of the matrix- Second run - Retriving inverse from Cached data
#   > cacheSolve1(m1)
# getting cached data related to inverse of the matrix
# NULL
