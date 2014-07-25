### These pair of functions will create a cache, cache an inputted matrix, and calculate 
### and cache the inverse of that matrix.
### When the inverse of the matrix is requested, it can retrieve the inverted matrix 
### from the cache instead of computing it again. 


## This first function creates a cache; it creates a list containing an original matrix 
## and an inverse of that matrix, which can be filled with the inverse matrix using 
## the second function. 

## The original matrix can be inputted into a variable by calling 'makeCacheMatrix' 
## and putting it in as an argument, and updated by using '[variable_name]$set', 
## with the new matrix as an argument.

makeCacheMatrix <- function(x = matrix()) {

	#Sets variable 's' (the initial values of the inverse matrix) to NULL
    s <- NULL					
	  
	# This function sets values of 'x' and 's' to the variables outside of the function. 
	# It looks for the variables in a parent context to overwrite, instead of creating 
	# a new variable within the scope of the function. If it does not exist yet in the 
	# parent environments, it creates the variable in the global environment.
	# Basically, this function resets the variables.	  
    set <- function(y) {
			# Puts input 'y' in the variable called 'x' outside of the function, 
			# basically refilling 'x' with the newly inputted data (a matrix)
            x <<- y	
			# Sets the variable called 's' to NULL outside of the function, 
			# basically resetting 's'
            s <<- NULL			
    }

	# This function gets (returns) the value of 'x'; the matrix 
    get <- function() x
	  
	# Puts the variable called 'inv_mtr' into the variable called 's' outside of the function	  
    setinverse <- function(inv_mtr) s <<- inv_mtr	

	# This function gets (returns) the value of 's'; the inversed matrix, 
	# or NULL if it is not calculated and inputted yet.
    getinverse <- function() s
	  
	# Creates a list of functions, able to be called from the command line
    list(													
		set = set, 
		get = get,							
        setinverse = setinverse,
        getinverse = getinverse
		)
}


## This function first checks whether there is a cached inverse matrix of 'x', 
## and returns it if it exists. 

## If the inverse matrix does not exist in the cache it calculates the inverse matrix, 
## returns it, and puts it in the cache.

cacheSolve <- function(x, ...) {

	# Calls to the object 'getinverse' from the list associated with 'x'. 
	# This is a function that might return the cached variable 's'; 
	# a previously calculated inverse matrix, or return NULL.
    s <- x$getinverse()										

	# If the variable 's' (the cached inverse matrix) is not NULL, 
	# this functions returns a message and then returns 's'.
    if(!is.null(s)) {											
          message("getting cached data")
          return(s)
    }
	  
	# Calls to the function 'get'; the element of the list associated with 'x'. 
	# This gets the values of 'x', the matrix, and puts it into the variable 'data'.
    data <- x$get()
	  
	# Calculates the inverse of the matrix 'x' that has been saved in the variable 'data'.
    s <- solve(data, ...)
	  
	# Calls to the function 'setinverse'; the element of the list associated with 'x'. 
	# This puts the calculated inverse matrix into variable 's'
    x$setinverse(s)
	  
	# Returns 's', the inverse matrix
    s
}

## Commands:

## [variable_name] = makeCacheMatrix(matrix(c([the_elements_in_the_matrix]), nrow=[number_of_rows], ncol=[number_of_columns])) --- creates the matrix inputted as an argument, and puts it in an element of a list (which functions as the cache). The list can also contain the calculated inverse of the matrix, but when this function is called the inverse will be set to NULL. 
## [variable_name]$get() --- Returns the matrix that was inputted, which has been placed in a list (the cache)
## cacheSolve([variable_name]) --- Checks if there's a cached inverse matrix in the list and returns it, and if there's nothin in the cache this function computes the inverse, caches it, and returns the inverse of the matrix
## [variable_name]$set(matrix(c([the_elements_in_the_matrix]), nrow=[number_of_rows], ncol=[number_of_columns])) --- Overwrites the existing matrix, and resets the cached inversed matrix to NULL
## [variable_name]$getinverse() --- Returns matrix inverse from the cache, or NULL if there isn't a cached inverse matrix

## DO NOT USE [variable_name]$setinverse(matrix(c([the_elements_in_the_matrix]), nrow=[number_of_rows], ncol=[number_of_columns])) 
## Calling '$setinverse' directly will overwrite the calculated & cashed inverse matrix, therefore the matrices in the list/cache will not match, making the output of cacheSolve incorrect. 
