
## This first function creates a cache; it creates a list containing an original matrix and an inverse of that matrix, which can be filled with the inverse matrix using the second function. 
## The original matrix can be inputted into a variable by calling 'makeCacheMatrix' and putting it in as an argument, and updated by using '[variable_name]$set', with the new matrix as an argument.

makeCacheMatrix <- function(x = matrix()) {

      s <- NULL					#Sets variable 's' (the initial values of the inverse matrix) to NULL
	  
	  #This function sets values of 'x' and 's' to the variables outside of the function; it looks for the variable in a parent context to overwrite, instead of creating a new variable within the scope of the function. Basically, this function resets the variables.	  
      set <- function(y) {									
            x <<- y				#Puts input 'y' in the variable called 'x' outside of the function, basically refilling 'x' with the newly inputted data (a matrix)
            s <<- NULL			#Sets the variable called 's' to NULL outside of the function, basically resetting 's'
      }

	  #This function gets (returns) the value of 'x'; the matrix 
      get <- function() x
	  
	  #Puts the variable called 'inv_mtr' into the variable called 's' outside of the function	  
      setinverse <- function(inv_mtr) s <<- inv_mtr	

	  #This function gets (returns) the value of 's'; the inversed matrix, or NULL if it is not calculated and inputted yet.
      getinverse <- function() s
	  
	  #Creates a list of functions, able to be called from the command line
      list(													
		  set = set, 
		  get = get,							
          setinverse = setinverse,
          getinverse = getinverse
		  )
}


## This function first checks whether there is a cached inverse matrix of 'x', and returns it if it exists. 
## If the inverse matrix does not exist in the cache it calculates the inverse matrix, returns it, and puts it in the cache.

cacheSolve <- function(x, ...) {

	  #calls to the object 'getinverse' from the list associated with 'x'. this is a function that might return the cached variable 's'; an previously calculated inverse matrix
      s <- x$getinverse()										

	  # If the variable 's' (the cached inverse matrix) is not NULL, this functions returns a message and then returns 's'
      if(!is.null(s)) {											
            message("getting cached data")
            return(s)
      }
	  
	  # calls to the function 'get'; the element of the list associated with 'x'. This gets the values of 'x', the matrix.
      data <- x$get()
	  
	  #calculates the inverse of the matrix 'x' that has been saved in the variable 'data'.
      s <- solve(data, ...)
	  
	  # calls to the function 'setinverse'; the element of the list associated with 'x'. This puts the calculated inverse matrix into variable 's'
      x$setinverse(s)
	  
	  #returns 's', the inverse matrix
      s
}

## Commands:

## [variable_name] = makeCacheMatrix(matrix(c([the_elements_in_the_matrix]), nrow=[number_of_rows], ncol=[number_of_columns]))  # creates the matrix inputted as an argument, and puts it in an element of a list (which functions as the cache). the list can also contain the calculated inverse of the matrix, but when this function is called the inverse will be set to NULL. 
## [variable_name]$get() # Returns the matrix that was inputted, which has been placed in a list (the cache)
## cacheSolve([variable_name]) # Checks if there's a cached inverse matrix in the list and returns it, and if there's nothin in the cache this function computes the inverse, caches it, and returns the inverse of the matrix
## [variable_name]$set(matrix(c([the_elements_in_the_matrix]), nrow=[number_of_rows], ncol=[number_of_columns])) # Overwrites the existing matrix, and resets the cached inversed matrix to NULL
## [variable_name]$getinverse() # Returns matrix inverse from the cache, or NULL if there isn't a cached inverse matrix

## DO NOT USE [variable_name]$setmatrix(matrix(c([the_elements_in_the_matrix]), nrow=[number_of_rows], ncol=[number_of_columns])) 
## Calling '$setmatrix' directly will overwrite the calculated & cashed inverse matrix, therefore the matrices in the list/cache will not match, making the output of cacheSolve incorrect. 
