## makeCacheMatrix function creates a special type of matrix with cache support
## cacheSolve function calculate the inverse of a special matrix. If the result was calculated 
## before then it returns the cached result

## this function takes a matrix type object and create a special matrix with cache support

makeCacheMatrix <- function(x = matrix()) {
	Inverse <- NULL;
	## set the matrix data
	set <- function(M){
		x <<-M;
		Inverse <<- NULL;	
	}
	## get the matraix data
	get<-function(){
		x;
	}
	## cache the inverse
	setinverse <- function(I) Inverse <<- I;
	## return the inverse
	getinverse <- function() Inverse;
	## this function will return a list of member functions
     list(set = set,get = get,setinverse = setinverse,getinverse=getinverse);
}


## this function takes a special matix as parameter and calculate the inverse of that matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse();
	## checking for cached value
	if(!is.null(inverse)){
		##print("chache is working!!");
		return (inverse);
	}
	## if not cached calculate the inverse and save it
	mat <- x$get();
	inverse <- solve(mat);
	x$setinverse(mat);
	inverse;
}
