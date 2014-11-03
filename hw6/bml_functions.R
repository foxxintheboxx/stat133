#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.initl<- function(r, c, p) { 
   m <- matrix(rbinom(r*c,c(1,2),p),nrow=r,ncol=c)
   m
}



#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

bml.step <- function(m){

  #advance north the
	new.m = apply(m, 2, function(c) { move.car(c, 1) })
	for (i in 1:nrow(new.m)) {
	 	new.m[i,] = move.car(new.m[i,], 2)
	}

	new.m
}
move.car <- function(vector, value) {
	  	indexes = indexes.to.advance(vector,value)
	  	print(value)
	  	sequence = 1:length(vector)
	  	if (value == 2)
	  	{
		 	sequence = rev(sequence) 	
	  	}
  		for (i in sequence){
  			if (i %in% indexes)
  			{

  				if (value == 1)
  				{
	  				new.i = i - 1
	  			} else {
	  				new.i = (i + 1) %% length(vector)
	  			}
  				if (new.i == 0) {
  					new.i = length(vector)
  				}
  				print(new.i)
  				vector[new.i] = vector[i]
  				vector[i] = 0
  			}
  		}
  		if (value == 2)
	  	{
	  		print("return")
	  		print(vector)
	  		
	  	}
	  	vector
}

#WORKING
indexes.to.advance <- function(vector, v)
{
	all.indexes = NULL
	sublist = NULL
	sequence = 1:length(vector)
	if (v == 1) {
		sequence = rev(sequence)
	}
	for (curr in sequence) {
		prev = NULL
		if (v == 1)
		{
			prev <- (curr - 1)%% (length(vector))
			if (prev == 0) {
				prev = length(vector)
			}
		} else {
			prev <- (curr + 1)%% (length(vector) + 1)
			if (prev == 0) {
				prev = 1
			}
		}

		if ((vector[prev] == 0 || prev %in% all.indexes) && vector[curr] == v) {

			all.indexes = c(all.indexes, sublist, curr)
			sublist = NULL
		} else if (vector[curr] == v) {

			sublist = c(sublist, curr)
		} else if (vector[curr] != 0) {
			sublist = NULL
		}
	}
	all.indexes
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){

}
