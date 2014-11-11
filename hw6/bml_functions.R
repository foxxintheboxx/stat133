#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p) { 
	m = matrix(0, r, c)
	total = r * c
	m + (runif(total ,0,1) < p) * sample(c(1,2), total ,replace= T)
}



#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.


bml.step <- function(m) {
	#make original

	original = matrix(rep(m), nrow = nrow(m), ncol = ncol(m))
	if (nrow(m) != 1 && ncol(m) != 1)
	{
		blocked <- m[,c(2:ncol(m), 1)]!= 0
		red_cars <- m*(m==1)
		m = m*(m!=1) + red_cars*blocked + (red_cars*!blocked)[,c(ncol(m),1:(ncol(m)-1))]

		#get blue cars
		blocked <- m[c(nrow(m), 1:(nrow(m) - 1)),] != 0
		blue_cars <- m*(m==2)
		m = m*(m!=2) + blue_cars*blocked + (blue_cars * !blocked)[c(2:nrow(m), 1),]
	} else if (nrow(m) == 1 && ncol(m) > 1) {
		blocked <- m[,1]!= 0
		red_cars <- m*(m==1)
		m = m*(m!=1) + red_cars*blocked + (red_cars*!blocked)[,c(ncol(m),1:(ncol(m)-1))]

		#get blue cars
		blocked <- m[1,] != 0
		blue_cars <- m*(m==2)
		m = m*(m!=2) + blue_cars*blocked + (blue_cars * !blocked)[1,]
	} else if (nrow(m) > 1 && ncol(m) == 1){
		blocked <- m[,1]!= 0
		red_cars <- m*(m==1)
		m = m*(m!=1) + red_cars*blocked + (red_cars*!blocked)[,1]

		#get blue cars
		blocked <- m[c(nrow(m), 1:(nrow(m) - 1)),] != 0
		blue_cars <- m*(m==2)
		m = m*(m!=2) + blue_cars*blocked + (blue_cars * !blocked)[c(2:nrow(m), 1),]
	}


	compared = m == original
	grid.new = FALSE

	if (FALSE %in% compared)
	{
		grid.new = TRUE
	}
	list(grid.new, m)
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
	m = bml.init(r, c, p)
	changed = TRUE
	steps = 0

	#image(m, col = c("white", "red", "blue"))
	while (changed == TRUE && steps < 6000)
	{

		lst = bml.step(m)
		changed = lst[[1]]
		m = lst[[2]]
		steps = steps + 1		
	}
	#image(m, col = c("white", "red", "blue"))
	steps

}
