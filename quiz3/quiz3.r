# Quiz 3
# Number of Problems: 6
# The quiz is out of 22 points.

# Function 1 (3 points)
# Write a function called numAtElements. Your function should take the following
# arguments
#   <chvec>: A character vector containing strings of letters, possibly possibly
#     with the "@" symbol
#
# and return the following
#   <num.at>: an integer indicating how many elements of <chvec> contain the "@"
#     symbol. For example: numAtElements(c('karl', 'k@rl', '@@@')) should return 2
numAtElements <- function(chvec) {
  
  # your code here
  return(length(grep("@", chvec)))
}

# Function 2 (3 points)
# Write a function called unexclaim. Your function should take the following
# arguments
#   <chstring>: a character vector of length 1 (contains only one string).
#
# and return the following
#   <newstring>: a character vector of length 1 where all ! symbols have been
#     replaced by . symbols
unexclaim <- function(chstring) {
  
  # your code here
  return(gsub("!", ".", chstring))

}

# Function 3 (3 points)
# Write a function called updateDate. Your function should take the following
# arguments
#   <dates>: a character vector of dates of the form "month, year" (e.g. "May, 2001")
#   <old.yr>: a string indicating the year for which elements will be updated
#     (e.g. "2002")
#
# and return the following
#   <updated.dates>: a character vector of dates where <old.yr> has been replaced by
#     '2015'. This vector should only contain the elements whose date has been
#     updated. For example updateDate(c('May, 2010', 'June, 2011'), '2010') should
#     return 'May, 2015'.
updateDate <- function(dates, old.yr) {
  
  # your code here
  indexes = grep(old.yr, dates)
  return(gsub("[[:digit:]]{4}", "2015", dates[indexes]))
}

# Function 4 (4 points)
# Write a function called countcatdog that counts the number of instances of
# "cat" (case-insensitive) and the number of instances of "dog"
# (case-insensitive) in a string. Your function should take the following
# arguments 
#   <chvec>: A character vector of length 1 (chvec contains only 1 string)
#
# and return the following
#   <counts>: An integer vector of length 2 with names "cat" and "dog".
#             For example, countcatdog("doGCATcat abcAt") returns:
#                    cat dog
#                     3   1
countcatdog<- function(chvec){
  chvec = tolower(chvec)
  cats = length(gregexpr("cat", chvec)[[1]])
  dogs = length(gregexpr("dog", chvec)[[1]])
  return(c(cats, dogs))
  # your code here
}

# Function 5 (3 points)
# Write a function called sumDigits that compute the sum of all the digits in
# a string.  The function should return 0 if there is no digit in the
# string. Your function should take the following arguments:
#   <chvec>: A character vector of length 1 (chvec contains only 1 string)
#
# and return the following
#   <total>: A single number (the sum of all the digits in chvec)
sumDigits <- function(chvec){
  chars = strsplit(chvec, "")[[1]]
  indexes  = grep("[[:digit:]]", chars)
  # your code here
  return(sum(as.integer(chars[indexes])))
}

# Some test cases:
# print(all.equal(sumDigits("1z3p ! 21"), 7))
# print(all.equal(sumDigits("abcdefg"), 0))


# Function 6 (6 points)
# DNA.vec is a character vector of strings of DNA. It contains at least two
# strings of DNA. For simplicity, each string has only 10 characters.  Note that
# a DNA is always made up of A, T, C, G's. Write a function called dnaTransform
# that performs the following:

# Step 1: Find the first two DNA strings in DNA.vec that contains the sequence
# "ATTA"; call them DNA1 and DNA2. If there are less than two DNA strings that
# contains the sequence "ATTA", the function ends immediately and returns the
# first two elements in DNA.vec

# Step 2: Split DNA1 into two halves (i.e. strings of length 5 each).
# (For example, if DNA1 is "ATTATAGCCA", then we have "ATTAT" as the first half
# and "AGCCA" as the second half)

# Step 3: Split DNA2 into two halves (as in Step 2).

# Step 4: Return a character vector of two strings:
# --first string: the first half of DNA1 combined with the second half of DNA2
# --second string: the first half of DNA2 combined with the second half of DNA1

# Input:
#   <DNA_vec>: A character vector of DNAs
# Output:
#   <DNA_final>: A character vector of two DNAs

dnaTransform <- function(DNA.vec){

  indexes = grep("ATTA", DNA.vec)
  if (length(indexes) < 2) {
  	x <- DNA.vec[c(1,2)]
  	return(x)
  }
  DNA1 = DNA.vec[indexes[1]]
  DNA2 = DNA.vec[indexes[2]]
  # your code here
  split1a = gsub(" ", "", paste(strsplit(DNA1, "")[[1]][1:5], "", collapse = ""))
  split1b = gsub(" ", "", paste(strsplit(DNA1, "")[[1]][6:10], "", collapse = ""))
  split2a = gsub(" ", "", paste(strsplit(DNA2, "")[[1]][1:5], "", collapse = ""))
  split2b = gsub(" ", "", paste(strsplit(DNA2, "")[[1]][6:10], "", collapse = ""))
  c(gsub(" ", "", paste(c(split1a, split2b), "", collapse = "")), gsub(" ", "", paste(c(split2a, split1b), "", collapse = "")))
}

# Some test cases:
# print(all.equal(dnaTransform(c("AAAAAAAAAA", "ATTAGATACT", "ATACATTACG")), c("ATTAGTTACG", "ATACAATACT")))
# print(all.equal(dnaTransform(c("ATCGATCGAT", "TCGATCGATT", "ATTTTTTTTT")), c("ATCGATCGAT", "TCGATCGATT")))

# End of Quiz 3
