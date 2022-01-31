# Practicing for Activity 2 in GEOG 331
# FI 01/31/22

# vector of tree heights in meters

heights <- c(30, 41, 20, 22)
# converting the heights to cm
heights_cm <- heights*100
heights_cm

# look at the first tree height
heights[1]

# looks at the 2nd and 3rd heights
heights[2:3]


####MATRIX####
# create a matrix with two columns that are filled in by rows
mat <- matrix(c(1,2,3,4,5,6), ncol = 2, byrow=TRUE)

# create a matrix where the values fill in by columns
mat.col <- matrix(c(1,2,3,4,5,6), ncol = 2, byrow=FALSE)

# Looking at a specific value
mat.col[1,2]

# looking at all the values in a row or column
mat.col[1,] # first row
mat.col[,2] # second column


