#use built in iris dataset
head(iris)
#load in some tidyverse packages
library(dplyr)
library(ggplot2)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

# hint: consider using a list, and also new vectors for regression variables

# filtering out the iris dataset to include only versicolor
iris.versi <- iris[iris$Species == "versicolor",]

# making dataframes of the columns to be used in regression
vars_dep<- data.frame(iris.versi$Sepal.Length, iris.versi$Petal.Length, iris.versi$Sepal.Length)
colnames(vars_dep) <- c("Sepal.Length", "Petal.Length", "Sepal.Length")
vars_indep <- data.frame(iris.versi$Sepal.Width, iris.versi$Petal.Width, iris.versi$Petal.Length)
colnames(vars_indep) <- c("Sepal.Width", "Petal.Width", "Petal.Length")
# creating a list to store the regression tables
reg_table_list <- list(0)

for (i in 1:3){
  reg_table_list[[i]] <- lm(vars_dep[,i]~vars_indep[,i])
}

#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))
iris_new <- full_join(iris, height)

#####################################
##### Part 3: plots in ggplot2  #####
#####################################

#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_point()

#3b. make a scatter plot with ggplot and get rid of  busy grid lines
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_point() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color = Species, size = Petal.Length)) + 
  geom_point() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Sepal Length (cm)",
       y = "Sepal Width (cm)",
       title = "Variation of Sepal Size")

#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		

# For the base r plot, it only plotted with the standard plot(x,y) scheme. In ggplot,
# it is necessary to create a new dataframe to start the ggplot and columns of the dataframe
# are used as arguments of aes() inside the ggplot function to set the data to plot.
# furthermore, the elements in a ggplot plotn are set in layers, where ggplot() makes up the
# base layer and we can keep adding elements with functions like geom_point(), geom_line(), etc.
# The elements of a ggplot plot are combined together using a + sign.