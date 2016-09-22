
##############R BLOGGERS###################################

################ For LOOP#############################

for (year in 2010:2015)
  print(paste("The year is", year))


#Using Next Let's have a look at a more mathematical example. Suppose you need to 
#print all uneven numbers between 1 and 10 but
#even numbers should not be printed. In that case your loop would look like this:

for (i in 1:10) {
  if (!i %% 2){
    next
  }
  print(i)
}
#When i is between 1 and 10 we enter the loop and if not the loop stops. In case we enter the loop, we need to check 
#if the value of i is uneven. If the value of i has a remainder of zero when divided by 2 (that's why we use the
#modulus operand %%) we don't enter the if statement, execute the print function and loop back. In case the remainder
#is non zero, the if statement evaluates to TRUE and we enter the conditional. Here we now see the next statement
#which causes to loop back to the i in 1:10 condition thereby ignoring the the instructions that follows (so the print(i)).



##################gg PLOT########################################
library(ggplot2)
data(diamonds)

############## basic plotting
plot(diamonds$carat, diamonds$price, col = diamonds$color,
     pch = as.numeric(diamonds$cut))

############ggplot2
ggplot(diamonds, aes(carat, price, col = color, shape = cut)) +
  geom_point()


#########################Using apply, sapply, lapply in R
#create data
m <- matrix(data=cbind(rnorm(30, 0), rnorm(30, 2), rnorm(30, 5)), nrow=30, ncol=3)

#Description: "Returns a vector or array or list of values obtained by applying
#a function to margins of an array or matrix."




# find mean of all rows
apply(m, 1, mean)
#find mean of all coloumns
apply(m, 1, mean)
# count of all negative numbers in each row
apply(m, 1, function(x) length(x[x<0]))

# count of all negative numbers in each coloumn
apply(m, 2, function(x) length(x[x<0]))

apply(m, 2, function(x) is.matrix(x))
apply(m, 2, is.vector)

############Using sapply and lapply
#passing a set of index values.#Here we will use sapply, which works on a list or vector of data. 
sapply(1:3, function(x) x^2) #givess vector output
lapply(1:3, function(x) x^2) # gives list output
sapply(1:3, function(x, y) mean(y[,x]), y=m)

# create a list with 2 elements
l <- list(a = 1:10, b = 11:20)
# the mean of the values in each element
lapply(l, mean)
# the sum of the values in each element
lapply(l, sum)

#sapply
#Description: "sapply is a user-friendly version of lapply by default returning a vector or matrix if appropriate."
#That simply means that if lapply would have returned a list with elements $a and $b, sapply will return either a 
#vector, with elements [['a']] and [['b']], or a matrix with column names "a" and "b". Returning to our previous 
#simple example:


#by-Description: "Function 'by' is an object-oriented wrapper for 'tapply' applied to data frames."
#The by function is a little more complex than that. Read a little further and the documentation tells you that 
#"a data frame is split by row into data frames subsetted by the values of one or more factors, and function 'FUN'
#is applied to each subset in turn." So, we use this one where factors are involved.
attach(iris)
head(iris)
# get the mean of the first 4 variables, by species
by(iris[, 1:4], Species, mean)


###############################replicate
##Description: "replicate is a wrapper for the common use of sapply for repeated evaluation of an expression
#(which will usually involve random number generation)."##The replicate function is very useful. Give it two
#mandatory arguments: the number of replications and the function to replicate; a third optional argument, 
#simplify = T, tries to simplify the result to a vector or matrix. An example - let's simulate 10 normal 
#distributions, each with 10 observations
replicate(10, rnorm(10))



#tapply
attach(iris)
#Usage is "tapply(X, INDEX, FUN = NULL, ., simplify = TRUE)", 
#where X is "an atomic object, typically a vector" and INDEX is "a list of factors, each of same length as X".

# mean petal length by species
tapply(iris$Petal.Length, Species, mean)




