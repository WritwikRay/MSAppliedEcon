##################################################
## IntroR_BaseR.R
## by Cristian Huse -- cristian.huse@uol.de
## Current version: 20210930
## First version:   20210101

# This lab session presents you with rudimentary features of "base R", 
# i.e., R without fancy libraries/packages.
# The idea is to get you started and help you get confidence with R. 
##################################################



###################
### Basic Commands
###################

# Defining an object x via the assignment operator "<-" is equivalent to using "="
x <- c(1,3,2,5)
x
x = c(1,6,2)
x

# Now modify x
y <- c(1,4,3)

# x and y are understood as lists, these are their lengths
length(x)
length(y)

# Since they have the same length, can perform the following element-by-element addition
x+y
# Try to redefine x <- c(1,3,2,5) and perform the addition again. What happens?

# Note, that they are not matrices
?dim
dim(x)
dim(y)

?is.matrix
is.matrix(x)
is.matrix(y)

# But they can be transformed into matrices
x<-as.matrix(x,ncol=1)
y<-as.matrix(y,ncol=1)
dim(x)
dim(y)
x
y
cbind(x,y)

# And can be summed element-by-element
x+y



# Checking what is in memory
objects()
# alternatively
ls()
#Remove specific objects
rm(x,y)
objects()
#Remove all objects at once
rm(list=objects())



# The above were not matrices, which play an important part in Statistics, Econometrics, and coding. 

# How to define a matrix? Seek help!
?matrix

z=matrix(data=c(1,2,3,4), nrow=2, ncol=2)
z
z=matrix(c(1,2,3,4),2,2)
z

# Compare the following with the above
matrix(c(1,2,3,4),2,2,byrow=TRUE)

# The following are also element-wise
sqrt(z)
z^2



# Back to x and y, let's check some basic matrix operations
# Define them as matrices
x<-matrix(data=c(1,6,2), ncol=1) #3x1
y<-matrix(data=c(1,4,3), ncol=1) #3x1


# Matrix transposition: t()
# Matrix multiplication: %*%
# The following should be scalars (1 x 1)
t(x)%*%  #(1x3)(3x1)
t(y)%*%x
# The following should be (3 x 3)
x%*%t(y) # (3x1)(1x3)
y%*%t(x)



#Now, let's try a famous formula
# Create column vector of ones
ones<-matrix(c(1,1,1),ncol=1)
# Bind the above with the x vector to create a matrix -- call it x
x<-cbind(ones,x)
x
# solve(X) inverts the matrix X
# Let's check it works
solve(t(x)%*%x)%*%(t(x)%*%x)

# Finally, let's compute that famous matrix
# betahat = (X'*X)^(-1)*(X'*y)
solve(t(x)%*%x)%*%(t(x)%*%y)

# Alternatively
library(matlib)
inv(t(x)%*%x)%*%(t(x)%*%y)



##################################
### Now, let's simulate some data
##################################

#seed(1234)
x=rnorm(50)
y=x+rnorm(50,mean=50,sd=.1)
y2=50+x+rnorm(50,mean=0,sd=.1)
cor(x,y)
cor(x,y2)

?lm
ols0<-lm(y~x)
summary(ols0)
# What did it do?
summary(lm(y2~x))





############################
### Graphics
############################

# Generate data, set up basic plot, save it to pdf (check what your working directory actually is)
x=rnorm(100)
y=rnorm(100)
plot(x,y)
plot(x,y,xlab="this is the x-axis",ylab="this is the y-axis",main="Plot of X vs Y")


# Generating sequences -- different ways
x=seq(1,10)
x
x=1:10
x

# Now let's create some contour plots to impress your friends
x=seq(-pi,pi,length=50)
y=x

?outer
f=outer(x,y,function(x,y)cos(y)/(1+x^2))

# Two versions of the same graph
contour(x,y,f)
contour(x,y,f,nlevels=45,add=T)

# Another function
fa=(f-t(f))/2
contour(x,y,fa,nlevels=15)

?image
image(x,y,fa)

# Different versions of the same graph
?persp
persp(x,y,fa)
persp(x,y,fa,theta=30)
persp(x,y,fa,theta=30,phi=20)
persp(x,y,fa,theta=30,phi=70)
persp(x,y,fa,theta=30,phi=40)



############################
### Indexing and subsetting
############################
A=matrix(1:16,4,4)
A

# Pick element (2,3), then different chunks of the matrix
A[2,3]
A[c(1,3),c(2,4)]
A[1:3,2:4]
A[1:2,]
A[,1:2]
A[1,]

# Pick elements except for (-) some elements
A[-c(1,3),]
A[-c(1,3),-c(1,3,4)]
dim(A)





#########################
### Loading Data
#########################

# There are several functions to read data, e.g., read.table, read.csv. 
# Here, we are loading data available via the library ISLR, but we will use those functions later
# For reference, the most efficient package/library to manipulate data is data.table

# Alternative 1: From a library (if applicable)
library(ISLR)
attach(Auto)
fix(Auto)
# Click to resume
# Disconnect the data
detach(Auto)

# Alternative 2: From a .csv file located in a pre-specified folder
setwd("XXX")
?read.table
#The following are incorrect -- why?
#Auto=read.table("Auto.data")
#Auto=read.table("Auto.data",header=TRUE)

#This is correct
Auto=read.table("Auto.data",header=TRUE,na.strings="?")
fix(Auto)

dim(Auto)
Auto[1:4,]
head(Auto)

# What does this do?
Auto=na.omit(Auto)
dim(Auto)
names(Auto)
?na.omit

# Additional Graphical and Numerical Summaries

#This works if attach() was used -- it makes the variables in the data frame available by name
plot(cylinders, mpg)

#This works in general: dataframe$variable
plot(Auto$cylinders, Auto$mpg)

summary(Auto$cylinders)

# What does this do?
dv_cylinders=as.factor(Auto$cylinders)
summary(dv_cylinders)

# Different versions of a Boxplot
plot(dv_cylinders, Auto$mpg)
plot(dv_cylinders, Auto$mpg, col="red")
plot(dv_cylinders, Auto$mpg, col="red", varwidth=T)
plot(dv_cylinders, Auto$mpg, col="red", varwidth=T,horizontal=T)
plot(dv_cylinders, Auto$mpg, col="red", varwidth=T, xlab="cylinders", ylab="MPG")

# Plotting histograms
hist(Auto$mpg)
hist(Auto$mpg,col=2)
hist(Auto$mpg,col=2,breaks=15)

# What does this do?
pairs(Auto)
# Maybe better
pairs(~ mpg + displacement + horsepower + weight + acceleration, data = Auto)

plot(Auto$horsepower,Auto$mpg)

# Summary statistics -- whole data or particular variable
summary(Auto)
summary(Auto$mpg)

# Some descriptive regressions -- let's predict fuel economy of vehicles (mpg)
names(Auto)

# Compare the following specifications
ols1<-lm(mpg ~ displacement + horsepower + weight + acceleration + cylinders, data=Auto)
ols2<-lm(mpg ~ displacement + horsepower + weight + acceleration + dv_cylinders, data=Auto)

# Compare the following results -- note we are using base R, so nothing fancy here
summary(ols1)
summary(ols2)


