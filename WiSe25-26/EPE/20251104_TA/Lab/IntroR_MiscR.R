##################################################
# IntroR_MiscR.R
## by Cristian Huse -- cristian.huse@uol.de
## Current version: 20210930
## First version:   20210101

# This lab session presents miscellaneous additional features of R.
# Please note that this is to provide you a big picture of its capabilities rather than to 
# get into detail

##################################################



##################################################################
# What is your working directory?
##################################################################
getwd()
# Note: Windows uses backlash (\) whereas R uses "forward "/" or double backlash "\\" (?!)

# Get the files of your wd
list.files(getwd())

# Now change the wd to the desired one using setwd('path_with_forward_lash'), e.g.,
setwd('XXX')




##################################################################
# Hello, World!
##################################################################
# The following are equivalent
print('Hello, World!')

print("Hello, World!")

word1 <- "Hello"
word2 <- "World"
paste(
  paste(word1, word2, sep = ", "), "!", sep = ""
  )

# The %>% is the pipe operator (from package maggittr), enters its 
# preceding argument ("Hello, World.") into the function print()
library(magrittr)
"Hello, World!" %>% print()

# Since R 4.1, there is a native (base R) pipe operator:
"Hello, World!" |> print()



##################################################################
# Data structures
##################################################################

# This is a vector, assigned to x using the concatenate function
x <- c(10.4, 5.6, 3.1, 6.4, 21.7)
# Note that this is a numeric vector

# As briefly discussed before, there are also character vectors, logical vectors, matrices, factors, 
# lists, data frames etc
# see https://cran.r-project.org/doc/manuals/r-devel/R-intro.html#Simple-manipulations-numbers-and-vectors
# for details



##################################################################
# data.frame -- the workhorse of statistics and econometrics
##################################################################

# Create some vectors based on https://www.wikidata.org/
cities <- c('New York', 'Belgrade', 'Moscow', 'Paris', 'Rome', 'Berlin')
countries <- c('USA', 'Serbia', 'Russia', 'France', 'Italy', 'Germany')
populations <- c(8405837,  1378682, 12692466, 2187526, 2872800, 3644826)
lattitudes <- c(40.730610,  44.787197, 55.751244, 48.864716,  41.902782, 52.520008)
longitudes <- c(-73.935242, 20.457273, 37.618423, 2.349014, 12.496366, 13.404954)

# Create a data.frame
?data.frame
myCities <- data.frame(city = cities,
                       country = countries, 
                       population = populations, 
                       lat = lattitudes, 
                       lon = longitudes, 
                       stringsAsFactors = FALSE)

print(myCities)

# Accessing variables
myCities$city
myCities$population

# Displaying subsets
print(myCities[ , c('lat', 'lon')])
myCities[1, c('lat', 'lon')]
myCities[1:2, c('lat', 'lon')]
myCities[ , c('lat', 'lon')]



###################################################
# Loading data
###################################################
# Go to http://insideairbnb.com/get-the-data.html
# Download the listings.csv for Amsterdam, North Holland, The Netherlands section (the first one)
# Save it into your data_raw sub-folder
# Open listings.csv in Microsoft Excel and save it using the same filename but as an .xlsx file, also 
# in data_raw directory

# Confirm the main folder
setwd('XXX')

# Define data folder and list available files in there -- paste0 just "glues" its arguments
?paste0
dataDir <- paste0(getwd(), '/data_raw/')
list.files(dataDir)

# We will now enter listings.csv into R and create a data.frame
?read.csv
filename <- paste0(dataDir, 'listings.csv')
listings <- read.csv(file = filename, 
                     header = T, 
                     check.names = F,
                     stringsAsFactors = F)

# Note:
# read.csv reads csv files into RAM
# file = complete file path
# header = T if first row of the file contains column names
# check.names = F tells R not to check whether the column names are valid R column names for a data 
# frame
# stringsAsFactors = F prevents R from the default of turning all character-valued columns into 
# factors 

# Have a look at the data
head(listings, 10)
tail(listings, 10)
str(listings)
colnames(listings)
objects(listings)

# Remove the data
rm(list=objects(listings))



# Read the data in xlsx format instead
install.packages('readxl')
library(readxl)
?read_excel
listings <- read_excel(paste0(dataDir, "listings.xlsx"), 
                       col_names = TRUE)
head(listings)

# Officially create the data.frame
listings <- as.data.frame(listings)

objects(listings)

# Subsetting -- note the equivalence
summary(listings$price)
listings[listings$price > 5000, c(2, 3)]
listings[listings$price > 5000, c('name','host_id')]



###########################################################
# A closer look at the listings data
###########################################################
str(listings)

# Calculating frequencies
# What is the distribution of room_type in listings?
# Note: Different data.frame's can coexist
roomType <- as.data.frame(table(listings$room_type))
colnames(roomType) <- c('room_type', 'frequency')
roomType <- roomType[order(roomType$frequency), ]
head(roomType)

#How many different room_type values do we observe?
roomTypes <- as.data.frame(table(listings$room_type))
colnames(roomTypes) <- c('room_type', 'count')
roomTypes





############################################################
# Bonus: A taste of more advanced plots
# Note: We won't invest much time in ggplots in this course
# (see Advanced Econometrics)
############################################################
library(ggplot2)

ggplot(data = roomTypes, 
       aes(x = room_type, y = count)) + 
  geom_bar(stat = 'identity', fill = "cadetblue3") +
  xlab('Room type') + 
  ylab('Count') +
  ggtitle('Room type distribution in Airbnb Listings') + 
  theme_bw() + 
  theme(panel.border = element_blank())
theme(panel.border = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5))

# Components of the ggplot graph:
# 1. define the data and the mapping of variables in aes() in a ggplot() call
# 2. add layers by using an appropriate geom, like geom_bar() in this example
# 3. style the plot by adding additional layers such as ggtitle() or theme()

# Step-by-step:
#1.
ggplot(data = roomTypes, 
       aes(x = room_type, y = count))
# doesn't do much

#2.
ggplot(data = roomTypes, 
       aes(x = room_type, y = count)) + 
  geom_bar(stat = 'identity', fill = "cadetblue3")
# geom_bar() adds a layer: 

#3.
ggplot(data = roomTypes, 
       aes(x = room_type, y = count)) + 
  geom_bar(stat = 'identity', fill = "cadetblue3") +
  xlab('Room type') + 
  ylab('Count') +
  ggtitle('Room type distribution in Airbnb Listings')
# added labels for axes and graph title

#4.
ggplot(data = roomTypes, 
       aes(x = room_type, y = count)) + 
  geom_bar(stat = 'identity', fill = "cadetblue3") +
  xlab('Room type') + 
  ylab('Count') +
  ggtitle('Room type distribution in Airbnb Listings') + 
  theme_bw() + 
  theme(panel.border = element_blank())
# making it cuter

# 5.
ggplot(data = roomTypes, 
       aes(x = room_type, y = count)) + 
  geom_bar(stat = 'identity', fill = "cadetblue3") +
  xlab('Room type') + 
  ylab('Count') +
  ggtitle('Room type distribution in Airbnb Listings') + 
  theme_bw() + 
  theme(panel.border = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5))
# centering the title

# 6.
ggplot(data = roomTypes, 
       aes(x = room_type, y = count)) + 
  geom_bar(stat = 'identity', fill = "cadetblue3") +
  xlab('Room type') + 
  ylab('Count') +
  ggtitle('Room type distribution in Airbnb Listings') + 
  theme_bw() + 
  theme(panel.border = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(size = 13))
# making it even cuter





#############################################################
# Reading large data sets in R ***efficiently***: data.table
#############################################################

install.packages('nycflights13')
library(nycflights13)
library(tidyverse)
library(data.table)
dataDir <- paste0(getwd(), "/data_raw/")

# Now we call the flights data from package nycflights13 using the operator "::"
flightsFrame <- nycflights13::flights
dim(flightsFrame)
# Not really huge, but useful to illustrate the efficiency of data.table below
?flights
str(flightsFrame)

# First save it in disk
write.csv(flightsFrame, paste0(dataDir, "flights.csv"))

# Now, we will compare the efficiency of two functions used to read data: 
# read.csv() from base R vs. data.table::fread()

t1 <- system.time(flightsFrame_baseR <- read.csv(paste0(dataDir, "flights.csv"),
                                           header = T,
                                           row.names = 1,
                                           stringsAsFactors = F,
                                           check.names = F)
)

t2 <- system.time(flightsFrame_DT <- fread(paste0(dataDir, "flights.csv"),
                                     header = T)
)

t1
t2

as.numeric(t1[1])/as.numeric(t2[1])
# This should show that fread is 5-10 times faster than read.csv
# It is also substantially faster than functions within the dplyr library

# Technical note: 
#There is a specific data.table class attached to R objects used as data.tables
class(flightsFrame_baseR)
class(flightsFrame_DT)
# fread accepts http and https URLs directly, check:
?fread



##################################################
# data.table operations
# Note that the following is a brief illustration, see the cheatsheet for the big picture
# https://www.rstudio.com/resources/cheatsheets/
# https://github.com/rstudio/cheatsheets/raw/master/datatable.pdf
##################################################

# Get rid of V1, which just counts observations
flightsFrame_DT$V1 <- NULL

# Filter rows by month
flightsFrame_DT[month > 6]

# Filter using multiple conditions
flightsFrame_DT[month == 1 & dep_delay > 0]

# Sort rows
flightsFrame_DT[order(dep_delay)]

# Selecting columns
# Option 1
arr_time <- flightsFrame_DT[, list(arr_time)]
class(arr_time)
# Option 2
arr_time <- flightsFrame_DT[, .(arr_time)]
class(arr_time)

# To obtain a vector from a data.table column:
arr_time <- flightsFrame_DT[, arr_time]
class(arr_time)

# Combining filtering and selection operations
selectedFlights <- flightsFrame_DT[arr_time > 100, .(arr_time, arr_delay, dest)]
head(selectedFlights)

# Create new variables
arr_time_hours <- flightsFrame_DT[air_time > 20, .(air_time, air_time_hours = air_time/60)]
head(arr_time_hours)

# Dropping observations with missing data
dim(flightsFrame_DT)
flightsFrame_DT <- flightsFrame_DT[!is.na(air_time)]
dim(flightsFrame_DT)

# Aggregating (by destination, in this case)
flightsFrame_DT[, .(avg_air_time = mean(air_time)), by = dest]

# Aggregation with filtering
flightsFrame_DT[dep_delay <= 0, .(dep_delay, avg_air_time = mean(air_time)), by = dest]

# Count observations by group
flightsFrame_DT[dep_delay <= 0, .N, by = dest]





###################################################################################
# Bonus: Joining (merging) different data sets (will be covered in another course)
###################################################################################
# Load the planes data set from the same package
planes <- nycflights13::planes
head(planes)

# Strategy:
# 1. use setkey()
# 2. promote planes to a data.table object
# 3. use merge()
setkey(flightsFrame_DT, tailnum)
planes <- data.table(planes)
setkey(planes, tailnum)
flightPlanes <- merge(flightsFrame_DT,
                      planes,
                      by = "tailnum",
                      all.x = T)
?merge

# Save resulting file in disk
fwrite(flightsFrame_DT, paste0(dataDir, "flightsFrame_DT_fwrite.csv"))

#For comparison: 
# write.csv(flightsFrame_DT, paste0(dataDir, "flightsFrame_DT_writecsv.csv"))





##################################################
# Appendix: Read data from a webpage
##################################################

# This is the link to listings.csv at the time of writing:
# http://data.insideairbnb.com/the-netherlands/north-holland/amsterdam/2021-08-06/visualisations/listings.csv

urlData <- 'http://data.insideairbnb.com/the-netherlands/north-holland/amsterdam/2021-08-06/visualisations/listings.csv'
listingsOnline <- read.csv(URLencode(urlData), 
                           header = T, 
                           check.names = F, 
                           stringsAsFactors = F)
head(listingsOnline)

# Note: URLencode() is a key argument because it seamlessly deals with the html bits of the 
# webpage

rm(listingsOnline)

