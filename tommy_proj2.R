library(data.table)
library(rgl)
set.seed(244)

# Import the data using fread for faster times.

fileName <- 'data/train.csv'

system.time(
  training_data <- fread(fileName)
)


# First, how many distinct individuals there are
length(unique(training_data$Device))
# There are 387 unique devices.

# What is the range of time for the training set?
min <- min(training_data$T)
max <- max(training_data$T)
range <- max - min
num_days <- ((((range / 1000)/60)/60)/24)
# more or less a years worth of data.

# Split the dataset into a training and test set, probably like 60 days.
# what is the value of 60 days in acceleratomor (sp) time?
cutoff <- 1000*60*60*24*60
cutoff_date <- max - cutoff

test_set <- training_data[ which(T > cutoff_date),]
training_set <- training_data[ which(T <= cutoff_date),]

# What does this data look like?
head(training_set)

# We should try and visualize this somehow.

with(df,lines3d(X,Y,Z))
with(df[1,],points3d(X,Y,Z,size=7,col="red"))
with(df[-1,],points3d(X,Y,Z,col="blue"))
axes3d()
title3d(xlab="X",ylab="Y",zlab="Z")

