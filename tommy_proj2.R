library(data.table)
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
# Turns out there's only one individual who wore the acceleromator for this amount of time?

# Let's check the histogram of the cutoff time
test_set <- training_data[ which(T > 1338930532818.5),]
training_set <- training_data[ which(T <= 1338930532818.5),]

# What does this data look like?
hist(training_data$T)
# It's apparent that many peole stopped wearing the device after a certain amount of time.
as.character(quantile(training_data$T))

# We should try and visualize this somehow.