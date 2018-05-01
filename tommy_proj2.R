library(data.table)
library(lubridate)
library(neuralnet)
library(splitstackshape)
library(MASS)
library(dplyr)
library(ggplot2)
library(tidyr)


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
min_date <- as.POSIXct(min, origin="1970-01-01")
max <- max(training_data$T)
max_date <- as.POSIXct(max, origin="1970-01-01")


# We've learned that the time stamps are correct but the dates are not. Thus, 
# we are going ahead and extracting the timestamp.
# https://www.kaggle.com/c/accelerometer-biometric-competition/discussion/5212#27715

# This piece of code rounds the date to the nearest hour. Since that is all we care about.
format(round_date(max_date, unit = "hour"), format = '%T')

# As such, let's convert each timestamp to the rounded "hour time":
training_data$posix <- as.POSIXct(training_data$T, origin="1970-01-01")
training_data$hour <- format(round_date(training_data$posix, unit = "hour"), format = '%T')

training_data$hour_num <- paste(hour(training_data$hour),minute(training_data$hour), sep=":")
training_data$hour_int <- as.integer(substr(training_data$hour, start = 1, stop = 2))

small_df <- training_data[sample(nrow(training_data), 400), ]
small_df$hour_int <- as.integer(substr(small_df$hour, start = 1, stop = 2))

save(training_data, file = "training_data_complete.Rdata")

system.time(
  list2env(stratified(training_data, "Device", .50, bothSets = TRUE), envir = .GlobalEnv)
)

system.time{
  save(SAMP1, file = "training.Rdata")
  save(SAMP2, file = "test.Rdata")
}



#Fitting the neural net on the entire df
n <- names(training_data)
f <- as.formula(paste("Device ~", paste(n[!n %in% c("Device", "posix", "T", "hour")], collapse = " + ")))
system.time(
  nn <- neuralnet(f,data=small_df, linear.output=FALSE)
)


predictions <- (pr.nn$neurons[[2]])
actual <- SAMP2

status <-  predictions[,5] - SAMP2$hour_int



######### Analysis to breakdown by frequency (small sample)

train_freq <- SAMP1 %>% group_by(Device,hour_int) %>% summarise(Freq=n())
#example plot
samp<- train_freq[train_freq[, "Device"] <40,]

freq_plot <- ggplot(samp, aes(hour_int, Freq,colour=Device)) + 
  geom_line(aes(group = Device))

print(freq_plot + ggtitle("Frequency of Recording by Hour for DeviceIDs < 400 ") + labs(y="Frequency of Recordings", x = "Hour of the Day"))
samp$Device <- as.factor(samp$Device)

# reshape to wide format
a <- spread(samp, hour_int, Freq)
names(a) <- c("Device", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen", "Twenty", "Twentyone", "Twentytwo", "Twentythree")

# random forest to predict Device
rf_model <- randomForest(Device ~ One+ Two+ Three+ Four+ Five+Six+ Seven+ Eight+ Nine+ Ten+ Eleven+ Twelve+ Thirteen+ Fourteen+ Fifteen+ Sixteen+ Seventeen+ Eighteen+ Nineteen+ Twenty+ Twentyone+ Twentytwo+ Twentythree, ntree = 400, data = a)
prediction <- predict(rf_model)


######### Analysis to breakdown by frequency (actual sample)

train_freq <- SAMP1 %>% group_by(Device,hour_int) %>% summarise(Freq=n())
test_freq <- SAMP2 %>% group_by(Device,hour_int) %>% summarise(Freq=n())

train_freq$Device <- as.factor(train_freq$Device)
test_freq$Device <- as.factor(test_freq$Device)


# reshape to wide format
reshape_train <- spread(train_freq, hour_int, Freq)
reshape_test <- spread(test_freq, hour_int, Freq)
names(reshape_train) <- c("Device", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen", "Twenty", "Twentyone", "Twentytwo", "Twentythree")
names(reshape_test) <- c("Device", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen", "Twenty", "Twentyone", "Twentytwo", "Twentythree")
reshape_test_without <- reshape_test[,c( "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen", "Twenty", "Twentyone", "Twentytwo", "Twentythree") ]


# random forest to predict Device
rf_model <- randomForest(Device ~ One+ Two+ Three+ Four+ Five+Six+ Seven+ Eight+ Nine+ Ten+ Eleven+ Twelve+ Thirteen+ Fourteen+ Fifteen+ Sixteen+ Seventeen+ Eighteen+ Nineteen+ Twenty+ Twentyone+ Twentytwo+ Twentythree, ntree = 1000, data = reshape_train)
prediction <- predict(rf_model, reshape_test_without)

#Side by side
new_df <- reshape_test
new_df$predictions <- prediction
new_df$original <- reshape_test$Device
sum(new_df$original == new_df$predictions)


