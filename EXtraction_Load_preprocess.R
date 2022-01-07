#Data Extraction
#Data Cleaning
#Data loading
#Feature Adding


rm(list=ls(all=TRUE))
gc(reset=TRUE)
#garbage collection 

#### put the required packages here
require(reshape)



#####################
## Preprocess Data ##
#####################
cat('Preprocess data ...\n')

#### Read in all the data provided
dfStore <- read.csv(file='D:/project/Mini_project/stores.csv', header=TRUE)
dfTrain <- read.csv(file='D:/project/Mini_project/train.csv', header=TRUE)
dfTest <- read.csv(file='D:/project/Mini_project/test.csv', header=TRUE)
dfFeatures <- read.csv(file='D:/project/Mini_project/features.csv', header=TRUE)


#### Merge Type and Size of stores
dfTrain <- merge(x=dfTrain, y=dfStore, all.x=TRUE)
dfTest <- merge(x=dfTest, y=dfStore, all.x=TRUE)


#### Merge all the features
dfTrain <- merge(x=dfTrain, y=dfFeatures, all.x=TRUE)
dfTest <- merge(x=dfTest, y=dfFeatures, all.x=TRUE)

str(dfTrain)

####cleaning
dfTrain$IsHoliday <- as.integer(dfTrain$IsHoliday)
dfTest$IsHoliday <- as.integer(dfTest$IsHoliday)

dfTest[is.na(dfTest)] = 0
dfTrain[is.na(dfTrain)] = 0
summary(dfTest)

#### Convert Date to character
dfTrain$Date <- as.character(dfTrain$Date)
dfTest$Date <- as.character(dfTest$Date)
dfFeatures$Date <- as.character(dfFeatures$Date)

str(dfTrain)


#### Compute the number of days back to baseline date
baseline_date <- as.Date('2010-02-05')
dfTrain$Days <- as.numeric(as.Date(dfTrain$Date) - baseline_date)
dfTest$Days <- as.numeric(as.Date(dfTest$Date) - baseline_date)


#### Compute the corresponding day index for plotting figure
all_dates <- sort(unique(dfFeatures$Date))
dfTrain$Day_Index <- sapply(dfTrain$Date, function(d)which(d==all_dates))
dfTest$Day_Index <- sapply(dfTest$Date, function(d)which(d==all_dates))


#### Split Date into Year/Month/Day
## train
d <- strsplit(dfTrain$Date, '-')
d <- as.numeric(unlist(d))
dmat <- matrix(d, dim(dfTrain)[1], 3, byrow=T)
dfTrain$Year <- dmat[,1]
dfTrain$Month <- dmat[,2]
dfTrain$Day <- dmat[,3]

## test
d <- strsplit(dfTest$Date, '-')
d <- as.numeric(unlist(d))
d <- matrix(d, dim(dfTest)[1], 3, byrow=T)
dfTest$Year <- d[,1]
dfTest$Month <- d[,2]
dfTest$Day <- d[,3]


#### Switch columns for convenience :)
col.vars <- c('Store', 'Dept', 'Date', 'Year', 'Month', 'Day', 'Days', 'Day_Index', 'IsHoliday', 
              'Type', 'Size', 'Temperature', 'Fuel_Price', 'CPI', 'Unemployment',
              'MarkDown1', 'MarkDown2', 'MarkDown3', 'MarkDown4', 'MarkDown5', 'Weekly_Sales')


dfTrain <- dfTrain[,col.vars]

# note that we don't have Weekly_Sales for dfTest
dfTest <- dfTest[,col.vars[-length(col.vars)]]




#### Sort the dataframe with respect to c('Store', 'Dept', 'Date')
vars <- c('Store', 'Dept', 'Date')

## train
dfTrain <- sort_df(dfTrain, vars)
## test
dfTest <- sort_df(dfTest, vars)




#### Compute test_ID for the convenience of making submission
testID <- paste(dfTest$Store, dfTest$Dept, dfTest$Date, sep='_')

# verbose
cat('Done\n')
str(dfTrain)
str(dfTest)

#######################
## Generate Features ##
#######################

cat('Generate features ...\n')


#### Indicate which of the four holidays they are
# ------------------------------------------------------------------------------
# For those IsHoliday=1, we further indicate which of the four holidays they are
# For convenience, the holiday weeks for the four holidays are:
# Super Bowl: 2010-02-12, 2011-02-11, 2012-02-10, 2013-02-08
# Labor Day: 2010-09-10, 2011-09-09, 2012-09-07, 2013-09-06
# Thanksgiving: 2010-11-26, 2011-11-25, 2012-11-23, 2013-11-29
# Christmas: 2010-12-31, 2011-12-30, 2012-12-28, 2013-12-27
# ------------------------------------------------------------------------------
# verbose
cat('Generate feature set 1 ...\n')

Super_Bowl <- c('2010-02-12', '2011-02-11', '2012-02-10', '2013-02-08')
Labor_Day <- c('2010-09-10', '2011-09-09', '2012-09-07', '2013-09-06')
Thanksgiving <- c('2010-11-26', '2011-11-25', '2012-11-23', '2013-11-29')
Christmas <- c('2010-12-31', '2011-12-30', '2012-12-28', '2013-12-27')
Holidays <- data.frame(Super_Bowl=Super_Bowl,
                       Labor_Day=Labor_Day,
                       Thanksgiving=Thanksgiving,
                       Christmas=Christmas)
func <- function(d, Holidays){
  # Note that each column corresponding to a specific holiday, so we
  # use the column index returned by which() and then return the
  # corresponding colname
  d <- as.character(d)
  holiday <- colnames(Holidays)[which(Holidays == d, arr.ind=TRUE)[2]]
  return(holiday)
}

## train
dfTrain$Holiday <- rep('No', dim(dfTrain)[1])
dfTrain$Holiday[dfTrain$IsHoliday == TRUE] <- sapply(dfTrain$Date[dfTrain$IsHoliday == TRUE],
                                                     function(d)func(d, Holidays))
## test
dfTest$Holiday <- rep('No', dim(dfTest)[1])
dfTest$Holiday[dfTest$IsHoliday == TRUE] <- sapply(dfTest$Date[dfTest$IsHoliday == TRUE],
                                                   function(d)func(d, Holidays))
# verbose
cat('Done\n')

#Feature2
#### What is the last/next holiday?

# verbose
cat('Generate feature set 2 ...\n')

HolidayTimeLine <- rbind(Super_Bowl,Labor_Day,Thanksgiving,Christmas)
HolidayTimeLine <- unlist(HolidayTimeLine)

# Since the most front holiday in the training data is '2010-02-12', so we have to
# insert the date of Christmas in 2009 to deal with it

Christmas_2009 <- as.character(as.Date('2010-12-31')-364)
# to know how to get 364, check: diff(as.Date(Christmas))/diff(as.Date(Labor_Day))

HolidayTimeLine <- c(Christmas_2009, HolidayTimeLine)
holiday_names <- c('Super_Bowl', 'Labor_Day', 'Thanksgiving', 'Christmas')
holiday_names <- c('Christmas', rep(holiday_names, 4))

# convert to Date class
HolidayTimeLine <- as.Date(HolidayTimeLine)

func <- function(d, HolidayTimeLine, holiday_names){
  # find the closest date to d in HolidayTimeLine
  dif <- as.numeric(d-HolidayTimeLine)
  
  ind <- which.min(abs(dif))
  # d comes after the closest date
  
  if(dif[ind] > 0){
    last_holiday_ind <- ind
  }else{
    last_holiday_ind <- ind - 1
  }  
  last_holiday <- holiday_names[last_holiday_ind]
  
  next_holiday <- holiday_names[last_holiday_ind+1]


  return(c(last_holiday,
           next_holiday))
}


## train
dates <- as.Date(dfTrain$Date)
results <- sapply(dates, function(d)func(d, HolidayTimeLine, holiday_names))
dfTrain$Last_Holiday <- results[1,]
dfTrain$Next_Holiday <- results[2,]


## test
dates <- as.Date(dfTest$Date)
results <- sapply(dates, function(d)func(d, HolidayTimeLine, holiday_names))

dfTest$Last_Holiday <- results[1,]
dfTest$Next_Holiday <- results[2,]

# verbose
cat('Done\n')

#### Convert variables to factors
factor.vars <- c('Store', 'Dept', 'Type', 'Month',
                 'IsHoliday', 'Holiday', 'Last_Holiday', 'Next_Holiday')


str(dfTrain)
###############
## Save Data ##
###############
cat('Save data ...\n')

#### We are now safe to drop variable Date
dfTrain <- dfTrain[,-which(colnames(dfTrain)=='Date')]
dfTest <- dfTest[,-which(colnames(dfTest)=='Date')]

#### save dfTrain and dfTest
save(list=c('dfTrain', 'dfTest', 'testID'),
     file='D:/project/Mini_project/data/training_testing_data.RData')

# verbose
cat('Done\n\n')
gc(reset=TRUE)
cat('All done\n\n')

