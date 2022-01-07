#Visualisation

rm(list=ls(all=TRUE))
gc(reset=TRUE)


setwd("D:/project/Mini_project/")
getwd()
#install.packages("lattice")
#### put the required packages here
require(lattice)

#### Read in all the data provided
load('D:/project/Mini_project/data/training_testing_data.RData')


figure_format <- 'png'

for(dept in sort(unique(sort(dfTrain$Dept)))){
  
  #### Create the corresponding dir
  filePath <- './visualization/weekly_sales'
  
  dir.create(filePath, showWarnings=FALSE, recursive=TRUE)
  
  if(figure_format=='pdf'){
    pdf(paste(filePath, '/Dept', dept,'.pdf', sep=''))
  }else if(figure_format=='png'){
    png(paste(filePath, '/Dept', dept,'.png', sep=''))
  }
  
  dfTrain2 <- subset(dfTrain, Dept==dept)
  # create scatter plot
  print(xyplot(Weekly_Sales~Day_Index|Store,
               data=dfTrain2, main=paste('Dept: ', dept, sep=''), as.table=TRUE,
               strip = strip.custom(strip.names = TRUE, strip.levels = TRUE),
               par.strip.text = list(cex = 0.75)))
  dev.off()

}

