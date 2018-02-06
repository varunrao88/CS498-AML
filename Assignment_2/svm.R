library(klaR)
library(caret)

#Data Merge
ad <- read.csv("adult.data", header=FALSE) #Adult Data
adt <- read.csv("adult.test", header=FALSE) #Adult Data Test
adt <- adt[-1,] #Removing irrelevant line from test dataset

ad <- rbind(ad,adt) #Merging results

#Data Prep
final_data <- ad[,c(1,3,5,11,12,13)] #Only select continous variables
final_data[,1] <- as.numeric(final_data[,1]) #Converting column 1 to numeric
label <- ad[,15]

#For Unit Variance
mean_val <- apply(final_data,2,mean) 
sd <- apply(final_data,2,sd)

for(i in 1:length(mean_val)){
  final_data[,i] <- (final_data[,i] - mean_val[i])/sd[i]
}
