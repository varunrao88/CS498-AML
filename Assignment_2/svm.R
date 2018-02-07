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

#Coverting labels to +1 or -1
label <- as.character(label)
label[label ==  " <=50K" ] <- -1
label[label ==  " <=50K." ] <- -1
label[ label == " >50K"] <- 1
label[ label == " >50K."] <- 1 
label <- as.factor(label)

#For Unit Variance
mean_val <- apply(final_data,2,mean) 
sd <- apply(final_data,2,sd)

for(i in 1:length(mean_val)){
  final_data[,i] <- (final_data[,i] - mean_val[i])/sd[i]
}

#Train-Test-Val split
#Test -> only for final accuracy
#Val -> For regularizarion constant

sp <- createDataPartition(y = label, p = 0.8, list=FALSE)
train <- final_data[sp,] #Training Set
train_label <- label[sp] #Training Labels

temp_data <- final_data[-sp,]
temp <- label[-sp]
sp2 <- createDataPartition(y = temp, p= 0.5 , list=FALSE)
test <- final_data[sp2,] #Test Set
test_label <- label[sp2] #Test Labels

val <- temp_data[-sp2,] #Val Set
val_label <- temp_data[-sp2] # Val Labels







