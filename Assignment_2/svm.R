library(klaR)
library(caret)
library(ggplot2)

get_accuracy <- function(a,b,data_to_check,labels_to_check){
  
  calc_labels <- c()
  
  for(j in 1:nrow(data_to_check)){
    
    calc_val <-  ( as.matrix(data_to_check[j,]) %*% t(a) ) + b 
    if(calc_val > 0){
      calc_labels <- c(calc_labels, 1)
    }
    else{
    calc_labels <- c(calc_labels, -1)
    }
  }
  
  checker <- calc_labels == labels_to_check
  
  acc <- sum(checker) / length(labels_to_check)
  return(acc)
  
}


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
label[label ==  " <=50K" ] <- 1
label[label ==  " <=50K." ] <- 1
label[ label == " >50K"] <- -1
label[ label == " >50K."] <- -1 
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

reg_constants <- c(1,0.1,0.01,0.001)
num_epoch <- 50
batch_size <- 10
num_steps <- 300

#Output data
op_data <- data.frame(Counter=c(1:500))

for(reg in 1:length(reg_constants)){

a <- matrix(rnorm(6), nrow = 1, ncol = 6) #A
b <- rnorm(1) #B
main_acc <- c() 
for(epoch in 1:50){

#pick out 50 numbers
rand50 <- sample(1:nrow(train), 50 , replace = FALSE)

ho_set_features <- train[rand50,] #Held out set features
ho_set_labels <- train_label[rand50] # Held out set labels

actual_train_features <- train[-rand50,] #Actual training features
actual_train_labels <- train_label[-rand50] #Actual training labels 

n <- 1 / ( (0.01 * epoch) + 50 ) #Step length


for( i in 1:300 ){ #Steps = 300
  
  rand1 <- sample(1:nrow(actual_train_features),1,replace = FALSE)
  
  x <- as.matrix(actual_train_features[rand1,]) #Sample
  
  y <- as.numeric(actual_train_labels[rand1]) #Sample Label
  
  val <- y * ( ( x %*% t(a) ) + b )
  if(val >= 1){
    a <- a - (n * (reg_constants[reg] * a))
    b <- b
  }
  else{
    a <- a - (n * (reg_constants[reg]*a - (y * x)))
    b <- b - (n * -y)
  }
  
  if(i%%30 == 0){
    temp_acc <- get_accuracy(a,b,ho_set_features, ho_set_labels)
    main_acc <- c(main_acc, temp_acc) 
  }
}
}

op_data <- cbind(op_data, main_acc)

}
colnames(op_data) <- c("Counter","1","2","3","4")



