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
test <- temp_data[sp2,] #Test Set
test_label <- temp[sp2] #Test Labels

validation_set <- temp_data[-sp2,] #Val Set
validation_set_label <- temp[-sp2] # Val Labels

reg_constants <- c(1,0.1,0.01,0.001)
num_epoch <- 50
batch_size <- 10
num_steps <- 300

#Output data
op_data <- data.frame(Counter=c(1:500))
op_a <- data.frame(Counter=c(1:500))

for(reg in 1:length(reg_constants))
{
  
  a <- matrix(rnorm(6), nrow = 1, ncol = 6) #A
  b <- rnorm(1) #B
  main_acc <- c() 
  main_mag <- c()
  for(epoch in 1:num_epoch){
    
    #pick out 50 numbers
    rand50 <- sample(1:nrow(train), 50 , replace = FALSE)
    
    ho_set_features <- train[rand50,] #Held out set features
    ho_set_labels <- train_label[rand50] # Held out set labels
    
    actual_train_features <- train[-rand50,] #Actual training features
    actual_train_labels <- train_label[-rand50] #Actual training labels 
    
    n <- 1 / ( (0.01 * epoch) + 50 ) #Step length
    
    
    for( i in 1:num_steps ){ #Steps = 300
      
      rand10 <- sample(1:nrow(actual_train_features),batch_size,replace = FALSE)
      
      x10 <- actual_train_features[rand10,] #Sample
      
      y10 <- actual_train_labels[rand10] #Sample Label
      
      ca <- c()
      cb <- c()
      for(s_b in 1:batch_size){
        val <- as.numeric(as.character(y10[s_b])) * ( ( as.matrix(x10[s_b,]) %*% t(a) ) + b )
        if(val >= 1){
          # ca <- a - (n * (reg_constants[reg] * a))
          # b <- b
          ca <- c(ca, reg_constants[reg] * a)
          cb <- c(cb,0)
        }
        else{
          # a <- a - (n * (reg_constants[reg]*a - (y * x)))
          # b <- b - (n * -y)
          ca <- c(ca, (reg_constants[reg]*a - (  as.numeric(as.character(y10[s_b])) * as.matrix(x10[s_b,]))) )
          cb <- c(cb, - as.numeric(as.character(y10[s_b])))
        }  
      }
      a <- a - n * mean(ca)
      b <- b - n * mean(cb)
      
      
      if(i%%30 == 0){
        temp_acc <- get_accuracy(a,b,ho_set_features, ho_set_labels)
        main_acc <- c(main_acc, temp_acc) 
        main_mag <- c(main_mag, norm(a))
      }
    }
  }
  
  cat("Accuracy for ", reg_constants[reg] , ":"  , get_accuracy(a,b,validation_set,validation_set_label), "\n", sep= " ")
  
  op_data <- cbind(op_data, main_acc)
  op_a <- cbind(op_a,main_mag)
  
}
colnames(op_data) <- c("Counter","V1","V2","V3","V4")
colnames(op_a) <- c("Counter","V1","V2","V3","V4")





# ggplot(data = op_data) + geom_line(aes(x = Counter, y=V1)) + geom_line(aes(x = Counter, y=V2)) + geom_line(aes(x = Counter, y=V3 )) + geom_line(aes(x = Counter, y=V4)) + scale_y_continuous(breaks = seq(0,1,0.1))
# ggplot(data = op_a) + geom_line(aes(x = Counter, y=V1), col="blue") + 
# geom_line(aes(x = Counter, y=V2)) + geom_line(aes(x = Counter, y=V3 )) +
# geom_line(aes(x = Counter, y=V4)) + scale_y_continuous(breaks = seq(0,1,0.1))
