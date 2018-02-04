#Random Forest
calc_rf_untouched <- function(ntree , depth){
  
  library(imager)
  library(caret)
  library(quanteda)
  library(h2o)
  
  load_mnist <- function(sel) {
    load_image_file <- function(filename) {
      ret = list()
      f = file(filename,'rb')
      readBin(f,'integer',n=1,size=4,endian='big')
      ret$n = readBin(f,'integer',n=1,size=4,endian='big')
      nrow = readBin(f,'integer',n=1,size=4,endian='big')
      ncol = readBin(f,'integer',n=1,size=4,endian='big')
      x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
      x[x >= 128] <- 255
      x[x < 128] <- 0
      ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
      close(f)
      ret
    }
    load_label_file <- function(filename) {
      f = file(filename,'rb')
      readBin(f,'integer',n=1,size=4,endian='big')
      n = readBin(f,'integer',n=1,size=4,endian='big')
      y = readBin(f,'integer',n=n,size=1,signed=F)
      close(f)
      y
    }
    
    if(sel == "train"){
      train <- load_image_file('train-images.idx3-ubyte')  
      train$y <- load_label_file('train-labels.idx1-ubyte')
      return(train)
    }
    else{
      test <- load_image_file('t10k-images.idx3-ubyte')
      test$y <- load_label_file('t10k-labels.idx1-ubyte')  
      return(test)
    }
  }
  
  #Training Data
  trd <- load_mnist("train")
  
  #Testing Data
  ted <- load_mnist("test")
  
  trdf <- as.data.frame(trd$x)
  colnames(trdf) <- paste("V",1:784,sep="")
  tedf <- as.data.frame(ted$x)
  colnames(tedf) <- paste("V",1:784,sep="")
  
  rtrdf <- trdf
  rtrdf <- cbind(rtrdf, Label = as.factor(trd$y))
  
  rtedf <- tedf
  rtedf <- cbind(rtedf, Label = as.factor(ted$y))
  
  h2o.init()
  rf <- h2o.randomForest(y = "Label", training_frame = as.h2o(rtrdf) ,ntrees = ntree,  max_depth = depth)
  
  predictions<-as.data.frame(h2o.predict(rf,as.h2o(rtedf)))
  
  print(sum(predictions$predict == ted$y)/length(ted$y))
}








