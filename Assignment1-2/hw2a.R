library(imager)
library(caret)
library(quanteda)

load_mnist <- function(sel) {
  load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    ret$n = readBin(f,'integer',n=1,size=4,endian='big')
    nrow = readBin(f,'integer',n=1,size=4,endian='big')
    ncol = readBin(f,'integer',n=1,size=4,endian='big')
    x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
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
tedf <- as.data.frame(ted$x)

model <- train(trdf, as.factor(trd$y), 'naive_bayes')
teclasses<-predict(model,newdata=tedf)
conf_mat <- confusionMatrix(data=teclasses, ted$y)

model_bern <- textmodel_nb(as.dfm(trd$x), trd$y, prior = "docfreq", distribution = "Bernoulli")
pred_bern <- predict(model_bern, newdata = as.dfm(ted$x))

