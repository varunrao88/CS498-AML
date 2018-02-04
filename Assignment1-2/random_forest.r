#Random Forest
calc_rf <- function(ntree , depth){

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

remove_blanks <- function(oneimg){
  
  oneimg[oneimg >= 128 ] <- 255
  oneimg[oneimg < 128 ] <- 0
  
  image_matrix <- matrix(oneimg, nrow = 28, ncol = 28)
  pos_row <- apply(image_matrix,1,sum)
  pos_row <- pos_row > 0
  
  pos_col <- apply(image_matrix,2,sum)
  pos_col <- pos_col > 0
  
  v <- c()
  for(i in 1:length(pos_row)){
    if(!pos_row[i]){
      v <- c(v,-i)
    }
  }
  image_matrix <- image_matrix[v,]
  
  v <- c()
  for(i in 1:length(pos_col)){
    if(!pos_col[i]){
      v <- c(v,-i)
    }
  }
  image_matrix <- image_matrix[,v]
}

re_wrap <- function(oneimg){
  return(as.matrix(resize(as.cimg(oneimg), size_x = 20, size_y = 20)))
}

#Training Data
trd <- load_mnist("train")

#Testing Data
ted <- load_mnist("test")

tr_bb <- apply(trd$x, 1, remove_blanks)
te_bb <- apply(ted$x, 1, remove_blanks)

tr_bb20 <- lapply(tr_bb, re_wrap)
te_bb20 <- lapply(te_bb, re_wrap)

to_mat <- lapply(tr_bb20, function(x){c(x)})
to_mat_test <- lapply(te_bb20, function(x){c(x)})

mat400 <- do.call(rbind,to_mat)
mat400test <- do.call(rbind,to_mat_test)

trdf <- as.data.frame(mat400)
tedf <- as.data.frame(mat400test)

rtrdf <- trdf
rtrdf <- cbind(rtrdf, Label = as.factor(trd$y))

rtedf <- tedf
rtedf <- cbind(rtedf, Label = as.factor(ted$y))

h2o.init()
rf <- h2o.randomForest(y = "Label", training_frame = as.h2o(rtrdf) ,ntrees = ntree,  max_depth = depth)

predictions<-as.data.frame(h2o.predict(rf,as.h2o(rtedf)))

sum(predictions$predict == ted$y)/length(ted$y)
}

pred1 <- calc_rf(10,4)
pred2 <- calc_rf(10,8)
pred3 <- calc_rf(10,16)
pred4 <- calc_rf(20,4)
pred5 <- calc_rf(20,8)
pred6 <- calc_rf(20,16)
pred7 <- calc_rf(30,4)
pred8 <- calc_rf(30,8)
pred9 <- calc_rf(30,16)





