library(klaR)
library(caret)
prob_density <- function(features, mean_values, sd_values, class_val, total_val){
  density_list <- c()
  for(i in 1:nrow(features)){
    row_val <- c()
    for(j in 1:ncol(features)){
      if(is.na(features[i,j]))
      {
        next
      }
      row_val[j]  <- dnorm(features[i,j], mean = mean_values[j], sd = sd_values[j], log = TRUE)
    }
    density_list[i] = sum(row_val,na.rm = TRUE)  + log(class_val/(total_val))
  }
  density_list
}

# Extract the data
wdat<-read.csv('pima-indians-diabetes.data', header=FALSE)

#Replace 0 to NA in columns V3,v4,v6,v8
wdat$V3[wdat$V3 == 0] <- NA
wdat$V4[wdat$V4 == 0] <- NA
wdat$V6[wdat$V6 == 0] <- NA
wdat$V8[wdat$V8 == 0] <- NA

#Get the features and label
bigx<-wdat[,-c(9)]
bigy<-wdat[,9]
  
#Training and Test Scores
trscore<-array(dim=10)
tescore<-array(dim=10)

for (wi in 1:10)
{
  #Split into Training and Test
  wtd<-createDataPartition(y=bigy, p=0.8, list=FALSE)
  nbx<-bigx
  
  #Training features and labels
  ntrbx<-nbx[wtd, ]
  ntrby<-bigy[wtd]
  
  #Splitting into classes
  trposflag<-ntrby>0
  ptregs<-ntrbx[trposflag, ] #Positive
  ntregs<-ntrbx[!trposflag,] #Negative
  
  #Test Data 
  ntebx<-nbx[-wtd, ]
  nteby<-bigy[-wtd]
  
  #Calculate Mean and Standard Deviation
  ptrmean<-sapply(ptregs, mean, na.rm=TRUE)
  ntrmean<-sapply(ntregs, mean, na.rm=TRUE)
  
  ptrsd<-sapply(ptregs, sd, na.rm=TRUE)
  ntrsd<-sapply(ntregs, sd, na.rm=TRUE)
  
  py1 <- sum(trposflag)
  py0 <- nrow(ntregs)
  total_val <- py1+py0 
  #For Positive
  pos <- c()
  pos <- prob_density(ntrbx, ptrmean, ptrsd, py1, total_val)
  
  #For Negative
  neg <- c()
  neg <- prob_density(ntrbx, ntrmean, ntrsd, py0, total_val)
  
  # Check scores
  lvwtr<-pos > neg
  gotrighttr<-lvwtr==ntrby
  trscore[wi]<-sum(gotrighttr)/(sum(gotrighttr)+sum(!gotrighttr))
  
  
  # Check on test set
  py0 <- py1 <- total_val <- 0
  py1 <- sum(nteby > 0)
  py0 <- length(nteby) - py1 
  total_val <- py0+py1
  
  pos <- c()
  pos <- prob_density(ntebx, ptrmean, ptrsd, py1, total_val)
  
  #For Negative
  neg <- c()
  neg <- prob_density(ntebx, ntrmean, ntrsd, py0, total_val)
  
  lvwte<-pos>neg
  gotright<-lvwte==nteby
  tescore[wi]<-sum(gotright)/(sum(gotright)+sum(!gotright))
  
}
print(mean(tescore))

