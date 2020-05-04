#basic setting
library(car)
library(dplyr)
setwd('C:/Users/Jooeun Kim/Desktop/ESC/final project')
crime <- read.csv("imp_data.csv")
crime <- crime %>%
  select(-communityname, -state, -index, -X)
crime<-crime[-which(crime$violentPerPop==0),] 
attach(crime)
y <- violentPerPop

#checking skewness
x = 0
for(i in 1:ncol(crime)){
  x = x+1
  hist(crime[,i], main=names(crime)[i], xlab=x)
  if(i > 50){
    break
  }
}

#zero imputation
x <- rep(0,ncol(crime))
for(j in 1:ncol(crime)){
  for(i in 1:nrow(crime)){
    if(crime[i,j]==0){
      x[j] = x[j]+1
    }
  }
}
x[1:50]
for(j in 1:ncol(crime)){
  for(i in 1:nrow(crime)){
    if(j==11){
    }
    else if(j==12){
    }
    else if(crime[i,j]==0){
      crime[i,j] <- 0.01
    }
  }
}


#Box-Cox transformation powers
tr <- c(1,3,4,5,6,11,15,18,20,21,22,23,24,25,26,28,29,30,45,49,50) 
powers <- rep(0,length(tr))
power.set <- crime[,c(1:50)]
x = 0
for(i in tr){
  Hl <- fivenum(crime[,i])[2]
  M <- fivenum(crime[,i])[3]
  Hu <- fivenum(crime[,i])[4]
  power <- 1-2*M*(Hu-M+Hl-M)/((Hl-M)^2+(Hu-M)^2)
  x = x+1
  powers[x] <- power
  if(power<0.45 & power>0){
    colnames(power.set)[i] <- paste(names(crime)[i],'log',sep='.')
    power.set[,i] <- log(crime[,i])
  }
  if(power<0){
    colnames(power.set)[i] <- paste(names(crime)[i],'power',sep='.')
    power.set[,i] <- -1*(crime[,i]**power)
    }
  if(power>=0.45 & power <= 0.55){
    colnames(power.set)[i] <- paste(names(crime)[i],'sqrt',sep='.')
    power.set[,i] <- sqrt(crime[,i])
    }
  if(power>0.55){
    colnames(power.set)[i] <- paste(names(crime)[i],'power',sep='.')
    power.set[,i] <- crime[,i]**power
  }
}
power.var <- as.data.frame(power.set)
attach(power.var)
write.csv(power.var,file='transform1.csv')
