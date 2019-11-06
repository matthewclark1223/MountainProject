library(tidyverse)
library(lme4)
library(rstanarm)
df<-read_csv("~/mtnProject/DataOutput/RouteDataCleaned.csv")
df<-df[,-c(1:2)]

options(mc.cores = parallel::detectCores())

##dont rerun!
fit<-stan_glmer.nb(routes.starVotes~Stars+(1|Type)+(1|Difficulty)+(1|Safety),
            data=df[1:1000,])

fitty<-stan_glmer.nb(routes.starVotes~Stars+(1|Type)+(1|Difficulty)+(1|Safety),
                   data=df[df$routes.starVotes>200,])
save(fitty, file="fittyApp.rda")
####dont rerun^^^
load(file="~/MtnProject/fittyApp.rda")
post<-(posterior_predict(
  fitty,data.frame(Stars=3,Type="Sport", Difficulty="Intermediate",
                 Safety="Safe"),draws=2000))

histy = hist(post,breaks=50,plot=FALSE) 
histy$density = histy$counts/sum(histy$counts)*100
#
n1 <- 25 
n2<-75
medv<-round(median(post),2)
minv<-min(post)
maxv<- max(post)

plot(histy, main=paste("Median Visitation Estimate = ", format(medv,big.mark = ",")),
     xlab="Number of Total Visitors", xlim=c(minv,maxv),ylab="Percentage of Samples",col="blue",freq=FALSE)
abline(v = median(post),
       col = "black",
       lwd = 3)

upbound1<-min(post[post > quantile(post,prob=1-n1/100),])

abline(v=upbound1,col="orange",lty=2,lwd=3)

lwrbound1<- ifelse(is.infinite( max(post[post<quantile(post,prob=1-n2/100),]))==TRUE,0,
                   max(post[post<quantile(post,prob=1-n2/100),]))  


abline(v=lwrbound1,col="orange",lty=2,lwd=3)


legend(x = "right", 
       c("Median",paste("50% Credibility Interval:",format(lwrbound1,big.mark=","), "-" ,format(upbound1,big.mark=","))),
       col =  c("black","orange"),lty=c(1,2), box.lty=8,
       lwd = c(2,2)
       )






