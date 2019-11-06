library(tidyverse)
library(lme4)
library(rstanarm)
df<-read_csv("~/mtnProject/DataOutput/RouteDataCleaned.csv")
df<-df[,-c(1:2)]

#create a standardize function which puts each variable on the scale fo a caegorical variable
stdize<-function(x) {(x-mean(x))/(2*sd(x))}


#make sure factors vs characters are being handled properly
df$Stars<-factor(df$Stars, levels=c("0","1","2","3","4","5"))

df$Safety<-factor(df$Safety, levels=c("Safe" ,"PG13", "R" , "X" ))

df$Difficulty<-factor(df$Difficulty, levels=c("Beginner" ,"Intermediate", "Advanced" , "Elite" ))

#based on plots, we need different intercepts for safety
#different intercepts and slopes for Type


#now try with location as a random efect
fit<-glmer(routes.starVotes~Stars+(1|Type)+(1|Difficulty)+(1|routes.location)+(1|Safety),
           family="poisson", data=df)
summary(fit)

#go bayesian
options(mc.cores = parallel::detectCores())

fitty<-stan_glmer(routes.starVotes~Stars+(1|Type)+(1|Difficulty)+(1|routes.location)+(1|Safety),
      family="poisson", data=df[c(1:200),])

fitty<-glmer(routes.starVotes~Stars+(Stars|Type)+(1|Difficulty)+(1|Safety),
                  family="poisson", data=df[sample(nrow(df),1000),])
#aic ^^ 40389.8

fitty<-glmer.nb(routes.starVotes~Stars+(1|Type)+(1|Difficulty)+(1|routes.location), data=df[1:5000,])
#The above model gives an error and suggests to rescale variables...try rescaling them


fitty<-stan_glmer.nb(routes.starVotes~Stars+(1|Type)+
                  (1|Difficulty)+(1|routes.location), data=df[1:2000,])
#ran this ^^ and left, should work!
save(fitty, file="fitty.rda")


####

fitty<-stan_glmer.nb(routes.starVotes~Stars+(1|Type)+
                       (1|Difficulty)+(1|routes.location), data=df[1:10000,])

#left this!^
