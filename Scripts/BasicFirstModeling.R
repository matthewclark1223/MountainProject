df<-read.csv("~/mtnProject/Data/RouteData.csv",stringsAsFactors = F)
library(dplyr)
library(lme4)
library(rstanarm)
dfsub<-df%>%filter(routes.type %in% c("Trad", "Sport", "Boulder", "TR"))
fit<-lm(routes.starVotes~routes.stars, data=dfsub)
summary(fit)

#try a heierarchical model, leave out the location for now
fit<-glmer(routes.starVotes~routes.stars+(1|routes.type)+(1|routes.pitches),family="poisson", data=dfsub)
summary(fit)

#now try with stars as a factor...this takes a long time
fit<-glmer(routes.starVotes~as.factor(routes.stars)+(1|routes.type)+(1|routes.pitches),family="poisson", data=dfsub)
summary(fit)

#try with a negative binomial distribution
fit2<-glmer.nb(routes.starVotes~routes.stars+(1|routes.type)+(1|routes.pitches), data=dfsub)

#now try with location as a random efect
fity<-stan_glmer(routes.starVotes~routes.stars+(1|routes.type)+(1|routes.pitches)+(1|routes.location),family="poisson", data=dfsub[1:300,])
summary(fity)


system.time(fit<-glmer.nb(routes.starVotes~routes.stars+(1|routes.type)+(1|routes.pitches), data=dfsub)
)


#go bayesian
options(mc.cores = parallel::detectCores())
options(mc.cores = 1)
fit<-stan_glmer(routes.starVotes~as.factor(routes.stars)+(1|routes.type)+
                  (1|routes.pitches),family="poisson", data=dfsub[1:200,])



system.time(fity<-glmer(routes.starVotes~routes.stars+(1|routes.type)+(1|routes.pitches)+(1|routes.location),family="poisson", data=dfsub))

summary(fity)



dfsub$Starz<-ifelse(dfsub$routes.stars %in% c("4.5","4.6","4.7","4.8","4.9","5"),"5",
                    ifesle(dfsub$routes.stars %in% c("4.4","4.3","4.3","4.2","4.1","4","3.9","3.8","3.7","3.6","3.5"),"4",
                         ifelse  ))
