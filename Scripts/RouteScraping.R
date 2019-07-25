library(jsonlite)
x<-fromJSON("https://www.mountainproject.com/data/get-routes?routeIds=108146254&key=200315401-af9d88cac05395ab069c94bc2781893e")
x<-as.data.frame(x)
x<-(x[,-(c(9:13,16))])
x
106703081
110037673

x<-fromJSON("https://www.mountainproject.com/data/get-routes-for-lat-lon?lat=19.27&lon=-155.85&maxDistance=500&maxResults=500&key=200315401-af9d88cac05395ab069c94bc2781893e")
x<-as.data.frame(x)
x<-(x[,-(c(9:13,16))])
head(x)
unique(x$routes.location)
min(x$routes.id) #105000000
max(x$routes.id) #118000000
65000

for(i in 1:650){
  assign(paste("r", i, sep = ""), ((105000000)+((i-1)*200)): (((105000000)+((i-1)*200))+200))
}
for(i in 651:1300){
  assign(paste("r", i, sep = ""), ((105000000)+((i-1)*200)): (((105000000)+((i-1)*200))+200))
}

#This Works!

getRouteData<-function(routeID){
  y<-paste0("https://www.mountainproject.com/data/get-routes?routeIds=",routeID,"&key=200315401-af9d88cac05395ab069c94bc2781893e")
  y<-as.data.frame(fromJSON(y))
  y[,-(c(9:13,16))]
  
}


df <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(df) <- c("routes.id","routes.name","routes.type",
                  "routes.rating","routes.stars","routes.starVotes",
                  "routes.pitches","routes.location", 
                  "routes.longitude","routes.latitude" )

pb <- winProgressBar(title = "progress bar", min = 105759900,
                     max = total, width = 300)
for(i in 105759900:105759950){
  
  try(df<-rbind(getRouteData(i),df))
  setWinProgressBar(pb, i, title=paste( round(i/total*100, 105759900),
                                        "% done"))
}

df

#try using apply
library(pbapply)
library(parallel)
library(snow)
cl<-makeCluster(3,type="SOCK")
#turn off cluster
stopCluster(cl)

getRouteData<-function(routeID){
  y<-paste0("https://www.mountainproject.com/data/get-routes?routeIds=",routeID,"&key=200315401-af9d88cac05395ab069c94bc2781893e")
  y<-tryCatch(fromJSON(y), error=function(e) {})
  return(y)
}



z<-105759900:105759950

d<-Filter(Negate(is.null),(lapply(z, getRouteData)))

data.frame(matrix(unlist(d), nrow=16, byrow=T),stringsAsFactors=FALSE)
d<-as.data.frame(d)
d<-d[,-(c(9:13,16))]



#Try with all of them....

getRouteData<-function(routeID){
  y<-paste0("https://www.mountainproject.com/data/get-routes?routeIds=",routeID,"&key=200315401-af9d88cac05395ab069c94bc2781893e")
  y<-as.data.frame(fromJSON(y))
  y[,-(c(9:13,16))]
  
}


df <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(df) <- c("routes.id","routes.name","routes.type",
                  "routes.rating","routes.stars","routes.starVotes",
                  "routes.pitches","routes.location", 
                  "routes.longitude","routes.latitude" )
for(i in 105000000:118000000){
  
  try(df<-rbind(getRouteData(i),df))
}

write.csv(df,file="RouteData.csv")
