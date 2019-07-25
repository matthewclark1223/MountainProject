library(jsonlite)

#test the API
x<-fromJSON("https://www.mountainproject.com/data/get-routes-for-lat-lon?lat=19.27&lon=-155.85&maxDistance=500&maxResults=500&key=200315401-af9d88cac05395ab069c94bc2781893e")
x<-as.data.frame(x)
x<-(x[,-(c(9:13,16))])
head(x)


#create function to get the data based on lat/long input
getRouteData<-function(Latt,Lonn){
  y<-paste0("https://www.mountainproject.com/data/get-routes-for-lat-lon?lat=",
            Latt,
            "&lon=",
            Lonn,
            "&maxDistance=500&maxResults=500&key=200315401-af9d88cac05395ab069c94bc2781893e")
  y<-as.data.frame(fromJSON(y))
  y[,-(c(9:13,16))]
  
}

#Create empty dataframe to fill with loop
df <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(df) <- c("routes.id","routes.name","routes.type",
                  "routes.rating","routes.stars","routes.starVotes",
                  "routes.pitches","routes.location", 
                  "routes.longitude","routes.latitude" )


#Set lat long specifications for the loop to run through
#limits represent the N/S & E/W limits of the US
lat<-seq(26.5, 48.00, by=0.5)
long<-seq(-123.00,-67.5, by=0.5)

#For all lat/long combinations, scrape 200 routes and 
#add them to the empty df we created above
for(i in lat){
  for(j in long){
    try(df<-rbind(getRouteData(Latt=i,Lonn=j),df))
    
  
  }
}

#Remove duplicate routes
library(dplyr)
df2<-df[!duplicated(df$routes.id),]
dd<-as.data.frame(df2)
dd$routes.location<-as.character(dd$routes.location)

#save csv
write.csv(dd,file="RouteData.csv")

