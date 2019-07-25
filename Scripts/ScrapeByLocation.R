library(jsonlite)
x<-fromJSON("https://www.mountainproject.com/data/get-routes-for-lat-lon?lat=19.27&lon=-155.85&maxDistance=500&maxResults=500&key=200315401-af9d88cac05395ab069c94bc2781893e")
x<-as.data.frame(x)
x<-(x[,-(c(9:13,16))])
head(x)



getRouteData<-function(Latt,Lonn){
  y<-paste0("https://www.mountainproject.com/data/get-routes-for-lat-lon?lat=",
            Latt,
            "&lon=",
            Lonn,
            "&maxDistance=500&maxResults=500&key=200315401-af9d88cac05395ab069c94bc2781893e")
  y<-as.data.frame(fromJSON(y))
  y[,-(c(9:13,16))]
  
}


df <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(df) <- c("routes.id","routes.name","routes.type",
                  "routes.rating","routes.stars","routes.starVotes",
                  "routes.pitches","routes.location", 
                  "routes.longitude","routes.latitude" )

df<-rbind(getRouteData(Latt=39.85,Lonn=-117.24),df)


lat<-seq(26.5, 48.00, by=0.5)
long<-seq(-123.00,-67.5, by=0.5)

for(i in lat){
  for(j in long){
    try(df<-rbind(getRouteData(Latt=i,Lonn=j),df))
    
  
  }
}
library(dplyr)
df2<-df[!duplicated(df$routes.id),]
dd<-as.data.frame(df2)
dd$routes.location<-as.character(dd$routes.location)
write.csv(dd,file="RouteData.csv")
plot(dd$routes.stars, dd$routes.starVotes)


for (i in 1:10){
  print(typeof(dd[[i]]))
}
as.character(head(dd)$routes.location)

dd_sub<-filter(dd, routes.type %in% c("Trad","Sport","Boulder"))

ggplot(dd_sub, aes(x=routes.stars, y=routes.starVotes, color=routes.type))+
  geom_point(alpha=0.6)+
  geom_smooth(method="lm")+
  scale_color_manual(values=c("#e41a1c","#377eb8","#4daf4a"))+
  theme_bw()
