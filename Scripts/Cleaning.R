library(dplyr)
df<-read.csv("~/mtnProject/Data/RouteData.csv")

df$routes.type<-as.character(df$routes.type)
df$routes.type<-ifelse(grepl("Aid", df$routes.type), "Aid",
                ifelse(grepl("Ice", df$routes.type)|grepl("Snow", df$routes.type)|grepl("Mixed", df$routes.type),"Ice",
                ifelse(grepl("Boulder",df$routes.type),"Boulder",
                ifelse(grepl("Trad",df$routes.type),"Trad",
                ifelse(grepl("Sport", df$routes.type),"Sport",
                ifelse(grepl("TR", df$routes.type),"TR","XXX"))))))

#remove routes with no Type and remove ice routes
df<-df %>% filter(routes.type!="XXX")%>% filter(routes.type!="Ice")

#do the dificulty now
unique(df$routes.rating)
df[df$routes.rating=="5.1",]

df$difficulty<-#elite
  ifelse(grepl("5.13", df$routes.rating)|
           grepl("5.14", df$routes.rating)|
           grepl("5.15", df$routes.rating)|
           grepl("V10", df$routes.rating)|
           grepl("V11", df$routes.rating)|
           grepl("V12", df$routes.rating)|
           grepl("V13", df$routes.rating)|
           grepl("V14", df$routes.rating)|
           grepl("V15", df$routes.rating)|
           grepl("V16", df$routes.rating)|
           grepl("V17", df$routes.rating)|
           grepl("C4", df$routes.rating)|
           grepl("C5", df$routes.rating)|
           grepl("A4", df$routes.rating)|
           grepl("A5", df$routes.rating)|
           grepl("A6", df$routes.rating),"Elite",
         #advanced
         ifelse(grepl("5.11", df$routes.rating)|
                  grepl("5.12", df$routes.rating)|
                  grepl("V6", df$routes.rating)|
                  grepl("V7", df$routes.rating)|
                  grepl("V8", df$routes.rating)|
                  grepl("V9", df$routes.rating)|
                  grepl("A3", df$routes.rating)|
                  grepl("C3", df$routes.rating),"Advanced",
                
         
                #now for intermediate
                
                ifelse(grepl("5.8", df$routes.rating)|
                         grepl("5.9", df$routes.rating)|
                         grepl("5.10", df$routes.rating)|
                         grepl("V3", df$routes.rating)|
                         grepl("V4", df$routes.rating)|
                         grepl("V5", df$routes.rating)|
                         grepl("A2", df$routes.rating)|
                         grepl("C2", df$routes.rating),"Intermediate",
  
  
  
  #beginner
  ifelse(grepl("5.0", df$routes.rating)|
           grepl("5.1", df$routes.rating)|
                        grepl("5.2", df$routes.rating)|
                        grepl("5.3", df$routes.rating)|
                        grepl("5.4", df$routes.rating)|
                        grepl("5.5", df$routes.rating)|
                        grepl("5.6", df$routes.rating)|
                        grepl("5.7", df$routes.rating)|
                        grepl("A0", df$routes.rating)|
                        grepl("A1", df$routes.rating)|
                        grepl("C0", df$routes.rating)|
                        grepl("C1", df$routes.rating)|
                        grepl("VB", df$routes.rating)|
                        grepl("V0", df$routes.rating)|
                        grepl("V1", df$routes.rating)|
                        grepl("V2", df$routes.rating)|
                        grepl("2nd", df$routes.rating)|
                        grepl("3rd", df$routes.rating)|
                        grepl("4th", df$routes.rating)|
                        grepl("Easy", df$routes.rating)|
                        grepl("easy", df$routes.rating)|
                        grepl("V2", df$routes.rating), "Beginner","XXX"))))
                      
df<-df %>% filter(routes.rating!="XXX") 

#add safety scale

df$safety<-ifelse(grepl("PG", df$routes.rating), "PG13",
                  ifelse(grepl("R",df$routes.rating ),"R",
                  ifelse(grepl("X",df$routes.rating ),"X", "Safe")))
             

              
#Make a better star category
df$Stars<-ifelse(df$routes.stars %in% c("4.5","4.6","4.7","4.8","4.9","5"),"5",
             ifelse(df$routes.stars %in% c("4.4","4.3","4.3","4.2","4.1","4","3.9","3.8","3.7","3.6","3.5"),"4",
             ifelse(df$routes.stars %in% c("3.4","3.3","3.3","3.2","3.1","3","2.9","2.8","2.7","2.6","2.5"),"3",
             ifelse(df$routes.stars %in% c("2.4","2.3","2.3","2.2","2.1","2","1.9","1.8","1.7","1.6","1.5"),"2",  
             ifelse(df$routes.stars %in% c("1.4","1.3","1.3","1.2","1.1","1","0.9","0.8","0.7","0.6","0.5"),"1",
             ifelse(df$routes.stars %in% c("0.4","0.3","0.3","0.2","0.1","0"),"0","XXX"))))))

#Create a cleaned CSV
dir.create("~/MtnProject/DataOutput")
write.csv(df,"~/MtnProject/DataOutput/RouteDataCleaned.csv")

                   
                                          
