library(tidyverse)
library(lme4)
library(rstanarm)
library(leaflet)
library(rsconnect)
library(shinythemes)
#df<-read_csv("~/mtnProject/DataOutput/RouteDataCleaned.csv")
#df<-df[,-c(1:2)]
load(file="./fittyApp.rda")

ui<-shinyUI(navbarPage(theme=shinytheme("cerulean"),"Climbing Data Interactive Plot Demo",
                       tabPanel("Model Explorer",
                                fluidRow(column(12,
                                                h1("Model Explorer"),
                                                p("This application lets you explore the effects of various parameters on the expected number of people who have logged a specific climb."),
                                                br(),
                                                h4("Instructions"),
                                                p("Choose your input values"))),
                                hr(),
                                fluidRow(sidebarPanel(width = 3,
                                                      h4("Parameters"),
                                                      helpText("Choose the paremeters you would like to explore"),
                                                      selectInput(inputId = "difficulty",label = "Difficulty",choices=c("Beginner","Intermediate","Advanced","Elite")),
                                                      selectInput(inputId = "safety", label = "Safety",choices=c("Safe", "PG13" ,"R", "X" )), 
                                                      selectInput(inputId = "type", label = "Type",choices=c("Aid", "Boulder" ,"Sport", "TR","Trad" )), 
                                                      
                                                      sliderInput(inputId ="stars", label = "Stars", min = 0, step=1,max = 5, value = 3)),

                                         mainPanel(plotOutput("Forecast", height = 500))
                       )
                       
)))



server <- function(input, output) {
#Make Forecast Plots
output$Forecast<- renderPlot({
    
    post<-(posterior_predict(
        fitty,data.frame(Stars=input$stars,Type=input$"type", Difficulty=input$"difficulty",
                         Safety=input$"safety"),draws=2000))
    
    histy = hist(post,breaks=50,plot=FALSE) 
    histy$density = histy$counts/sum(histy$counts)*100
    #
    n1 <- 25 
    n2<-75
    medv<-round(median(post),2)
    minv<-min(post)
    maxv<- max(post)
    
    plot(histy, main=paste("Median Climber Estimate = ", format(medv,big.mark = ",")),
         xlab="Number of Total Climbers", xlim=c(minv,maxv),ylab="Percentage of Samples",col="#b2df8a",freq=FALSE)
    abline(v = median(post),
           col = "black",
           lwd = 3)
    
    upbound1<-min(post[post > quantile(post,prob=1-n1/100),])
    
    abline(v=upbound1,col="#1f78b4",lty=2,lwd=3)
    
    lwrbound1<- ifelse(is.infinite( max(post[post<quantile(post,prob=1-n2/100),]))==TRUE,0,
                       max(post[post<quantile(post,prob=1-n2/100),]))  
    
    
    abline(v=lwrbound1,col="#1f78b4",lty=2,lwd=3)
    
    
    legend(x = "topright", 
           c("Median",paste("50% Credibility Interval:",format(lwrbound1,big.mark=","), "-" ,format(upbound1,big.mark=","))),
           col =  c("black","#1f78b4"),lty=c(1,2), box.lty=8,
           lwd = c(2,2)
    )
    
    
    
})
}



# Run the application 
shinyApp(ui = ui, server = server)

