library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(rsconnect)
setwd("C:/Users/Vera/Desktop/TRIP/WEB/SNAP POLLS")

####### create snap poll x ########
fac17<-read.csv("TRIP_FacultySurvey_2017_US_Clean_1.1.csv",stringsAsFactor=FALSE)
names(fac17)<-tolower(names(fac17))
Qs<-read.csv("TRIP_FS2017_US_SnapPoll_Listofquestions.csv",stringsAsFactors=FALSE)
Qs$Question<-tolower(Qs$Question)
i<-grep(paste0(Qs$Question,collapse="|"),names(fac17))
snap_10<-cbind(fac17[3],fac17[i])
#names(snap_10)[names(snap_10) %in% Qs$Question]<-Qs$Question_text

write.csv(snap_10,"TRIP_SnapPoll10_1.0.0.csv",fileEncoding="UTF-8")

###### read in snap polls ########
### should read in individually, as needed ###
i<-1
for(i in 1:10){
  assign(paste("snap",i,sep="_"),read.csv(paste("TRIP_SnapPoll",i,"_1.0.0.csv",sep=""),stringsAsFactors=FALSE))
  i<-i+1
}

#######################################################
#                   USER INTERFACE                    #
#######################################################
ui <- fluidPage(
  titlePanel("Snap Poll (testing ver.)"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("dataSelect","Choose snap poll..",choices=c("Snap Poll I: Syria, Ukraine, and the U.S. Defense Budget"="snap_1",
                                                    "Snap Poll II: Ukraine, Energy, and the Middle East"="snap_2",
                                                    "Snap Poll III: Seven Questions on Current Global Issues for IR Scholars"="snap_3",
                                                    "Snap Poll IV: Ten Questions on Current Global Issues for IR Scholars"="snap_4",
                                                    "Snap Poll V: Proposed Nuclear Agreement with Iran"="snap_5",
                                                    "Snap Poll VI: Greece, Migration Crisis, Trade Agreements, and FIFA"="snap_6",
                                                    "Snap Poll VII: South China Sea, Refugee Crisis, and Iran Deal"="snap_7",
                                                    "Snap Poll VIII: 2016 Presidential Campaign, Zika, and Terrorism in the Middle East"="snap_8",
                                                    "Snap Poll IX: U.S. Foreign Policy and the 2016 Presidential Election"="snap_9",
                                                    "Snap Poll X (Embedded in 2017 Faculty Survey)"="snap_10"))), # updates go here
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Questions",tableOutput("questions")),
                  tabPanel("Graph",plotOutput("graph"))))),
  tags$style(type="text/css", ".shiny-output-error{visibility: hidden;}", ".shiny-output-error:before{visibility:hidden;}")
)

#######################################################
#                       SERVER                        #
#######################################################
server <- function(input, output, session) {
  observeEvent(input$dataSelect, {
    if(input$dataSelect=="Select..."){
    }
  })
  ## create graph ##
  output$graph<-renderPlot({
    ggplot(get(input$dataSelect),aes(x=gender)) +
       geom_bar(stat="count",fill="#ffbfd7") +
       theme(axis.text.x=element_text(angle=45)) + 
       theme_bw()
  })
  ## list of questions ##
  output$questions<-renderTable({
    head(get(input$dataSelect))[1:5]
    #head(Qs$Question_text)[1:5]
  })
}

shinyApp(ui = ui, server = server)

