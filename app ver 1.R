library(shiny)
library(dplyr)
library(skimr)
library(tidyr)
library(ggplot2)
library(plotly)
library(rsconnect)


#######################################################
#                   USER INTERFACE                    #
#######################################################
ui <- fluidPage(
  titlePanel(strong("Faculty Survey")),
    tabsetPanel(id="Questions",
                 tabPanel("Questions",
                          br(),
                          sidebarPanel(
                            selectInput("dataSelect","Choose Survey Year:",choices=c("2004 Faculty Survey"=3,"2006 Faculty Survey"=4,"2008 Faculty Survey"=5,"2011 Faculty Survey"=6,"2014 Faculty Survey"=7,"2017 Faculty Survey"=8,"All years (selected questions)"=1))), # updates go here
                          mainPanel(uiOutput("questions"),textOutput("print"))),
                 tabPanel("Graph",
                          br(),
                          sidebarPanel(
                            selectInput("test","test",c(1:10)),
                            actionButton("return","Return to question list")),
                          mainPanel(plotOutput("graph")))),
  tags$style(type="text/css", ".shiny-output-error{visibility: hidden;}", ".shiny-output-error:before{visibility:hidden;}"))

#######################################################
#                       SERVER                        #
#######################################################
server <- function(input, output, session) {

###################### Q U E S T I O N S ######################
  qid<-reactive({qid_overtime[qid_overtime[as.numeric(input$dataSelect)]!="",c(1,as.numeric(input$dataSelect))]})
  
  ## list of questions ##
  output$questions<-renderUI({
    lapply(1:10, function(x) fluidRow(actionLink(paste0("btn_",x),qid()[,1][x]), br()))
  })
  
  dataStore<-reactiveValues(dataLoc=NULL,dataNum=NULL)
  observe({
    input_btn<-paste0("btn_", 1:10)
    lapply(input_btn, function(x) observeEvent(input[[x]],{
      i <- as.numeric(sub("btn_", "", x))
      dataStore$dataNum <- i
      dataStore$dataLoc<-paste(unlist(strsplit(qid()[as.numeric(dataStore$dataNum),2],":")),collapse="|")
      updateTabsetPanel(session, "Questions","Graph")}))
  })
  
  output$print<-renderText({paste(dataStore$dataLoc)})
  
  df<-reactive({data.frame(ifelse(input$dataSelect==3, df<-new_surveys[1],
                           ifelse(input$dataSelect==4, df<-new_surveys[2],
                           ifelse(input$dataSelect==5, df<-new_surveys[3],
                           ifelse(input$dataSelect==6, df<-new_surveys[4],
                           ifelse(input$dataSelect==7, df<-new_surveys[5],
                           ifelse(input$dataSelect==8, df<-new_surveys[6])))))))
  })

######################  G R A P H ######################
  
  ## create graph ##
  output$graph<-renderPlot({
    ggplot(data=df(),aes_string(x=df()[[grep(dataStore$dataLoc, names(df()))]])) +
      geom_bar(stat="count",fill="#ffbfd7") +
      theme(axis.text.x=element_text(angle=45)) + 
      theme_bw()
  })
  
  ## return to questions list ##
  observeEvent(input$return, {
    updateTabsetPanel(session,"Questions","Questions")
  })
}


shinyApp(ui, server)
