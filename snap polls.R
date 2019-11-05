library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(rsconnect)
setwd("C:/Users/Vera/Desktop/FALL 2019/TRIP/WEB/SNAP POLLS")

###### read in snap polls ########
snap<-lapply(1:11, function(x) assign(paste("snap",x,sep="_"),read.csv(paste("TRIP_SnapPoll",x,"_1.0.0.csv",sep=""),na.strings="")))

#### reformat snap polls ####
names(snap[[10]])<-tolower(names(snap[[10]]))
levels(snap[[1]]$qg_47)<-c("Too little","About the right amount","Too much","Don't know")
levels(snap[[2]]$qg_75)<-c("I strongly oppose it","I somewhat oppose it","I neither support nor oppose it","I somewhat support it","I strongly support it","Don't know")
levels(snap[[3]]$qg_431)<-c("Very incapable","Incapable","Neither capable nor incapable","Capable","Very capable","Don't know")
levels(snap[[3]]$qg_142)<-c("Decreased","Kept at its present level","Increased","No opinion")
levels(snap[[3]]$qg_270)<-c("Much less capable","Somewhat less capable","Neither more nor less capable","Somewhat more capable","Much more capable","Don't know")
levels(snap[[4]]$qg_1680)<-c("Harmful","More harmful than helpful","Neither helpful nor harmful","More helpful than harmful","Helpful")
levels(snap[[4]]$qg_1692_10418)<-c("Strongly disagree","Somewhat disagree","Somewhat agree","Strongly agree")
levels(snap[[4]]$qg_1692_10422)<-c("Strongly disagree","Somewhat disagree","Somewhat agree","Strongly agree")
levels(snap[[6]]$qg_75)<-c("I strongly oppose it","I somewhat oppose it","I neither support nor oppose it","I somewhat support it","I strongly support it","Don't know")
levels(snap[[7]]$qg_1792)<-c("Decrease foreign aid contributions to developing countries","Maintain the current level of foreign aid contributions to developing countries","Increase foreign aid contributions to developing countries")
levels(snap[[7]]$qg_1805)<-c("Less than 1 percent of the national budget","Between 1-5 percent of the national budget","Between 5-10 percent of the national budget","Between 10-20 percent of the national budget","Between 20-50 percent of the national budget")
levels(snap[[8]]$qg_1856_11288)<-c("Never effective","Rarely effective","Mostly effective","Always effective")
levels(snap[[8]]$qg_1856_11290)<-c("Never effective","Rarely effective","Mostly effective","Always effective")
levels(snap[[8]]$qg_1856_11292)<-c("Never effective","Rarely effective","Mostly effective","Always effective")
levels(snap[[8]]$qg_1856_11294)<-c("Never effective","Rarely effective","Mostly effective","Always effective")
levels(snap[[8]]$qg_1856_11296)<-c("Never effective","Rarely effective","Mostly effective","Always effective")
levels(snap[[8]]$qg_1856_11297)<-c("Never effective","Rarely effective","Mostly effective","Always effective")
levels(snap[[8]]$qg_1856_11298)<-c("Never effective","Rarely effective","Mostly effective","Always effective")
levels(snap[[8]]$qg_1857_11299)<-c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree","Don't know")
levels(snap[[8]]$qg_1857_11301)<-c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree","Don't know")
levels(snap[[8]]$qg_1857_11303)<-c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree","Don't know")
levels(snap[[8]]$qg_1857_11305)<-c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree","Don't know")

commas<-lapply(1:length(snap[[10]]), function(x) names(snap[[10]][x])[length(grep(",",snap[[10]][[x]]))!=0])
commas<-c("qg_153","qg_151","q4486")
#### ####

#### read in codebooks ####
snap_cb<-lapply(1:11, function(x) assign(paste("snap",x,sep="_"),read.csv(paste("TRIP_SnapPoll",x,"_1.0.0_codebook.csv",sep=""),na.strings="")))

#######################################################
#                   USER INTERFACE                    #
#######################################################
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    .action-button{
                    padding:10px;
                    margin-top:-12px;
                    display:block;
                    background-color:#e1eaf0;
                    transition: background-color .3s, color .3s;
                    border-radius:5px;
                    }
                    .action-button:hover{
                    background-color:#edf2f5;
                    transition: background-color .3s, color .3s;
                    text-decoration:none;
                    }
                    .well{
                    background-color:#e1eaf0;
                    }
                    #return{
                    background-color:#f2f9fc;
                    transition: background-color .3s, color .3s;
                    border-color:#ffffff;
                    width:100%;
                    }
                    #return:hover{
                    background-color:#fafdff;
                    transition: background-color .3s, color .3s;
                    }
                    "))
    ),
  titlePanel(h1(strong("Snap Polls"))),
  tabsetPanel(id="Questions",
              tabPanel("Questions",
                       br(),
                       sidebarPanel(
                         selectInput("dataSelect","Choose Snap Poll:",
                                     choices=c("Snap Poll I: Syria, Ukraine, and the U.S. Defense Budget"=1,
                                               "Snap Poll II: Ukraine, Energy, and the Middle East"=2,
                                               "Snap Poll III: Seven Questions on Current Global Issues for IR Scholars"=3,
                                               "Snap Poll IV: Ten Questions on Current Global Issues for IR Scholars"=4,
                                               "Snap Poll V: Proposed Nuclear Agreement with Iran"=5,
                                               "Snap Poll VI: Greece, Migration Crisis, Trade Agreements, and FIFA"=6,
                                               "Snap Poll VII: South China Sea, Refugee Crisis, and Iran Deal"=7,
                                               "Snap Poll VIII: 2016 Presidential Campaign, Zika, and Terrorism in the Middle East"=8,
                                               "Snap Poll IX: U.S. Foreign Policy and the 2016 Presidential Election"=9,
                                               "Snap Poll X (Embedded in 2017 Faculty Survey)"=10,
                                               "Snap Poll XI: What Experts Make of Trump's Foreign Policy"=11))), # updates go here
                       mainPanel(br(),uiOutput("questions"),tableOutput("print"))),
              tabPanel("Graph",
                       fluidRow(column(9,
                                       strong(h3(textOutput("selectedQ"))),
                                       br())),
                       fluidRow(column(4,
                                       wellPanel(
                                         uiOutput("countryList"),br(),
                                         actionButton("return","Return to question list"))),
                                column(8,
                                       textOutput("error"),
                                       uiOutput("noGraph"),
                                       plotlyOutput("graph"),
                                       plotOutput("legend"))))),
  tags$style(type="text/css", ".shiny-output-error{visibility: hidden;}", ".shiny-output-error:before{visibility:hidden;}"))

#######################################################
#                       SERVER                        #
#######################################################
server <- function(input, output, session) {
  
  ###################### Q U E S T I O N S ######################
  qid<-reactive({snap_cb[[as.numeric(input$dataSelect)]]})
  
  dataStore<-reactiveValues(dataLoc=NULL,dataNum=NULL)
  
  ## list of questions ##
  output$questions<-renderUI({
    lapply(1:nrow(qid()), function(x) fluidRow(actionLink(paste0("btn_",x),qid()[,2][x]), br()))
  })
  
  observe({
    input_btn<-paste0("btn_", 1:nrow(qid()))
    lapply(input_btn, function(x) observeEvent(input[[x]],{
      i <- as.numeric(sub("btn_", "", x))
      dataStore$dataNum <- i
      dataStore$dataLoc<-strsplit(as.character(qid()[as.numeric(dataStore$dataNum),1]),":")
      updateTabsetPanel(session, "Questions", "Graph")
    }))
  })
  
  ## construct dataframe ##
  new_df<-function(x){
    y <- if(length(dataStore$dataLoc[[1]])==1 && dataStore$dataLoc[[1]][1] %in% commas){ #multiselect Qs
      z<-data.frame(snap[[x]] %>% select(response=dataStore$dataLoc[[1]][1]))
      z<-data.frame(response=unlist(strsplit(as.character(z$response), ",")))
      z<-z %>% group_by(response) %>% drop_na() %>% summarize(n=n()) %>% mutate(per=round(n/sum(n)*100,2))
      z
    } else if(length(dataStore$dataLoc[[1]])==1){
      data.frame(snap[[x]] %>% select(response=dataStore$dataLoc[[1]][1]) %>% filter(!is.na(response)) %>% drop_na() %>% mutate(total_n=n()) %>% group_by(response) %>% summarize(n=n()) %>% mutate(per=round(n/sum(n)*100,2)))
    }  else {
      data.frame(snap[[x]] %>% select(dataStore$dataLoc[[1]][1]:dataStore$dataLoc[[1]][2]) %>%
                   gather(key,value,dataStore$dataLoc[[1]][1]:dataStore$dataLoc[[1]][2]) %>%
                   select(response=value) %>% filter(!is.na(response)) %>% drop_na() %>% mutate(total_n=n()) %>% 
                   group_by(response) %>% summarize(n=n()) %>% mutate(per=round(n/sum(n)*100,2)))
    }
    return(y)
  }
  
  df<-reactive({
    df<-new_df(as.numeric(input$dataSelect))
    return(df)
  })
  
  ###################### G R A P H ######################
  ## create graph ##
  p<-reactive({
      p<-plot_ly(df(), x=~response, y=~per, colors="YlOrRd", type="bar",hoverinfo='text',text= ~paste(response,
                                                                                                 '<br>Percentage: ', per,'%',sep=""))
  })
  
  output$graph<-renderPlotly({
    p()
  })
  
  ## print error ##
  output$error<-renderText({
    str(dataStore$dataNum)
    str(dataStore$dataLoc)
  })
  
  ## return to questions list ##
  observeEvent(input$return, {
    updateTabsetPanel(session,"Questions","Questions")
  })
  
  ## print question as title ##
  output$selectedQ<-renderText({paste(qid()[,2][as.numeric(dataStore$dataNum)])})
  #### ####
}

shinyApp(ui, server)
