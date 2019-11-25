# This code does XXXXX
# Code developed by Vera Choo, with reference from: XXXX

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(rsconnect)
library(RColorBrewer)
setwd("C:/Users/Vera/Desktop/FALL 2019/TRIP/WEB/SNAP POLLS")

###### read in snap polls ########
snap<-lapply(1:11, function(x) assign(paste("snap",x,sep="_"),read.csv(paste("TRIP_SnapPoll",x,"_1.0.0.csv",sep=""),na.strings=c("","NA","NULL"))))

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

commas<-data.frame(commas=c("qg_153","qg_151","q4486","Q21"),length=c(3,3,3,6))
multipart<-list("q83_1"=c("Deter nuclear attacks by another state","Coerce states that have nuclear weapons to change their behavior","Deter conventional attacks by another nuclear armed state","Coerce states without nuclear weapons to change their behavior","Deter conventional attacks by a state without nuclear weapons"),
                "q87_1"=c("Australia","Canada","Colombia","Denmark","Germany","Hong Kong","Ireland","Israel","Italy","Japan","Jordan","Republic of Korea","Norway","Philippines","Poland","Turkey","United Kingdom"),
                "q90_1"=c("Maintaining U.S. military superiority","Placing sanctions on other countries","Signing free trade agreements","Maintaining existing alliances","Building new alliances","International agreements","Military intervention","Participating in international organizations"),
                "qg_1856_11288"=c("US air strikes against suspected terrorists by manned aircraft","US air strikes against suspected terrorists by drones/unmanned aircraft","Sending US trainers and special operations forces to countries where terrorists operate","Using enhanced interrogation or torture against suspected terrorists who are captured","Sending US ground troops to fight suspected terrorists abroad","Limiting the flow of migrants/refugees and increasing border controls","Blocking suspected terrorist financing"),
                "qg_1857_11299"=c("The United States","A coalition of Middle Eastern states","NATO","The United Nations Security Council"),
                "qg_1952_12278"=c("Hillary Clinton","Donald Trump"),
                "qg_1944_12235"=c("South Korea","Japan","Taiwan"),
                "qg_1943_12230"=c("South Korea","Japan","Taiwan"),
                "qg_1942_12277"=c("Hillary Clinton","Donald Trump"),
                "qg_1941_12276"=c("Hillary Clinton","Donald Trump"),
                "qg_1938_12275"=c("Hillary Clinton","Donald Trump"),
                "qg_1937_12274"=c("Hillary Clinton","Donald Trump"))

#### ####
#### read in codebooks ####
snap_cb<-lapply(1:11, function(x) assign(paste("snap",x,sep="_"),read.csv(paste("TRIP_SnapPoll",x,"_1.0.0_codebook.csv",sep=""),na.strings="")))
snap_cb[[11]]<-snap_cb[[11]][1:20,]

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
                       fluidRow(column(12,
                                       strong(h3(textOutput("selectedQ"))),
                                       br())),
                       fluidRow(column(3,
                                       wellPanel(
                                         selectInput("crosstabList","Crosstab:",
                                                     c("Select..."="NULL","Gender"="gender","Rank","Area of study","etc.")),br(),
                                         actionButton("return","Return to question list"))),
                                column(9,
                                       textOutput("error"),
                                       uiOutput("noGraph"),
                                       plotlyOutput("graph"),
                                       br(),br())))),
  tags$style(type="text/css", ".shiny-output-error{visibility: hidden;}", ".shiny-output-error:before{visibility:hidden;}"))

#######################################################
#                       SERVER                        #
#######################################################
server <- function(input, output, session) {
  ###################### Q U E S T I O N S ######################
  qid<-reactive({snap_cb[[as.numeric(input$dataSelect)]]})
  dataStore<-reactiveValues(dataLoc=NULL,dataNum=NULL)
  crosstabs<-reactive({
    if(input$crosstabList=="NULL"){
      crosstabs<-NULL
    } else {
      crosstabs<-input$crosstabList
    }
    })
  
  ## list of questions ##
  output$questions<-renderUI({
    lapply(1:nrow(qid()), function(x) fluidRow(actionLink(paste0("btn_",x),qid()[,2][x]), br()))
  })

  lapply(1:28, function(x){
    observeEvent(input[[paste0("btn_",x)]], {
      i <- as.numeric(sub("btn_", "", x))
      dataStore$dataNum <- i
      dataStore$dataLoc<-unlist(strsplit(as.character(qid()[as.numeric(dataStore$dataNum),1]),"[,:]"))
      updateTabsetPanel(session, "Questions", "Graph")
    })
  })
  
  ## construct dataframe ##
  new_df<-function(x){
    y <- if(length(dataStore$dataLoc)==1 && dataStore$dataLoc[1] %in% commas[,1]){ #multiselect Qs
      len<-commas[grep(dataStore$dataLoc[1],commas[,1]),2]
      z<-data.frame(snap[[x]] %>% select(crosstabs(),response=dataStore$dataLoc[1]))
      z<-z %>% drop_na() %>% separate(response,c(paste("response",1:len,sep="_")),sep=",") %>% group_by_at(crosstabs()) %>% mutate(total_n=n()) %>% gather(key,response,response_1:paste0("response_",len)) %>% select(-key) %>% group_by_all() %>% mutate(per=round(n()/total_n*100,2)) %>% distinct(response, .keep_all=TRUE) %>% arrange(desc(per))
      z
    } else if(length(dataStore$dataLoc)==2 && dataStore$dataLoc[1] %in% names(multipart)){
      len<-length(multipart[[grep(dataStore$dataLoc[1],names(multipart))]])
      z<-data.frame(snap[[x]] %>% select(dataStore$dataLoc[1]:dataStore$dataLoc[2],crosstabs()))
      z<-lapply(1:len, function(x){
        if(!is.null(crosstabs())){
          y<-z[c(len+1,x)] %>% group_by_all() %>% drop_na() %>% summarise(n=n()) %>% mutate(per=round(n/sum(n)*100,2)) %>% mutate(sub_question=multipart[[grep(names(z)[1],names(multipart))]][x])
          names(y)<-c(crosstabs(),"response","n","per", "sub_question")
          y 
        } else if(is.null(crosstabs())){
          y<-z[c(x)] %>% group_by_all() %>% drop_na() %>% summarise(n=n()) %>% mutate(per=round(n/sum(n)*100,2)) %>% mutate(sub_question=multipart[[grep(names(z)[1],names(multipart))]][x])
          names(y)<-c("response","n","per", "sub_question")
          y   
        }
      })
      z<-do.call(rbind,z)
      z
    } else if(length(dataStore$dataLoc)==1){
      data.frame(snap[[x]] %>% select(crosstabs(),response=dataStore$dataLoc[1]) %>% filter(!is.na(response)) %>% drop_na() %>% group_by_all() %>% summarize(n=n()) %>% mutate(per=round(n/sum(n)*100,2)))
    } else if(length(dataStore$dataLoc)==2){
      data.frame(snap[[x]] %>% select(crosstabs(),dataStore$dataLoc[1]:dataStore$dataLoc[2]) %>% group_by_at(crosstabs()) %>% mutate(total_n=n()) %>% gather(key,value,dataStore$dataLoc[1]:dataStore$dataLoc[2]) %>% select(crosstabs(), response=value,total_n) %>% filter(!is.na(response)) %>% drop_na() %>% group_by_all() %>% summarize(n=n()) %>% mutate(per=round(n/total_n*100,2)) %>% select(-total_n))
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
    if(dataStore$dataLoc[1] %in% names(multipart)){
      if(is.null(crosstabs())){
        p<-plot_ly(df(), x=~sub_question, y=~per, color=~response, colors="YlOrRd", type="bar",hoverinfo='text',text= ~paste(sub_question,'<br>', response, ': ', per,'%',sep=""), height=800) %>% layout(barmode='stack',margin = list(l = 50, r = 50, t = 50, b = 450))
    } else if(!is.null(crosstabs())){
        len<-length(multipart[[grep(dataStore$dataLoc[1],names(multipart))]])
        sub_q<-multipart[[grep(dataStore$dataLoc[1],names(multipart))]]
        p_list_1<-df() %>% filter(sub_question==sub_q[1]) %>% plot_ly(x=~get(crosstabs()), y=~per, color=~response,colors="YlOrRd",type='bar', height=800,hoverinfo='text',text= ~paste(sub_question,'<br>', response, ': ', per,'%',sep="")) %>% layout(annotations=list(text = sprintf(paste("<b>",sub_q[1],"</b>")),xref="paper",yref="paper",yanchor="bottom",xanchor="center",align="center",x=0.5,y=1,showarrow=FALSE,textangle=-45),legend=list(.08,.08),margin=list(l=50, r=0, t=350, b=50),barmode='stack', xaxis=list(title=""))
        p_list<-assign(paste("p",len,sep="_"), lapply(2:len, function(x) df() %>% filter(sub_question==sub_q[x]) %>% plot_ly(x=~get(crosstabs()), y=~per, color=~response,colors="YlOrRd",type='bar',showlegend=FALSE, height=800,hoverinfo='text',text= ~paste(sub_question,'<br>', response, ': ', per,'%',sep="")) %>% layout(annotations=list(text = sprintf(paste("<b>",sub_q[x],"</b>")),xref="paper",yref="paper",yanchor="bottom",xanchor="center",align="center",x=0.5,y=1,showarrow=FALSE,textangle=-45),legend=list(.08,.08),margin=list(l=50, r=0, t=350, b=50),barmode='stack', xaxis=list(title=""))))
        p_list[[len]]<-p_list_1
        p<-subplot(p_list,shareX=TRUE,shareY=TRUE) %>% layout(showlegend=TRUE)
    }
    } else if(!is.null(crosstabs())){
      p<-plot_ly(df(), x=~response, y=~per, color=~get(crosstabs()), colors="YlOrRd", type="bar",hoverinfo='text',text=~paste(response,'<br>Percentage: ', per,'%',sep=""), height=800) %>% layout(bargap=5,legend=list(.08,.08),margin=list(l=50, r=0, t=50, b=450))
    } else if(is.null(crosstabs())){
      p<-plot_ly(df(), x=~response, y=~per, color=~response, colors="YlOrRd", type="bar",hoverinfo='text',text= ~paste(response,'<br>Percentage: ', per,'%',sep=""), height=800) %>% layout(bargap=5,legend=list(.08,.08),margin=list(l=50, r=0, t=0, b=450),barmode='relative')
    }
  })
  
  output$graph<-renderPlotly({
    p()
  })
  ## print error ##
  output$error<-renderText({
    str(crosstabs())
    print(head(df()))
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

