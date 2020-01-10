# This code does XXXXX
# Code developed by Vera Choo, with reference from: XXXX

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(rsconnect)
library(RColorBrewer)
library(viridis)
library(colorspace)

#setwd("C:/Users/Vera/Desktop/FALL 2019/TRIP/WEB/SNAP POLLS")

###### read in snap polls ########
snap<-list("snap_1"=readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/snap_1.rds")),
           "snap_2"=readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/snap_2.rds")),
           "snap_3"=readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/snap_3.rds")),
           "snap_4"=readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/snap_4.rds")),
           "snap_5"=readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/snap_5.rds")),
           "snap_6"=readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/snap_6.rds")),
           "snap_7"=readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/snap_7.rds")),
           "snap_8"=readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/snap_8.rds")),
           "snap_9"=readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/snap_9.rds")),
           "snap_10"=readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/snap_10.rds")),
           "snap_11"=readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/snap_11.rds")),
           "snap_12"=readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/snap_12.rds"))
)

#### read in codebooks ####
snap_cb<-list("snap_1"=readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/snap_cb_1.rds")),
           "snap_2"=readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/snap_cb_2.rds")),
           "snap_3"=readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/snap_cb_3.rds")),
           "snap_4"=readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/snap_cb_4.rds")),
           "snap_5"=readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/snap_cb_5.rds")),
           "snap_6"=readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/snap_cb_6.rds")),
           "snap_7"=readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/snap_cb_7.rds")),
           "snap_8"=readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/snap_cb_8.rds")),
           "snap_9"=readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/snap_cb_9.rds")),
           "snap_10"=readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/snap_cb_10.rds")),
           "snap_11"=readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/snap_cb_11.rds")),
           "snap_12"=readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/snap_cb_12.rds"))
)

commas<-readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/commas.rds"))
multipart<-readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/multipart.rds"))
notes<-readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/notes.rds"))

#######################################################
#                   USER INTERFACE                    #
#######################################################
ui <- function(req){
  fluidPage(
  tags$head(
    tags$style(HTML("
                    #display-titles{width:75%;height:200px;overflow:clip;color:#fff;display:block;margin:auto;background-color:#000;z-index:1000;margin-top:-100px;}
                    .text{transform-origin: top left;transform:rotate(315deg); width:220px;overflow:break-word;color:#000;margin-top:160px;}
                    .action-button{padding:10px; margin-top:-12px;display:block;background-color:#e1eaf0;transition: background-color .3s, color .3s; border-radius:5px;}
                    .action-button:hover{background-color:#edf2f5;transition: background-color .3s, color .3s;text-decoration:none;}
                    .well{ background-color:#e1eaf0;}
                    #tweet{background-color:#1DA1F2;color:#ffffff;font-weight:bold;width:100%;}
                    #return{background-color:#f2f9fc;transition: background-color .3s, color .3s;border-color:#ffffff;width:100%;font-weight:bold;}
                    #return:hover{ background-color:#fafdff;  transition: background-color .3s, color .3s;}
                    #clipbtn{background-color:#fff;font-weight:bold;width:100%;}
                    #clipbtn:hover{background-color:#edf2f5;transition: background-color .3s, color .3s;}
                    #tweet:hover{background-color:#65bff6;transition: background-color .3s, color .3s;}
                    "))
    ),
  titlePanel(h1(strong("Snap Polls"))),
  tabsetPanel(id="Questions",
              tabPanel("Questions",
                       br(),
                       sidebarPanel(
                         selectInput("dataSelect","Choose Snap Poll:",
                                     choices=rev(c("Snap Poll I: Syria, Ukraine, and the U.S. Defense Budget"=1,
                                               "Snap Poll II: Ukraine, Energy, and the Middle East"=2,
                                               "Snap Poll III: Seven Questions on Current Global Issues for IR Scholars"=3,
                                               "Snap Poll IV: Ten Questions on Current Global Issues for IR Scholars"=4,
                                               "Snap Poll V: Proposed Nuclear Agreement with Iran"=5,
                                               "Snap Poll VI: Greece, Migration Crisis, Trade Agreements, and FIFA"=6,
                                               "Snap Poll VII: South China Sea, Refugee Crisis, and Iran Deal"=7,
                                               "Snap Poll VIII: 2016 Presidential Campaign, Zika, and Terrorism in the Middle East"=8,
                                               "Snap Poll IX: U.S. Foreign Policy and the 2016 Presidential Election"=9,
                                               "Snap Poll X (Embedded in 2017 Faculty Survey)"=10,
                                               "Snap Poll XI: What Experts Make of Trump's Foreign Policy"=11,
                                               "Snap Poll XII: 2020 Primary, Impeachment, and Trump's Foreign Policy"=12)))), # updates go here
                       mainPanel(br(),uiOutput("questions"),tableOutput("print"))),
              tabPanel("Graph",
                       fluidRow(column(12,
                                       strong(h3(textOutput("selectedQ"))),
                                       br())),
                       fluidRow(column(3,
                                       wellPanel(
                                         selectInput("crosstabList","Crosstab:",
                                                     c("Select.."="NULL","All respondents"="NO","Gender"="gender","Rank"="rank","Age"="age","Economic Ideology"="econ_ideology","Social Ideology"="social_ideology","Party Identification"="party_id")),br(),
                                         actionButton("clipbtn","Copy URL",icon("clipboard")),br(),
                                         actionButton("tweet","Tweet",onclick="window.open('http://twitter.com/intent/tweet?text=' + encodeURIComponent(window.location.href))"),br(),
                                         actionButton("return","Return to question list")),
                                       uiOutput("exp_notes")),
                                column(9,
                                       #uiOutput("titles"),
                                       uiOutput("noGraph"),
                                       plotlyOutput("graph"),
                                       br(),br())))),
  tags$style(type="text/css", ".shiny-output-error{visibility: hidden;}", ".shiny-output-error:before{visibility:hidden;}"))
}

#######################################################
#                       SERVER                        #
#######################################################
server <- function(input, output, session) {
  ###################### Q U E S T I O N S ######################
  qid<-reactive({snap_cb[[as.numeric(input$dataSelect)]]})
  dataStore<-reactiveValues(dataLoc=NULL,dataNum=NULL)
  crosstabs<-reactive({
    if(input$crosstabList=="NULL" | input$crosstabList=="NO"){
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
      z<-z %>% drop_na() %>% separate(response,c(paste("response",1:len,sep="_")),sep=",") %>% group_by_at(crosstabs()) %>% mutate(total_n=n()) %>% gather(key,response,response_1:paste0("response_",len)) %>% select(-key) %>% group_by_all() %>% mutate(Percentage=round(n()/total_n*100,2)) %>% distinct(response, .keep_all=TRUE) %>% arrange(desc(Percentage))
      z
    } else if(length(dataStore$dataLoc)==2 && dataStore$dataLoc[1] %in% names(multipart)){
      len<-length(multipart[[grep(dataStore$dataLoc[1],names(multipart))]])
      z<-data.frame(snap[[x]] %>% select(dataStore$dataLoc[1]:dataStore$dataLoc[2],crosstabs()))
      z<-lapply(1:len, function(x){
        if(!is.null(crosstabs())){
          y<-z[c(len+1,x)] %>% group_by_all() %>% drop_na() %>% summarise(n=n()) %>% mutate(Percentage=round(n/sum(n)*100,2)) %>% mutate(sub_question=multipart[[grep(names(z)[1],names(multipart))]][x])
          names(y)<-c(crosstabs(),"response","n","Percentage", "sub_question")
          y 
        } else if(is.null(crosstabs())){
          y<-z[c(x)] %>% group_by_all() %>% drop_na() %>% summarise(n=n()) %>% mutate(Percentage=round(n/sum(n)*100,2)) %>% mutate(sub_question=multipart[[grep(names(z)[1],names(multipart))]][x])
          names(y)<-c("response","n","Percentage", "sub_question")
          y   
        }
      })
      z<-do.call(rbind,z)
      z
    } else if(length(dataStore$dataLoc)==1){
      data.frame(snap[[x]] %>% select(crosstabs(),response=dataStore$dataLoc[1]) %>% filter(!is.na(response)) %>% drop_na() %>% group_by_all() %>% summarize(n=n()) %>% mutate(Percentage=round(n/sum(n)*100,2)))
    } else if(length(dataStore$dataLoc)==2){
      data.frame(snap[[x]] %>% select(crosstabs(),dataStore$dataLoc[1]:dataStore$dataLoc[2]) %>% group_by_at(crosstabs()) %>% mutate(total_n=n()) %>% gather(key,value,dataStore$dataLoc[1]:dataStore$dataLoc[2]) %>% select(crosstabs(), response=value,total_n) %>% filter(!is.na(response)) %>% drop_na() %>% group_by_all() %>% summarize(n=n()) %>% mutate(Percentage=round(n/total_n*100,2)) %>% select(-total_n))
    }
    return(y)
  }
  
  df<-reactive({
    df<-new_df(as.numeric(input$dataSelect))
    return(df)
  })
  ##### #####
  
  ###################### G R A P H ######################
  ## create graph ##
  p<-reactive({
    if(dataStore$dataLoc[1] %in% names(multipart)){
      if(is.null(crosstabs())){
        p<-plot_ly(df(), x=~sub_question, y=~Percentage, color=~response, colors=sequential_hcl(15,"SunsetDark"), type="bar",hoverinfo='text',text= ~paste(sub_question,'<br>', response, ': ', Percentage,'%',sep=""), height=600) %>% layout(barmode='stack',margin = list(l = 50, r = 50, t = 50, b = 200),xaxis=list(title = ""))
    } else if(!is.null(crosstabs())){
        len<-length(multipart[[grep(dataStore$dataLoc[1],names(multipart))]])
        sub_q<-multipart[[grep(dataStore$dataLoc[1],names(multipart))]]
        p_list_1<-df() %>% filter(sub_question==sub_q[1]) %>% plot_ly(x=~get(crosstabs()), y=~Percentage, color=~response,colors=sequential_hcl(15,"SunsetDark"),type='bar', height=800,hoverinfo='text',text= ~paste(sub_question,'<br>', response, ': ', Percentage,'%',sep="")) %>% layout(annotations=list(text = sprintf(paste("<b>",sub_q[1],"</b>")),xref="paper",yref="paper",yanchor="bottom",xanchor="center",align="center",x=0.5,y=1,showarrow=FALSE,textangle=-45),legend=list(.08,.08),margin=list(l=50, r=0, t=150, b=250),barmode='stack', xaxis=list(title=""))
        p_list<-assign(paste("p",len,sep="_"), lapply(2:len, function(x) df() %>% filter(sub_question==sub_q[x]) %>% plot_ly(x=~get(crosstabs()), y=~Percentage, color=~response,colors=sequential_hcl(15,"SunsetDark"),type='bar',showlegend=FALSE, height=800,hoverinfo='text',text= ~paste(sub_question,'<br>', response, ': ', Percentage,'%',sep="")) %>% layout(annotations=list(text = sprintf(paste("<b>",sub_q[x],"</b>")),xref="paper",yref="paper",yanchor="bottom",xanchor="center",align="center",x=0.5,y=1,showarrow=FALSE,textangle=-45),legend=list(.08,.08),margin=list(l=50, r=0, t=150, b=250),barmode='stack', xaxis=list(title=""))))
        p_list[[len]]<-p_list_1
        p<-subplot(p_list,shareX=TRUE,shareY=TRUE) %>% layout(showlegend=TRUE,xaxis=list(title = ""))
    }
    } else if(!is.null(crosstabs())){
      p<-plot_ly(df(), x=~response, y=~Percentage, color=~get(crosstabs()), colors=sequential_hcl(15,"SunsetDark"), type="bar",hoverinfo='text',text=~paste(get(crosstabs()),'<br>',response,'<br>Percentage: ', Percentage,'%',sep=""), height=600) %>% layout(bargap=5,legend=list(.08,.08),margin=list(l=50, r=50, t=50, b=200),xaxis=list(title = ""))
    } else if(is.null(crosstabs())){
      p<-plot_ly(df(), x=~response, y=~Percentage, color=~response, colors=sequential_hcl(15,"SunsetDark"), type="bar",hoverinfo='text',text= ~paste('<br>',response,'<br>Percentage: ', Percentage,'%',sep=""), height=600) %>% layout(bargap=5,legend=list(.08,.08),margin=list(l=50, r=50, t=50, b=200),barmode='relative',xaxis=list(title = ""))
    }
  })
  # color used to be "RdYlBu"
  
  output$graph<-renderPlotly({
    p()
  })
    
  ## return to questions list ##
  observeEvent(input$return, {
    updateTabsetPanel(session,"Questions","Questions")
  })
  ## print question as title ##
  output$selectedQ<-renderText({paste(qid()[,2][as.numeric(dataStore$dataNum)])})
  #### ####
  
  note_display<-reactive({as.character(notes[(grep(dataStore$dataLoc[1],notes[,1])),2])})
  output$exp_notes<-renderUI({
    if(dataStore$dataLoc[1] %in% notes[,1]){
      wellPanel(strong(h5(note_display())))
    }
  })
  
  output$titles<-renderUI({
    if(dataStore$dataLoc[1] %in% names(multipart)){
      test<-HTML(unlist(lapply(1:length(multipart[[grep(dataStore$dataLoc[1],names(multipart))]]), function(x) paste0("<div style=\"margin-right:0;width:",(80/length(multipart[[grep(dataStore$dataLoc[1],names(multipart))]])),"%;display:inline-block;\"><div class=\"text\">",multipart[[grep(dataStore$dataLoc[1],names(multipart))]][[x]],"</div></div>"))))
      HTML("<div id=\"display-titles\">",test,"</div")
    }
  })
  
  ## URL and bookmarking ##
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(function(url){updateQueryString(url)})
  observeEvent(input$clipbtn, {showModal(urlModal(URL(), title="Copy URL to clipboard"))})
   URL <- reactiveVal()
   onBookmarked(function(url){URL(url)})
   onRestore(function(state){updateTabsetPanel(session,"Questions","Questions")})
}

enableBookmarking(store = "url")
shinyApp(ui, server)




