# This code does XXXXX
# Code developed by Vera Choo, with reference from: XXXX

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggiraph)
library(stringr)
library(rsconnect)
library(RColorBrewer)
library(colorspace)
library(textutils)

#setwd("C:/Users/Vera/Desktop/FALL 2019/TRIP/WEB/SNAP POLLS")

###### read in snap polls ########
snap<-readRDS(url("https://www.dropbox.com/s/pkbg8n6nuxx7fcx/snap.rds?dl=1"))

#### read in codebooks ####
snap_cb<-readRDS(url("https://www.dropbox.com/s/8gd9g1sep32ycee/snap_cb.rds?dl=1"))

commas<-readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/commas.rds"))
multipart<-readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/multipart.rds"))
notes<-readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/notes.rds"))
palette<-readRDS(url("https://www.dropbox.com/s/y4paezgfjk8ddcm/palette.rds?dl=1"))

plot_theme<-theme_bw() +
  theme(axis.text.x=element_text(angle=310,vjust=1,hjust=0,size=7),axis.ticks.y=element_blank(),axis.ticks.x=element_line(size=.25),axis.text.y=element_text(size=7.5), axis.title.x=element_text(vjust=0),axis.title.y=element_text(size=8),panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),legend.position="right",legend.direction="vertical",legend.key.width=unit(1.5, "points"),legend.key.height=unit(1.5, "points"),legend.margin=margin(0),legend.justification="top",legend.box.margin=margin(0),legend.text=element_text(size=7.5),legend.title=element_text(size=7.5),plot.margin=margin(0,0,0,0),strip.background=element_blank(),strip.text=element_text(size=7))

#######################################################
#                   USER INTERFACE                    #
#######################################################
ui <- function(req){
  fluidPage(
  tags$head(
    tags$title("TRIP Snap Polls"),
    tags$style(HTML("
                    text{font-family: sans-serif;}
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
                       fluidRow(column(3,br(),
                                       wellPanel(
                                         selectInput("crosstabList","Crosstab:",
                                                     c("Select.."="NULL","All respondents"="NO","Gender"="gender","Rank"="rank","Age"="age","Economic Ideology"="econ_ideology","Social Ideology"="social_ideology","Party Identification"="party_id")),br(),
                                         actionButton("clipbtn","Copy URL",icon("clipboard")),br(),
                                         actionButton("tweet","Tweet",onclick="window.open('http://twitter.com/intent/tweet?text=' + encodeURIComponent(window.location.href))"),br(),
                                         actionButton("return","Return to question list")),
                                       uiOutput("exp_notes")),
                                column(9,
                                       strong(h3(textOutput("selectedQ"))),
                                       #uiOutput("titles"),
                                       uiOutput("noGraph"),
                                       girafeOutput("graph",width="99%",height="725px"),
                                       textOutput("error"),
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
      z<-data.frame(snap[[x]] %>% select(crosstabs(),Response=dataStore$dataLoc[1]))
      z<-z %>% drop_na() %>% separate(Response,c(paste("Response",1:len,sep="_")),sep=",") %>% group_by_at(crosstabs()) %>% mutate(total_n=n()) %>% gather(key,Response,Response_1:paste0("Response_",len)) %>% select(-key) %>% group_by_all() %>% mutate(Percentage=round(n()/total_n*100,2)) %>% distinct(Response, .keep_all=TRUE) %>% arrange(desc(Percentage))
      z
    } else if(length(dataStore$dataLoc)==2 && dataStore$dataLoc[1] %in% names(multipart)){
      len<-length(multipart[[grep(dataStore$dataLoc[1],names(multipart))]])
      z<-data.frame(snap[[x]] %>% select(dataStore$dataLoc[1]:dataStore$dataLoc[2],crosstabs()))
      z<-lapply(1:len, function(x){
        if(!is.null(crosstabs())){
          y<-z[c(len+1,x)] %>% group_by_all() %>% drop_na() %>% summarise(n=n()) %>% mutate(Percentage=round(n/sum(n)*100,2)) %>% mutate(sub_question=multipart[[grep(names(z)[1],names(multipart))]][x])
          names(y)<-c(crosstabs(),"Response","n","Percentage", "sub_question")
          y 
        } else if(is.null(crosstabs())){
          y<-z[c(x)] %>% group_by_all() %>% drop_na() %>% summarise(n=n()) %>% mutate(Percentage=round(n/sum(n)*100,2)) %>% mutate(sub_question=multipart[[grep(names(z)[1],names(multipart))]][x])
          names(y)<-c("Response","n","Percentage", "sub_question")
          y   
        }
      })
      z<-do.call(rbind,z)
      z
    } else if(length(dataStore$dataLoc)==1){
      data.frame(snap[[x]] %>% select(crosstabs(),Response=dataStore$dataLoc[1]) %>% filter(!is.na(Response)) %>% drop_na() %>% group_by_all(.drop=FALSE) %>% summarize(n=n()) %>% mutate(Percentage=round(n/sum(n)*100,2)))
    } else if(length(dataStore$dataLoc)==2){
      data.frame(snap[[x]] %>% select(crosstabs(),dataStore$dataLoc[1]:dataStore$dataLoc[2]) %>% group_by_at(crosstabs()) %>% mutate(total_n=n()) %>% gather(key,value,dataStore$dataLoc[1]:dataStore$dataLoc[2]) %>% select(crosstabs(), Response=value,total_n) %>% filter(!is.na(Response)) %>% drop_na() %>% group_by_all() %>% summarize(n=n()) %>% mutate(Percentage=round(n/total_n*100,2)) %>% select(-total_n))
    }
    return(y)
  }
  
  df<-reactive({
    df<-new_df(as.numeric(input$dataSelect))
    #levels(df$Response)<-gsub("'","'",levels(df$Response))
    # levels(df$Response)<-gsub("'"," ",levels(df$Response))
    # levels(df$Response)<-gsub("'"," ",levels(df$Response))
    levels(df$Response)<-gsub("[\\]'","'",levels(df$Response))
    df$Response<-factor(df$Response)
    levels(df$Response)<-str_wrap(levels(df$Response),width=35)
    if(!is.null(df$sub_question)){
      # levels(df$sub_question)<-gsub("'"," ",levels(df$sub_question))
      # levels(df$sub_question)<-gsub("'"," ",levels(df$sub_question))
      # levels(df$sub_question)<-gsub("'"," ",levels(df$sub_question))
      df$sub_question<-factor(df$sub_question)
      levels(df$sub_question)<-str_wrap(levels(df$sub_question),width=35)}
    if(!is.null(crosstabs())){
      if(is.null(df$sub_question)){levels(df[[1]])<-str_wrap(levels(df[[1]]),width=35)}
      if(!is.null(df$sub_question)){levels(df[[1]])<-str_wrap(levels(df[[1]]),width=1)}
    }
    df$Percentage[is.nan(df$Percentage)]<-0
    return(df)
  })
  ##### #####
  
  ###################### G R A P H ######################
  ## create graph ##
  
  plot<-reactive({
      if(dataStore$dataLoc[1][1] %in% names(multipart)){
        if(is.null(crosstabs())){
          plot<-ggplot(df(), aes(x=sub_question,fill=Response,group=rev(Response))) +
            ylab("Percentage") + xlab("") + plot_theme + 
            geom_bar_interactive(aes(tooltip = paste0("<strong>Response:</strong> ",HTMLencode(Response),"<br> <strong>Percentage:</strong> ", round(Percentage,2),"%"),y=Percentage,fill=Response),stat="identity",size=2, show.legend=TRUE) +
            #{if(input$labels=="On") geom_text(aes(y=Percentage,label=paste0(Percentage,"%"),x=Response),size=2.5,vjust=-.5,color="#959595")} +
            scale_fill_manual(values=palette[1:length(levels(df()$Response))],name="Legend") +
            #scale_fill_discrete_sequential(palette="SunsetDark",name="Legend") +
            scale_y_continuous(limits=c(0,NA),expand=expand_scale(add=c(0,3.25))) +
            guides(fill=guide_legend(ncol=1))   
        } else if(!is.null(crosstabs())){
          plot<-ggplot(df(), aes(x=sub_question,fill=Response,group=rev(Response))) +
            ylab("Percentage") + xlab("") + plot_theme + facet_grid(get(crosstabs())~.) +
            geom_bar_interactive(aes(tooltip = paste0("<strong>Crosstab:</strong> ",HTMLencode(get(crosstabs())),"<br><strong>Category:</strong> ",HTMLencode(sub_question),"<br><strong>Response:</strong> ",HTMLencode(Response),"<br><strong>Percentage:</strong> ", round(Percentage,2),"%"),y=Percentage),stat="identity",size=2, show.legend=TRUE,drop=FALSE) +
            #{if(input$labels=="On") geom_text(aes(y=Percentage,label=paste0(Percentage,"%"),x=Response),size=2.5,vjust=-.5,color="#959595")} +
            scale_fill_manual(values=palette[1:length(levels(df()$Response))],name="Legend") +
            #scale_fill_discrete_sequential(palette="SunsetDark",name="Legend") +
            scale_y_continuous(limits=c(0,NA),expand=expand_scale(add=c(0,3.25))) +
            guides(fill=guide_legend(ncol=1))          
        }
      } else {
        if(is.null(crosstabs())){
          plot<-ggplot(df(), aes(x=Response,fill=Response)) +
            ylab("Percentage") + xlab("") + plot_theme +
            geom_bar_interactive(aes(tooltip = paste0("<strong>Response:</strong> ",HTMLencode(Response),"<br> <strong>Percentage:</strong> ", round(Percentage,2),"%"),y=Percentage),stat="identity", size=2, show.legend=TRUE) +
            #{if(input$labels=="On") geom_text(aes(y=Percentage,label=paste0(Percentage,"%"),x=Response),size=2.5,vjust=-.5,color="#959595")} +
            scale_fill_manual(values=palette[1:length(levels(df()$Response))],name="Legend") +
            #scale_fill_discrete_sequential(palette="SunsetDark",name="Legend",drop=FALSE) +
            scale_x_discrete() +
            scale_y_continuous(limits=c(0,NA),expand=expand_scale(add=c(0,3.25))) +
            guides(fill=guide_legend(ncol=1))
        } else if(!is.null(crosstabs())){
          plot<-ggplot(df(), aes(x=Response,y=Percentage,fill=factor(get(crosstabs())),group=factor(get(crosstabs())))) +
            ylab("Percentage") + xlab("") + plot_theme + 
            geom_bar_interactive(aes(tooltip=paste0("<strong>Crosstab:</strong> ",get(crosstabs()),"<br><strong>Response:</strong> ",HTMLencode(Response),"<br> <strong>Percentage:</strong> ", round(Percentage,2),"%")),stat="identity",size=1.5,show.legend=TRUE,position="dodge") + 
            #{if(input$labels=="On") geom_text(aes(y=Percentage,label=paste0(Percentage,"%"),x=Response,group=factor(get(crosstabs()))),position=position_dodge(width=1),size=2.5,color="#959595",angle=90,hjust=-.15)} +
            scale_fill_manual(values=palette[1:length(levels(df()[[1]]))],drop=FALSE,name="Legend") +
            scale_x_discrete() +
            scale_y_continuous(limits=c(0,NA),expand=expand_scale(add=c(0,3.25))) +
            guides(fill=guide_legend(ncol=1))          
        }
      }
  })
  
  output$graph<-renderGirafe({
    girafe_options(girafe(ggobj=plot()),width_svg=7,height_svg=6,opts_zoom(.5,2),opts_toolbar(saveaspng=FALSE),opts_sizing(rescale=FALSE,width=1),opts_tooltip(css="font-family:arial;font-size:12px;background-color:#ffffff;padding:5px;border-radius:7px;box-shadow:2px 2px #555555;"))
  })
  
  output$error<-renderText({
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
   setBookmarkExclude("clipbtn")
}

enableBookmarking(store = "url")
shinyApp(ui, server)

