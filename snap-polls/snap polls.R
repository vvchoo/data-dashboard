# This code does XXXXX
# Code developed by Vera Choo, with reference from: XXXX

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggiraph)
library(stringr)
library(rsconnect)
library(textutils)

#### read in data ####
snap<-readRDS("snap.rds")
snap_cb<-readRDS("snap_cb.rds")
snap_dates<-readRDS("snap_dates.rds")
palette<-readRDS("palette.rds")
multipart<-readRDS("multipart.rds")
multipart_stacked<-readRDS("multipart_stacked.rds")
commas<-readRDS("commas.rds")
notes<-readRDS("notes.rds")

#### create custom functions ####
`%!in%` <- Negate(`%in%`)
coalesce_by_column<-function(df) {
  return(dplyr::coalesce(!!!as.list(df)))
}

#### set up how the plot looks ####
plot_theme<-theme_bw() +
  theme(axis.text.x=element_text(angle=310,vjust=1,hjust=0,size=7),axis.ticks.y=element_blank(),axis.ticks.x=element_line(size=.25),axis.text.y=element_text(size=7.5), axis.title.x=element_text(vjust=0),axis.title.y=element_text(size=8),panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),legend.position="right",legend.direction="vertical",legend.key.width=unit(1.5, "points"),legend.key.height=unit(1.5, "points"),legend.margin=margin(0),legend.justification="top",legend.box.margin=margin(0),legend.text=element_text(size=7.5),legend.title=element_text(size=7.5),plot.margin=margin(0,0,0,0),strip.background=element_blank(),strip.text=element_text(size=7))

#######################################################
#                   USER INTERFACE                    #
#     HMTL that structures the layout of the app      #
#######################################################
ui <- function(req){
  fluidPage(
    tags$head(
      includeHTML("tracking.html"),
      tags$title("TRIP Snap Polls"), 
      tags$style(HTML("
                    text{font-family: sans-serif;}
                    #display-titles{width:75%;height:200px;overflow:clip;color:#fff;display:block;margin:auto;background-color:#000;z-index:1000;margin-top:-100px;}
                    .text{transform-origin: top left;transform:rotate(315deg); width:220px;overflow:break-word;color:#000;margin-top:160px;}
                    .action-button{padding:10px; margin-top:-12px;display:block;background-color:#F3F7FB;transition: background-color .3s, color .3s; border-radius:5px;}
                    .action-button:hover{background-color:#ECF1F7;transition: background-color .3s, color .3s;text-decoration:none;}
                    .well{ background-color:#F3F7FB;}
                    #tweet{background-color:#1DA1F2;color:#ffffff;font-weight:bold;width:100%;}
                    #return{background-color:#fff;transition: background-color .3s, color .3s;width:100%;font-weight:bold;}
                    #return:hover{background-color:#edf2f5;  transition: background-color .3s, color .3s;}
                    #clipbtn{background-color:#fff;font-weight:bold;width:100%;}
                    #clipbtn:hover{background-color:#edf2f5;transition: background-color .3s, color .3s;}
                    #tweet:hover{background-color:#65bff6;transition: background-color .3s, color .3s;}
                    #downloadPlot{font-weight:bold;width:100%;margin-top:10px;background-color:#fff;padding:10px;}
                    #downloadPlot:hover{background-color:#edf2f5;transition: background-color .3s, color .3s;}
                    #toggle{background-color:#fff;font-weight:bold;width:100%;}
                    #toggle:hover{background-color:#edf2f5;transition: background-color .3s, color .3s;}
                    ")) # CSS to tweak appearance of page
    ),
    titlePanel(h1(strong("Snap Polls"))),
    tabsetPanel(id="Questions",
                tabPanel("Questions",
                         br(),
                         column(4,
                                wellPanel(
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
                                                            "Snap Poll XII: 2020 Primary, Impeachment, and Trump's Foreign Policy"=12 # updates go in here
                                                            )))),
                                uiOutput("survey_info")),
                         mainPanel(br(),uiOutput("questions"),tableOutput("print"))),
                tabPanel("Graph",
                         fluidRow(column(3,br(),
                                         wellPanel(
                                           uiOutput("crosstab_cont"),br(),
                                           actionButton("toggle","View as table below"),br(),
                                           downloadButton("downloadPlot","Save plot as .PNG"),br(),br(),
                                           actionButton("clipbtn","Copy URL",icon("clipboard")),br(),
                                           actionButton("tweet","Tweet",onclick="window.open('http://twitter.com/intent/tweet?text=' + encodeURIComponent(window.location.href))"),br(),
                                           actionButton("return","Return to question list")),
                                         uiOutput("exp_notes")),
                                  column(9,
                                         strong(h3(textOutput("selectedQ"))),
                                         uiOutput("noGraph"),
                                         girafeOutput("graph",width="99%",height="750px"),
                                         textOutput("error"),
                                         uiOutput("table"))))),
    tags$style(type="text/css", ".shiny-output-error{visibility: hidden;}", ".shiny-output-error:before{visibility:hidden;}"))
}

#######################################################
#                       SERVER                        #
#######################################################
server <- function(input, output, session) {
  ###################### Q U E S T I O N S ######################
  qid<-reactive({snap_cb[[as.numeric(input$dataSelect)]]})
  dataStore<-reactiveValues(dataLoc=NULL,dataNum=NULL)
  
#### output crosstabs ####
  output$crosstab_cont<-renderUI({
    selectInput("crosstabList","Crosstab:",
      switch(input$dataSelect,
             `1`=c("Select.."="NULL","All respondents"="NO","Gender"="gender","Rank"="rank","Age"="age"),
             `2`=c("Select.."="NULL","All respondents"="NO","Gender"="gender","Rank"="rank","Age"="age"),
             `3`=c("Select.."="NULL","All respondents"="NO","Gender"="gender","Rank"="rank","Age"="age"),
             `4`=c("Select.."="NULL","All respondents"="NO","Gender"="gender","Rank"="rank","Age"="age"),
             `5`=c("Select.."="NULL","All respondents"="NO","Gender"="gender","Rank"="rank","Age"="age"),
             `6`=c("Select.."="NULL","All respondents"="NO","Gender"="gender","Rank"="rank","Age"="age"),
             `7`=c("Select.."="NULL","All respondents"="NO","Gender"="gender","Rank"="rank","Age"="age","Economic Ideology"="econ_ideology","Social Ideology"="social_ideology","Paradigm"="paradigm","Region of study"="region"),
             `8`=c("Select.."="NULL","All respondents"="NO","Gender"="gender","Age"="age","Economic Ideology"="econ_ideology","Social Ideology"="social_ideology","Paradigm"="paradigm","Region of study"="region"),
             `9`=c("Select.."="NULL","All respondents"="NO","Gender"="gender","Age"="age","Economic Ideology"="econ_ideology","Social Ideology"="social_ideology","Paradigm"="paradigm","Region of study"="region"),
             `10`=c("Select.."="NULL","All respondents"="NO","Gender"="gender","Rank"="rank","Age"="age","Economic Ideology"="econ_ideology","Social Ideology"="social_ideology","Paradigm"="paradigm","Region of study"="region","Issue Area"="issue_area"),
             `11`=c("Select.."="NULL","All respondents"="NO","Gender"="gender","Age"="age","Economic Ideology"="econ_ideology","Social Ideology"="social_ideology","Paradigm"="paradigm","Region of study"="region","Issue Area"="issue_area"),
             `12`=c("Select.."="NULL","All respondents"="NO","Gender"="gender","Rank"="rank","Age"="age","Economic Ideology"="econ_ideology","Social Ideology"="social_ideology","Party Identification"="party_id") # add crosstabs for new surveys here
      )
    )
  })
  
  crosstabs<-reactive({
    if(input$crosstabList=="NULL" | input$crosstabList=="NO"){crosstabs<-NULL} else {crosstabs<-input$crosstabList}
  })
  
  ## list of questions ##
  output$questions<-renderUI({
    lapply(1:nrow(qid()), function(x) fluidRow(actionLink(paste0("btn_",x),qid()[,2][x]), br()))
  })
  
  lapply(1:28, function(x){
    observeEvent(input[[paste0("btn_",x)]], {
      i <- as.numeric(sub("btn_", "", x))
      dataStore$dataNum <- i
      dataStore$dataLoc<-unlist(strsplit(as.character(qid()[as.numeric(dataStore$dataNum),1]),"[,:]")) # dataStore$dataLoc contains the variable name of the selected question
      updateTabsetPanel(session, "Questions", "Graph")
    })
  })
  
#### construct dataframe ####
  new_df<-function(x){
    y <- if(length(dataStore$dataLoc)==1 && dataStore$dataLoc[1] %in% commas[,1]){ #multiselect Qs
      len<-commas[grep(dataStore$dataLoc[1],commas[,1]),2]
      z<-data.frame(snap[[x]] %>% select(crosstabs(),Response=dataStore$dataLoc[1]))
      z<-z %>% drop_na() %>% separate(Response,c(paste("Response",1:len,sep="_")),sep=",") %>% group_by_at(crosstabs()) %>% mutate(total_n=n()) %>% gather(key,Response,Response_1:paste0("Response_",len)) %>% select(-key) %>% group_by_all() %>% mutate(Percentage=round(n()/total_n*100,2)) %>% group_by(Response) %>% mutate(Per_sum=sum(na.rm=TRUE,Percentage)) %>% drop_na() %>% select(crosstabs(),Response,total_n,Percentage,Per_sum)
      z
    } else if(length(dataStore$dataLoc)==2 && dataStore$dataLoc[1] %in% names(multipart)){
      len<-length(multipart[[grep(dataStore$dataLoc[1],names(multipart))]])
      z<-data.frame(snap[[x]] %>% select(dataStore$dataLoc[1]:dataStore$dataLoc[2],crosstabs()))
      z<-lapply(1:len, function(x){
        if(!is.null(crosstabs())){
          y<-z[c(len+1,x)] %>% group_by_all() %>% drop_na() %>% summarise(n=n()) %>% mutate(Percentage=round(n/sum(na.rm=TRUE,n)*100,2)) %>% mutate(Sub_question=multipart[[grep(names(z)[1],names(multipart))]][x])
          names(y)<-c(crosstabs(),"Response","n","Percentage", "Sub_question")
          y 
        } else if(is.null(crosstabs())){
          y<-z[c(x)] %>% group_by_all() %>% drop_na() %>% summarise(n=n()) %>% mutate(Percentage=round(n/sum(na.rm=TRUE,n)*100,2)) %>% mutate(Sub_question=multipart[[grep(names(z)[1],names(multipart))]][x])
          names(y)<-c("Response","n","Percentage", "Sub_question")
          y   
        }
      })
      z<-do.call(rbind,z)
      z<-z %>% group_by(Response) %>% mutate(Per_sum=sum(na.rm=TRUE,Percentage)) %>% drop_na()
      z
    } else if(length(dataStore$dataLoc)==1){
      data.frame(snap[[x]] %>% select(crosstabs(),Response=dataStore$dataLoc[1]) %>% filter(!is.na(Response)) %>% drop_na() %>% group_by_all(.drop=FALSE) %>% summarize(n=n()) %>% mutate(Percentage=round(n/sum(na.rm=TRUE,n)*100,2))) %>% group_by(Response) %>% mutate(Per_sum=sum(na.rm=TRUE,Percentage))
    } else if(length(dataStore$dataLoc)==2){
      data.frame(snap[[x]] %>% select(crosstabs(),dataStore$dataLoc[1]:dataStore$dataLoc[2]) %>% group_by_at(crosstabs()) %>% mutate(total_n=n()) %>% gather(key,value,dataStore$dataLoc[1]:dataStore$dataLoc[2]) %>% select(crosstabs(), Response=value,total_n) %>% filter(!is.na(Response)) %>% drop_na() %>% group_by_all() %>% summarize(n=n()) %>% mutate(Percentage=round(n/total_n*100,2)) %>% select(-total_n)) %>% group_by(Response) %>% mutate(Per_sum=sum(na.rm=TRUE,Percentage)) %>% drop_na()
    }
    if(!is.null(crosstabs()) && dataStore$dataLoc[[1]][1] %!in% names(multipart)){y<-tidyr::complete(y,!!sym(crosstabs()),Response,fill=list(n=0,Percentage=0,Per_sum=0))}
    Response_list<-y %>% ungroup() %>% distinct(Response,.keep_all=TRUE) %>% arrange(desc(Per_sum)) %>% dplyr::slice(1:10) %>% select(Response)
    y<-y %>% filter(Response %in% Response_list[[1]])
    return(y)
  }
  
  df<-reactive({
    df<-new_df(as.numeric(input$dataSelect))
    levels(df$Response)<-gsub("[\\]'","'",levels(df$Response))
    df$Response<-factor(df$Response)
    levels(df$Response)<-str_wrap(levels(df$Response),width=35)
    if(!is.null(df$Sub_question)){
      df$Sub_question<-factor(df$Sub_question)
      levels(df$Sub_question)<-str_wrap(levels(df$Sub_question),width=35)}
    if(!is.null(crosstabs())){
      if(is.null(df$Sub_question)){levels(df[[1]])<-str_wrap(levels(df[[1]]),width=35)}
      if(!is.null(df$Sub_question)){levels(df[[1]])<-str_wrap(levels(df[[1]]),width=1)}
    }
    df$Percentage[is.nan(df$Percentage)]<-0
    df<-data.frame(df)
    return(df)
  })
  ##### #####
  
  ###################### G R A P H ######################
  ## create graph ##
  plot<-reactive({
    if(dataStore$dataLoc[1][1] %in% names(multipart_stacked)){
      if(is.null(crosstabs())){
        plot<-ggplot(df(), aes(x=Sub_question,fill=Response,group=rev(Response))) +
          ylab("Percentage") + xlab("") + plot_theme + 
          geom_bar_interactive(aes(tooltip = paste0("<strong>Response:</strong> ",HTMLencode(Response),"<br> <strong>Percentage:</strong> ", round(Percentage,2),"%"),y=Percentage,fill=Response),stat="identity",size=2, show.legend=TRUE) +
          scale_fill_manual(values=palette[1:length(levels(df()$Response))],name="Legend") +
          scale_y_continuous(limits=c(0,NA),expand=expand_scale(add=c(0,3.25))) +
          guides(fill=guide_legend(ncol=1))   
      } else if(!is.null(crosstabs())){
        plot<-ggplot(df(), aes(x=Sub_question,fill=Response,group=rev(Response))) +
          ylab("Percentage") + xlab("") + plot_theme + facet_grid(get(crosstabs())~.) +
          geom_bar_interactive(aes(tooltip = paste0("<strong>Crosstab:</strong> ",HTMLencode(get(crosstabs())),"<br><strong>Category:</strong> ",HTMLencode(Sub_question),"<br><strong>Response:</strong> ",HTMLencode(Response),"<br><strong>Percentage:</strong> ", round(Percentage,2),"%"),y=Percentage),stat="identity",size=2, show.legend=TRUE,drop=FALSE) +
          scale_fill_manual(values=palette[1:length(levels(df()$Response))],name="Legend") +
          scale_y_continuous(limits=c(0,NA),expand=expand_scale(add=c(0,3.25))) +
          guides(fill=guide_legend(ncol=1))          
      }
    } else {
      if(is.null(crosstabs())){
        plot<-ggplot(df(), aes(x=Response,fill=Response)) +
          ylab("Percentage") + xlab("") + plot_theme +
          geom_bar_interactive(aes(tooltip = paste0("<strong>Response:</strong> ",HTMLencode(Response),"<br> <strong>Percentage:</strong> ", round(Percentage,2),"%"),y=Percentage),stat="identity", size=2, show.legend=TRUE) +
          geom_vline(xintercept=distinct(df(),as.numeric(Response))[1:n_distinct(df()$Response)-1,]+.5,color="grey",size=.35,linetype="dotted") +
          {if(dataStore$dataLoc[1][1] %in% names(multipart)) facet_grid(Sub_question~.)} +
          scale_fill_manual(values=palette[1:length(levels(df()$Response))],name="Legend") +
          scale_x_discrete() +
          scale_y_continuous(limits=c(0,NA),expand=expand_scale(add=c(0,3.25))) +
          guides(fill=guide_legend(ncol=1))
      } else if(!is.null(crosstabs())){
        plot<-ggplot(df(), aes(x=Response,y=Percentage,fill=factor(get(crosstabs())),group=factor(get(crosstabs())))) +
          ylab("Percentage") + xlab("") + plot_theme + 
          geom_vline(xintercept=distinct(df(),as.numeric(Response))[1:n_distinct(df()$Response)-1,]+.5,color="grey",size=.35,linetype="dotted") +
          geom_bar_interactive(aes(tooltip=paste0("<strong>Crosstab:</strong> ",get(crosstabs()),"<br><strong>Response:</strong> ",HTMLencode(Response),"<br> <strong>Percentage:</strong> ", round(Percentage,2),"%")),stat="identity",size=1.5,show.legend=TRUE,position="dodge") + 
          {if(dataStore$dataLoc[1][1] %in% names(multipart)) facet_grid(Sub_question~.)} +
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
##################################
  ## return to questions list ##
  observeEvent(input$return, {
    updateTabsetPanel(session,"Questions","Questions")
  })
  ## print question as title ##
  Qtitle<-reactive({Qtitle<-paste(qid()[,2][as.numeric(dataStore$dataNum)])})
  output$selectedQ<-renderText({Qtitle()})
  #### ####

#### generate note and information boxes ####
  ## front page info ##
  survey_info<-reactive({as.character(snap_dates[as.numeric(input$dataSelect),2])})
  output$survey_info<-renderUI({wellPanel(HTML(survey_info()))})
  note_display<-reactive({as.character(notes[(grep(dataStore$dataLoc[1],notes[,1])),2])})
  ## question-specific info ##
  output$exp_notes<-renderUI({
    if(dataStore$dataLoc[1] %in% notes[,1]){
      wellPanel(strong(h5(note_display())))
    }
  })
  
#### download handler for .png ####
  output$downloadPlot <- downloadHandler(filename=function(){
    paste("TRIP_PJD_", format(Sys.Date(), "%Y_%b_%d"),".png", sep="")
  },
  content=function(file){
    ggsave(file,plot()+ggtitle(str_wrap(Qtitle(),80)) + theme(plot.title=element_text(size=10.5),plot.margin=unit(c(2,2,0,2),"points")),device="png",width=6,height=3.5,dpi=300)
  })
  
#### generate table to print below graph ####
  observeEvent(input$toggle,{
    if(input$toggle %% 2==1){
      output$table<-renderUI({
        output$display<-renderTable({
          if(!is.null(crosstabs())){
            df() %>% select(-n,-Per_sum) %>% distinct() %>% mutate(Percentage=paste0(Percentage,"%")) %>% 
            pivot_wider(names_from=crosstabs(),values_from=Percentage) %>%
            {if(is.null(df()$Sub_question)) group_by(.,Response) %>% summarise_all(coalesce_by_column) %>% select(Response,everything()) else group_by(.,Response,Sub_question) %>% summarise_all(coalesce_by_column) %>% select(Sub_question,Response,everything()) %>% arrange(.,Sub_question)}
          } else {
            df() %>% select(-n,-Per_sum) %>% distinct() %>% mutate(Percentage=paste0(Percentage,"%")) %>%
              {if(is.null(df()$Sub_question)) select(.,Response,Percentage) else arrange(.,Sub_question) %>% select(.,Sub_question,Response,Percentage)}
          }
        },width="95%",hover=TRUE)
        tableOutput("display")
      })
      updateActionButton(session,"toggle","Remove table")
    } else if(input$toggle %% 2!=1){
      removeUI(selector="#display")
      updateActionButton(session,"toggle","View as table below") 
    }
  })
  
#### URL and bookmarking ####
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


