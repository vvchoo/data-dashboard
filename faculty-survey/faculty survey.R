# This does XXXXX
# Code developed by Vera Choo, with reference from: XXXX

library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(rsconnect)
library(ggiraph)
library(textutils)

#rsconnect::setAccountInfo(name='vvchoo', token='20318BE8E51FD829619E1D7095A496CB', secret='VUQ9WQai6Vt1CftmuLpcaxg5j3pHNgYReyWhCGZK')

surveys<-readRDS("surveys.rds")

# create codebook #
qid_list<-readRDS("qid_list.rds")
commas<-readRDS("commas.rds")
multipart<-readRDS("multipart.rds")
multipart_stacked<-readRDS("multipart_stacked.rds")
palette<-readRDS("palette.rds")
notes<-readRDS("notes.rds")
dates<-readRDS("fs_dates.rds")

`%!in%` <- Negate(`%in%`)
coalesce_by_column<-function(df) {
  return(dplyr::coalesce(!!!as.list(df)))
}

plot_theme<-theme_bw() +
  theme(axis.text.x=element_text(angle=310,vjust=1,hjust=0,size=7),axis.ticks.y=element_blank(),axis.ticks.x=element_line(size=.25),axis.text.y=element_text(size=7.5), axis.title.x=element_text(vjust=0),axis.title.y=element_text(size=8),panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),legend.position="right",legend.direction="vertical",legend.key.width=unit(1.5, "points"),legend.key.height=unit(2.5, "points"),legend.margin=margin(0),legend.justification="top",legend.box.margin=margin(0),legend.text=element_text(size=7.5),legend.title=element_text(size=7.5),plot.margin=margin(2,0,0,0),strip.background=element_blank(),strip.text=element_text(size=7))

#######################################################
#                   USER INTERFACE                    #
#######################################################
ui <- function(req){
  fluidPage(
    tags$head(
      includeHTML("tracking.html"),
      tags$title("TRIP Faculty Survey"),
      tags$link(rel="me", href="https://twitter.com/trip_irsurvey"),
      tags$style(HTML("
                      table{width:100%;}
                      .table{width:100%;}
                      text{font-family: sans-serif}
                      .action-button{padding:10px;margin-top:-12px;display:block;background-color:#F3F7FB;transition:background-color .3s, color .3s;border-radius:5px;}
                      .action-button:hover{background-color:#ECF1F7;transition:background-color .3s, color .3s;text-decoration:none;}
                      .well{background-color:#F3F7FB;}
                      #return{background-color:#fff;transition:background-color .3s, color .3s;width:100%;font-weight:bold;overflow-wrap:break-word;}
                      #return:hover{background-color:#edf2f5;transition:background-color .3s, color .3s;}
                      #tweet{background-color:#1DA1F2;color:#ffffff;font-weight:bold;width:100%;}
                      #tweet:hover{background-color:#65bff6;transition:background-color .3s, color .3s;}
                      #clipbtn{background-color:#fff;font-weight:bold;width:100%;}
                      #clipbtn:hover{background-color:#edf2f5;transition: background-color .3s, color .3s;}
                      #toggle{background-color:#fff;font-weight:bold;width:100%;}
                      #toggle:hover{background-color:#edf2f5;transition: background-color .3s, color .3s;}
                      #savePNG{background-color:#fff;font-weight:bold;width:100%;padding:10px;}
                      #savePNG:hover{background-color:#edf2f5;transition: background-color .3s, color .3s;padding:10px;}
                      #display{margin:auto;}
                      "))
      ),
    titlePanel(h1(strong("Faculty Survey"))),
    tabsetPanel(id="Questions",
                tabPanel("Questions",
                         br(),
                         column(3,
                         wellPanel(selectInput("dataSelect","Choose Survey Year:",
                                                  choices=c("2004 Faculty Survey"=1,
                                                            "2006 Faculty Survey"=2,
                                                            "2008 Faculty Survey"=3,
                                                            "2011 Faculty Survey"=4,
                                                            "2014 Faculty Survey"=5,
                                                            "2017 Faculty Survey"=6, # updates go here
                                                            "All Years (selected questions)"=7)),
                                      br(),
                                      uiOutput("section")),
                                      uiOutput("survey_info")),
                         mainPanel(uiOutput("sectionName"),br(),uiOutput("questions"),tableOutput("print"))),
                tabPanel("Graph",
                         fluidRow(column(3,br(),
                                         wellPanel(
                                           uiOutput("countryList"),br(),
                                           selectInput("crosstabList","Crosstab:",
                                                       c("Select..."="NULL",
                                                         "All respondents"="NO",
                                                         "Gender"="gender",
                                                         "Age"="age",
                                                         "Rank"="rank",
                                                         "Area of study"="subfield")),br(),
                                           actionButton("toggle","View as table below"),br(),
                                           downloadButton("savePNG","Save as .PNG"),br(),br(),
                                           actionButton("clipbtn","Copy URL",icon("clipboard")),br(),
                                           actionButton("tweet","Tweet",onclick="window.open('http://twitter.com/intent/tweet?text=' + encodeURIComponent(window.location.href))"),br(),
                                           actionButton("return","Return to question list"),
                                           uiOutput("exp_notes"))),
                                  column(9,
                                         strong(h3(textOutput("selectedQ"))),br(),
                                         uiOutput("noGraph"),
                                         girafeOutput("girafe",width="99%",height="725px"),
                                         textOutput("error"),
                                         uiOutput("table")
                                         )))),
    tags$style(type="text/css", ".shiny-output-error{visibility: hidden;}", ".shiny-output-error:before{visibility:hidden;}"))
}

#######################################################
#                       SERVER                        #
#######################################################
server <- function(input, output, session) {
  ##### country filter ####
  observeEvent(input$dataSelect, {
    output$countryList<-renderUI({
      if(input$dataSelect==7){
        selectInput("countries","Choose Survey Country:",c("United States only"="United States","All Countries"))
      } else {
        selectInput("countries","Choose Survey Country:",c("All Countries",levels(surveys[[as.numeric(input$dataSelect)]]$surveyCountry)))
      }
    })
  })
  
  countryFilter<-reactive({
    if(input$countries=="All Countries" & input$dataSelect!=7){
      countryFilter<-levels(surveys[[as.numeric(input$dataSelect)]]$surveyCountry)[table(surveys[[as.numeric(input$dataSelect)]]$surveyCountry)!=0]
    } else if(input$countries=="All Countries" & input$dataSelect==7){
      countryFilter<-unlist(lapply(1:6, function(x) levels(surveys[[x]]$surveyCountry)))
    } else {
      countryFilter<-as.character(input$countries)
    }
    countryFilter
  })
  
  res<-reactive({"Response"})
  
  
  #### question category filter ####
  observeEvent(input$dataSelect, {
    if(input$dataSelect!=7){
      output$section<-renderUI({
        selectInput("sectionSelect","Choose Question Category:",c("All questions"="NULL",names(table(qid_list[[as.numeric(input$dataSelect)]]$Question.category))))
      })
    }
  })
  
  sectionFilter<-reactive({
    if(input$sectionSelect=="NULL"){
      sectionFilter<-c(names(table(qid_list[[as.numeric(input$dataSelect)]]$Question.category)))
    } else {
      sectionFilter<-as.character(input$sectionSelect)
    }
  })
  
  observeEvent(input$sectionSelect, {
    if(input$sectionSelect!="NULL"){
      output$sectionName<-renderUI({h4(strong(as.character(input$sectionSelect)))})
    } else if(input$sectionSelect=="NULL"){output$sectionName<-NULL}
  })
  ####
  
  ###################### Q U E S T I O N S ######################
  qid<-reactive({
    if(input$dataSelect!=7){
      qid_list[[as.numeric(input$dataSelect)]] %>% filter(Question.category %in% sectionFilter())
    } else {
      qid_list[[as.numeric(input$dataSelect)]]
    }
  })
  
  dataStore<-reactiveValues(dataLoc=NULL,dataNum=NULL)
  crosstabs<-reactive({
    if(input$crosstabList=="NULL" | input$crosstabList=="NO"){
      crosstabs<-NULL
    } else { crosstabs<-input$crosstabList }
  })
  
  
  ## list of questions ##
  output$questions<-renderUI({ lapply(1:nrow(qid()), function(x) fluidRow(actionLink(paste0("btn_",x),qid()[,1][x]), br())) })
  
  ## generate listeners for action buttons ##
  
  lapply(1:60, function(x){
    observeEvent(input[[paste0("btn_",x)]], {
      i <- as.numeric(sub("btn_", "", x))
      dataStore$dataNum <- i
      if(input$dataSelect!=7){dataStore$dataLoc<-strsplit(as.character(qid()[as.numeric(dataStore$dataNum),3]),"[,:]")}
      if(input$dataSelect==7){dataStore$dataLoc<-lapply(2:7, function(x) unlist(strsplit(as.character(qid_list[[7]][as.numeric(dataStore$dataNum),x]),":")))}
      updateTabsetPanel(session, "Questions", "Graph")
    })
  })
  
  ## construct dataframe ##
  new_df<-function(x){
    y <- if(length(dataStore$dataLoc[[1]])==1 && dataStore$dataLoc[[1]][1] %in% commas[,1]){ #multiselect Qs
        len<-commas[grep(dataStore$dataLoc[[1]][1],commas[,1]),2]
        z<-data.frame(surveys[[x]] %>% filter(surveyCountry %in% countryFilter()) %>% select(crosstabs(),Response=dataStore$dataLoc[[1]][1]))
        z<-z %>% drop_na() %>% separate(Response,c(paste("Response",1:len,sep="_")),sep=",") %>% group_by_at(.drop=FALSE,crosstabs()) %>% mutate(total_n=n()) %>% gather(key,Response,Response_1:paste0("Response_",len)) %>% select(-key) %>% group_by_all(.drop=FALSE) %>% mutate(Percentage=round(n()/total_n*100,2)) %>% distinct(Response,crosstabs(),.keep_all=TRUE) %>% group_by(.drop=FALSE,Response) %>% mutate(Per_sum=sum(Percentage)) %>% drop_na() %>% select(crosstabs(),Response,total_n,Percentage,Per_sum)
        z
    } else if(length(dataStore$dataLoc[[1]])==2 && dataStore$dataLoc[[1]][1] %in% names(multipart)){
        len<-length(multipart[[grep(dataStore$dataLoc[[1]][1],names(multipart))]])
        z<-data.frame(surveys[[x]] %>% filter(surveyCountry %in% countryFilter()) %>% select(dataStore$dataLoc[[1]][1]:dataStore$dataLoc[[1]][2],crosstabs()))
        z<-lapply(1:len, function(x){
          if(!is.null(crosstabs())){
            y<-z[c(len+1,x)] %>% group_by_all(.drop=FALSE) %>% drop_na() %>% summarise(n=n()) %>% mutate(Percentage=round(n/sum(n)*100,2)) %>% mutate(Sub_question=multipart[[grep(names(z)[1],names(multipart))]][x]) %>% top_n(10,Percentage)
            names(y)<-c(crosstabs(),"Response","n","Percentage", "Sub_question")
            y 
          } else if(is.null(crosstabs())){
            y<-z[c(x)] %>% group_by_all(.drop=FALSE) %>% drop_na() %>% summarise(n=n()) %>% mutate(Percentage=round(n/sum(n)*100,2)) %>% mutate(Sub_question=multipart[[grep(names(z)[1],names(multipart))]][x]) %>% top_n(10,Percentage)
            names(y)<-c("Response","n","Percentage", "Sub_question")
            y   
          }
        })
        z<-do.call(rbind,z)
        z<-z %>% group_by(.drop=FALSE,Response) %>% mutate(Per_sum=sum(Percentage)) %>% drop_na()
        z
    } else if(length(dataStore$dataLoc[[1]])==1){
        data.frame(surveys[[x]] %>% filter(surveyCountry %in% countryFilter()) %>% select(crosstabs(),Response=dataStore$dataLoc[[1]][1]) %>% filter(!is.na(Response)) %>% drop_na() %>% group_by_all(.drop=FALSE) %>% summarize(n=n()) %>% mutate(Percentage=round(n/sum(n)*100,2))) %>% group_by(.drop=FALSE,Response) %>% mutate(Per_sum=sum(Percentage)) %>% drop_na()
    } else if(length(dataStore$dataLoc[[1]])==2){
        data.frame(surveys[[x]] %>% filter(surveyCountry %in% countryFilter()) %>% select(crosstabs(),surveyId,dataStore$dataLoc[[1]][1]:dataStore$dataLoc[[1]][2]) %>% group_by_at(.drop=FALSE,crosstabs()) %>% gather(key,value,dataStore$dataLoc[[1]][1]:dataStore$dataLoc[[1]][2]) %>% drop_na(value) %>% mutate(total_n=n_distinct(surveyId)) %>% select(crosstabs(), Response=value,total_n) %>% group_by_all(.drop=FALSE) %>% summarize(n=n()) %>% mutate(Percentage=round(n/total_n*100,2)) %>% select(-total_n)) %>% group_by(.drop=FALSE,Response) %>% mutate(Per_sum=sum(Percentage)) %>% drop_na()
    }
    if(!is.null(crosstabs()) && dataStore$dataLoc[[1]][1] %!in% names(multipart)){
      y<-y %>% ungroup() %>% tidyr::complete(!!sym(crosstabs()),Response,fill=list(total_n=0,Percentage=0,Per_sum=0))
      }
    Response_list<-y %>% ungroup() %>% distinct(Response,.keep_all=TRUE) %>% arrange(desc(Per_sum)) %>% dplyr::slice(1:10) %>% select(Response)
    y<-y %>% filter(Response %in% Response_list[[1]])
    return(y)
  }
  
  new_df_all<-function(x){
    y <- if(length(dataStore$dataLoc[[x]])==1 && dataStore$dataLoc[[x]][1] %in% commas[[1]]){ #multiselect Qs
      len<-commas[grep(dataStore$dataLoc[[x]][1],commas[,1]),2]
      z<-data.frame(surveys[[x]] %>% filter(surveyCountry %in% countryFilter()) %>% select(crosstabs(),Response=dataStore$dataLoc[[x]][1],surveyId) %>% drop_na(Response) %>% group_by_at(.drop=FALSE,crosstabs()) %>% mutate(total_n=n_distinct(surveyId)) %>% separate(Response,paste0("Response_",1:len),",") %>% gather(key,Response,Response_1:paste0("Response_",len)) %>% group_by_at(.drop=FALSE,vars(crosstabs(),res())) %>% mutate(n=n(),Percentage=round(n/total_n*100,2), Year=surveys[[x]]$surveyYear[1]) %>% drop_na() %>% distinct_at(vars(crosstabs(),Response,Percentage,Year),.keep_all=TRUE) %>% select(-surveyId,-key)) %>% group_by_at(.drop=FALSE,crosstabs()) %>% top_n(10,Percentage) %>% droplevels()
      z
    } else if(length(dataStore$dataLoc[[x]])==1 && dataStore$dataLoc[[x]][1] %!in% commas[[1]]){
      data.frame(surveys[[x]] %>% filter(surveyCountry %in% countryFilter()) %>% select(crosstabs(),Response=dataStore$dataLoc[[x]][1],surveyId) %>% filter(!is.na(Response)) %>% group_by_at(.drop=FALSE,crosstabs()) %>% mutate(total_n=n()) %>% group_by_at(.drop=FALSE,vars(crosstabs(),res())) %>% mutate(n=n(), Percentage=round((n/total_n)*100,2),Year=surveys[[x]]$surveyYear[1])) %>% distinct_at(vars(crosstabs(),Response,Percentage,Year)) %>% group_by_at(.drop=FALSE,crosstabs()) %>% top_n(10,Percentage) %>% drop_na %>% droplevels()
    } else {
      data.frame(surveys[[x]] %>% filter(surveyCountry %in% countryFilter()) %>% select(crosstabs(),dataStore$dataLoc[[x]][1]:dataStore$dataLoc[[x]][2],surveyId) %>% gather(key,value,dataStore$dataLoc[[x]][1]:dataStore$dataLoc[[x]][2]) %>% group_by_at(.drop=FALSE,crosstabs()) %>% drop_na() %>% mutate(total_n=n_distinct(surveyId)) %>% select(crosstabs(),Response=value,total_n) %>% group_by_all(.drop=FALSE) %>% summarize(n=n()) %>% mutate(Percentage=round(n/total_n*100,2),Year=surveys[[x]]$surveyYear[1])) %>% drop_na() %>% group_by_at(.drop=FALSE,crosstabs()) %>% top_n(10,Percentage) %>% droplevels()
    }
    y<-y %>% group_by(.drop=FALSE,Response) %>% mutate(Per_sum=sum(Percentage)) %>% drop_na()
    if(!is.null(crosstabs()) & dataStore$dataLoc[[1]][1] %!in% names(multipart)){y<-y %>% ungroup() %>% tidyr::complete(!!sym(crosstabs()),Response,fill=list(Percentage=0,Per_sum=0,total_n=0))}
    Response_list<-y %>% ungroup() %>% distinct(Response,.keep_all=TRUE) %>% arrange(desc(Per_sum)) %>% dplyr::slice(1:10) %>% select(Response)
    y<-y %>% filter(Response %in% Response_list[[1]])
    return(y)
  }
  
  df<-reactive({
    switch(input$dataSelect,
           `7`={df<-data.frame(rbind(new_df_all(1),new_df_all(2),new_df_all(3),new_df_all(4),new_df_all(5),new_df_all(6)),stringsAsFactors=TRUE)
           },
           {df<-data.frame(new_df(as.numeric(input$dataSelect)))
           }
    )
    df$Response<-factor(df$Response)
    levels(df$Response)<-str_wrap(levels(df$Response),width=35)
    if(!is.null(df$Sub_question)){
      df$Sub_question<-factor(df$Sub_question)
      levels(df$Sub_question)<-str_wrap(levels(df$Sub_question),width=40)
    }
    if(is.null(df$Sub_question) & !is.null(crosstabs())){
        levels(df[[1]])<-str_wrap(levels(df[[1]]),width=36)
    }
    df$Percentage[is.nan(df$Percentage)]<-0
    df<-df %>% drop_na() %>% select(any_of(c(crosstabs(),"Year","Sub_question","Response","Percentage")))
    return(df)
  })
  
  ##### #####
  
  ###################### G R A P H ######################
  ## create graph ##
  plot<-reactive({
    if(input$dataSelect!=7){    
      if(dataStore$dataLoc[[1]][1] %in% names(multipart_stacked)){
        plot<-ggplot(df(), aes(x=Sub_question,y=Percentage,fill=Response)) +
          ylab("Percentage") + xlab("") + plot_theme +
          {if(is.null(crosstabs())) geom_bar_interactive(aes(tooltip = paste0("<strong>Response:</strong> ",HTMLencode(Response),"<br> <strong>Percentage:</strong> ", round(Percentage,2),"%")),stat="identity",size=2)} +
          {if(!is.null(crosstabs())) geom_bar_interactive(aes(tooltip = paste0("<strong>Crosstab:</strong> ",get(crosstabs()),"<br><strong>Category:</strong> ",HTMLencode(Sub_question),"<br><strong>Response:</strong> ",HTMLencode(Response),"<br><strong>Percentage:</strong> ", round(Percentage,2),"%")),stat="identity",size=2)} +
          {if(!is.null(crosstabs())) facet_grid(str_wrap(get(crosstabs()),1)~.)} +
          scale_fill_manual(values=palette[1:length(levels(df()$Response))],name="Legend") +
          scale_y_continuous(limits=c(0,NA),expand=expand_scale(add=c(0,3.25))) +
          guides(fill=guide_legend(ncol=1))          
      } else {
        plot<-ggplot(df(), aes(x=Response,y=Percentage)) +
          ylab("Percentage") + xlab("") + plot_theme +
          geom_vline(xintercept=distinct(df(),as.numeric(Response))[1:n_distinct(df()$Response)-1,]+.5,color="grey",size=.25,linetype="dotted") +
          {if(is.null(crosstabs())) geom_bar_interactive(aes(tooltip = paste0("<strong>Response:</strong> ",HTMLencode(Response),"<br> <strong>Percentage:</strong> ", round(Percentage,2),"%"),fill=Response),stat="identity", size=2)} +
          {if(!is.null(crosstabs())) geom_bar_interactive(aes(tooltip=paste0("<strong>Crosstab:</strong> ",get(crosstabs()),"<br><strong>Response:</strong> ",HTMLencode(Response),"<br> <strong>Percentage:</strong> ", round(Percentage,2),"%"),fill=get(crosstabs())),stat="identity",size=1.5,position="dodge")} +
          {if(dataStore$dataLoc[[1]][1] %in% names(multipart)) facet_grid(Sub_question~.)} +
          scale_fill_manual(values=palette[1:length(levels(df()[[1]]))],name="Legend") +
          scale_x_discrete() +
          scale_y_continuous(limits=c(0,NA),expand=expand_scale(add=c(0,3.25))) +
          guides(fill=guide_legend(ncol=1))
      }
    } else if(input$dataSelect==7){
      plot<-ggplot(df(),aes(y=Percentage,x=Year,group=Response,color=Response)) + 
      {if(!is.null(crosstabs())) facet_grid(get(crosstabs())~.)} +
        geom_line_interactive(position="identity") +
        {if(is.null(crosstabs())) geom_point_interactive(aes(tooltip=paste0("<strong>Response:</strong> ",HTMLencode(Response),"<br><strong>Year:</strong> ", Year,"<br><strong>Percentage:</strong> ",round(Percentage,2),"%")),position="identity",size=1)} +
        {if(!is.null(crosstabs())) geom_point_interactive(aes(tooltip=paste0("<strong>Crosstab:</strong> ",get(crosstabs()),"<br><strong>Response:</strong> ",HTMLencode(Response),"<br><strong>Year:</strong> ", Year,"<br><strong>Percentage:</strong> ",round(Percentage,2),"%")),position="identity",size=1)} +
        scale_x_continuous(limits=c(2004,2017),breaks=c(2004,2006,2008,2011,2014,2017)) +
        scale_y_continuous(limits=c(0,NA),expand=expand_scale(add=c(0,3.25))) +
        plot_theme + theme(axis.text.x=element_text(angle=0,hjust=.50)) + guides(color=guide_legend(ncol=1))
    }
  })
  
  output$girafe<-renderGirafe({
    girafe_options(girafe(ggobj=plot()),width_svg=6,height_svg=4,opts_zoom(.5,2),opts_toolbar(saveaspng=FALSE),opts_sizing(rescale=FALSE,width=1),opts_tooltip(css="font-family:arial;font-size:12px;background-color:#ffffff;padding:5px;border-radius:7px;box-shadow:2px 2px #555555;"))
  })
  
  output$noGraph<-renderUI({if(nrow(df())==0){h4(strong("The selected question was not asked in this country."))}})

  #### print out additional info ####
  note_display<-reactive({as.character(notes[(grep(dataStore$dataLoc[1],notes[,1])),2])})
  output$exp_notes<-renderUI({
    if(dataStore$dataLoc[1] %in% notes[,1]){
      wellPanel(strong(h5(note_display())))
    }
  })  
  survey_info<-reactive({as.character(dates[as.numeric(input$dataSelect),2])})
  output$survey_info<-renderUI({wellPanel(HTML(survey_info()))})
  
  ## return to questions list ##
  observeEvent(input$return,{updateTabsetPanel(session,"Questions","Questions") })

  #### download handler for .png ####
  output$savePNG<-downloadHandler(filename=function(){
    paste0("TRIP_Faculty_Survey",format(Sys.Date(),"%Y_%b_%d"),".png")},
  content=function(file){
    ggsave(file,plot() + ggtitle(str_wrap(paste(qid()[,1][as.numeric(dataStore$dataNum)]),75)) + theme(plot.margin=unit(c(4,2,0,1),"points"),plot.title=element_text(size=9)),"png",width=9,height=6,dpi=300)
  })
  
  ## print question as title ##
  output$selectedQ<-renderText({paste(qid()[,1][as.numeric(dataStore$dataNum)])})
 
#### generate table to print below graph ####
  observeEvent(input$toggle,{
    if(input$toggle %% 2==1){
      output$table<-renderUI({
        output$display<-renderTable({
          if(!is.null(crosstabs())){
            if(input$dataSelect!=7){
              df() %>% distinct() %>% mutate(Percentage=paste0(Percentage,"%")) %>% 
                pivot_wider(names_from=crosstabs(),values_from=Percentage) %>% 
                {if(is.null(df()$Sub_question)) group_by(.,Response) %>% summarise_all(coalesce_by_column) %>% select(Response,everything()) else group_by(.,Response,Sub_question) %>% summarise_all(coalesce_by_column) %>% select(Sub_question,Response,everything()) %>% arrange(.,Sub_question)}
            } else if(input$dataSelect==7){
              df() %>% distinct() %>% mutate(Percentage=paste0(Percentage,"%"),Year=as.character(Year)) %>% 
                pivot_wider(names_from=crosstabs(),values_from=Percentage) %>% group_by(Response,Year) %>% summarise_all(coalesce_by_column) %>% mutate_at(vars(-Response,-Year),replace_na,"0%") %>% arrange(-desc(Year))
            }
          } else {
            if(input$dataSelect!=7){
            df() %>% mutate(Percentage=paste0(Percentage,"%")) %>% 
            {if(is.null(df()$Sub_question)) select(.,Response,Percentage) else arrange(.,Sub_question) %>% select(.,Sub_question,Response,Percentage)}
            } else if(input$dataSelect==7){
              df() %>% distinct() %>% mutate(Percentage=paste0(Percentage,"%"),Year=as.character(Year)) %>% group_by(Year,Response) %>% summarise_all(coalesce_by_column) %>% mutate_at(vars(Percentage),replace_na,"0%")
            }
          }
        }, width="95%",hover=TRUE)
        tableOutput("display")
      }) 
      updateActionButton(session,"toggle","Remove table")
    } else if(input$toggle %% 2!=1){
      removeUI(selector="#display")
      updateActionButton(session,"toggle","View as table") 
    }
  })
  
  #### bookmarking for url ####
  outputOptions(output,"questions",suspendWhenHidden=FALSE)
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  
  onBookmarked(function(url){updateQueryString(url)})
  observeEvent(input$clipbtn, {showModal(urlModal(URL(), title="Copy URL to clipboard"))})
  URL <- reactiveVal()
  onBookmarked(function(url){URL(url)})
  onRestore(function(state){updateTabsetPanel(session,"Questions","Questions")})
  #### ####
  
  output$error<-renderText({
    print(names(df()))
    str(df())
  })
}

enableBookmarking(store = "url")
shinyApp(ui, server)

