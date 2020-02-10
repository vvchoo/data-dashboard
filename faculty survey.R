# This does XXXXX
# Code developed by Vera Choo, with reference from: XXXX

library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(rsconnect)
library(colorspace)
library(ggiraph)

#rsconnect::setAccountInfo(name='vvchoo', token='20318BE8E51FD829619E1D7095A496CB', secret='VUQ9WQai6Vt1CftmuLpcaxg5j3pHNgYReyWhCGZK')

surveys<-readRDS(url("https://www.dropbox.com/s/tqz1oi33powdy95/surveys.rds?dl=1"))

# create codebook #
qid_list<-readRDS(url("https://www.dropbox.com/s/3j0vn6fdq9rojkv/qid_list.rds?dl=1"))
commas<-readRDS(url("https://www.dropbox.com/s/6lq2la4n47yx2v3/commas.rds?dl=1"))
multipart<-readRDS(url("https://www.dropbox.com/s/1ysmiq24hgydfkc/multipart.rds?dl=1"))
palette<-readRDS(url("https://www.dropbox.com/s/y4paezgfjk8ddcm/palette.rds?dl=1"))

`%notin%` <- Negate(`%in%`)
plot_theme<-theme_bw() +
  theme(axis.text.x=element_text(angle=310,vjust=1,hjust=0,size=7),axis.ticks.y=element_blank(),axis.ticks.x=element_line(size=.25),axis.text.y=element_text(size=7.5), axis.title.x=element_text(vjust=0),axis.title.y=element_text(size=8),panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),legend.position="right",legend.direction="vertical",legend.key.width=unit(1.5, "points"),legend.key.height=unit(1.5, "points"),legend.margin=margin(0),legend.justification="top",legend.box.margin=margin(0),legend.text=element_text(size=7.5),legend.title=element_text(size=7.5),plot.margin=margin(0,0,0,0),strip.background=element_blank(),strip.text=element_text(size=7))

#######################################################
#                   USER INTERFACE                    #
#######################################################
ui <- function(req){
  fluidPage(
    tags$head(
      tags$title("TRIP Faculty Survey"),
      tags$link(rel="me", href="https://twitter.com/trip_irsurvey"),
      tags$style(HTML("
                      text{font-family: sans-serif}
                      .action-button{padding:10px;margin-top:-12px;display:block;background-color:#e1eaf0;transition:background-color .3s, color .3s;border-radius:5px;}
                      .action-button:hover{background-color:#edf2f5;transition:background-color .3s, color .3s;text-decoration:none;}
                      .well{background-color:#e1eaf0;}
                      #return{background-color:#f2f9fc;transition:background-color .3s, color .3s;border-color:#ffffff;width:100%;font-weight:bold;overflow-wrap:break-word;}
                      #return:hover{background-color:#fafdff;transition:background-color .3s, color .3s;}
                      #tweet{background-color:#1DA1F2;color:#ffffff;font-weight:bold;width:100%;}
                      #tweet:hover{background-color:#65bff6;transition:background-color .3s, color .3s;}
                      
                      #clipbtn{background-color:#fff;font-weight:bold;width:100%;}
                      #clipbtn:hover{background-color:#fafdff;transition: background-color .3s, color .3s;}
                      #savePNG{background-color:#fff;font-weight:bold;width:100%;padding:10px;}
                      #savePNG:hover{background-color:#fafdff;transition: background-color .3s, color .3s;padding:10px;}
                      "))
      ),
    titlePanel(h1(strong("Faculty Survey"))),
    tabsetPanel(id="Questions",
                tabPanel("Questions",
                         br(),
                         sidebarPanel(selectInput("dataSelect","Choose Survey Year:",
                                                  choices=c("2004 Faculty Survey"=1,"2006 Faculty Survey"=2,"2008 Faculty Survey"=3,"2011 Faculty Survey"=4,"2014 Faculty Survey"=5,"2017 Faculty Survey"=6,"All years (selected questions)"=7)), # updates go here
                                      br(),
                                      uiOutput("section")),
                         mainPanel(uiOutput("sectionName"),br(),uiOutput("questions"),tableOutput("print"))),
                tabPanel("Graph",
                         fluidRow(column(3,br(),
                                         wellPanel(
                                           uiOutput("countryList"),br(),
                                           selectInput("crosstabList","Crosstab:",
                                                       c("Select..."="NULL","All respondents"="NO","Gender"="gender","Age"="age","Rank"="rank","Area of study"="subfield")),br(),
                                           #radioButtons("labels","Percentage labels",c("Off","On"),inline=TRUE),br(),
                                           downloadButton("savePNG","Save as .PNG"),br(),br(),
                                           actionButton("clipbtn","Copy URL",icon("clipboard")),br(),
                                           actionButton("tweet","Tweet",onclick="window.open('http://twitter.com/intent/tweet?text=' + encodeURIComponent(window.location.href))"),br(),
                                           actionButton("return","Return to question list"))),
                                  column(9,
                                         strong(h3(textOutput("selectedQ"))),br(),
                                         uiOutput("noGraph"),
                                         girafeOutput("girafe",width="99%",height="725px"),
                                         textOutput("error"))))),
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
        selectInput("countries","Choose Survey Country:",c("All Countries",names(table(surveys[[as.numeric(input$dataSelect)]]$surveyCountry)[table(surveys[[as.numeric(input$dataSelect)]]$surveyCountry)!=0])))
      }
    })
  })
  
  countryFilter<-reactive({
    if(input$countries=="All Countries" & input$dataSelect!=7){
      countryFilter<-c(names(table(surveys[[as.numeric(input$dataSelect)]]$surveyCountry)[table(surveys[[as.numeric(input$dataSelect)]]$surveyCountry)!=0]))
    } else if(input$countries=="All Countries" & input$dataSelect==7){
      countryFilter<-c(names(table(surveys[[6]]$surveyCountry)[table(surveys[[6]]$surveyCountry)!=0]))
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
        z<-data.frame(surveys[[x]] %>% select(crosstabs(),Response=dataStore$dataLoc[[1]][1]))
        z<-z %>% drop_na() %>% separate(Response,c(paste("Response",1:len,sep="_")),sep=",") %>% group_by_at(crosstabs()) %>% mutate(total_n=n()) %>% gather(key,Response,Response_1:paste0("Response_",len)) %>% select(-key) %>% group_by_all() %>% mutate(Percentage=round(n()/total_n*100,2)) %>% distinct(crosstabs(),Response,.keep_all=TRUE) %>% group_by_at(crosstabs()) %>% top_n(8,Percentage) %>% droplevels()
        z
    } else if(length(dataStore$dataLoc[[1]])==2 && dataStore$dataLoc[[1]][1] %in% names(multipart)){
        len<-length(multipart[[grep(dataStore$dataLoc[[1]][1],names(multipart))]])
        z<-data.frame(surveys[[x]] %>% select(dataStore$dataLoc[[1]][1]:dataStore$dataLoc[[1]][2],crosstabs()))
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
    } else if(length(dataStore$dataLoc[[1]])==1){
        data.frame(surveys[[x]] %>% select(crosstabs(),Response=dataStore$dataLoc[[1]][1]) %>% filter(!is.na(Response)) %>% drop_na() %>% group_by_all(.drop=FALSE) %>% summarize(n=n()) %>% mutate(Percentage=round(n/sum(n)*100,2))) %>% group_by_at(crosstabs()) %>% top_n(8,Percentage) %>% droplevels()
    } else if(length(dataStore$dataLoc[[1]])==2){
        data.frame(surveys[[x]] %>% select(crosstabs(),dataStore$dataLoc[[1]][1]:dataStore$dataLoc[[1]][2]) %>% group_by_at(crosstabs()) %>% mutate(total_n=n()) %>% gather(key,value,dataStore$dataLoc[[1]][1]:dataStore$dataLoc[[1]][2]) %>% select(crosstabs(), Response=value,total_n) %>% filter(!is.na(Response)) %>% drop_na() %>% group_by_all(.drop=FALSE) %>% summarize(n=n()) %>% mutate(Percentage=round(n/total_n*100,2)) %>% select(-total_n)) %>% group_by_at(crosstabs()) %>% top_n(8,Percentage)
    }
    return(y)
  }
  
  new_df_all<-function(x){
    y <- if(length(dataStore$dataLoc[[x]])==1 && dataStore$dataLoc[[x]][1] %in% commas[[1]]){ #multiselect Qs
      len<-commas[grep(dataStore$dataLoc[[x]][1],commas[,1]),2]
      z<-data.frame(surveys[[x]] %>% filter(surveyCountry %in% countryFilter()) %>% select(crosstabs(),Response=dataStore$dataLoc[[x]][1],surveyId)) %>% drop_na(Response) %>% group_by_at(crosstabs()) %>% mutate(total_n=n_distinct(surveyId)) %>% separate(Response,paste0("Response_",1:len),",") %>% gather(key,Response,Response_1:paste0("Response_",len)) %>% group_by_at(vars(crosstabs(),res())) %>% mutate(n=n(),Percentage=round(n/total_n*100,2), year=surveys[[x]]$surveyYear[1]) %>% drop_na() %>% distinct_at(vars(crosstabs(),Response,Percentage,year)) %>% group_by_at(crosstabs()) %>% top_n(8,Percentage) %>% droplevels()
      z
    } else if(length(dataStore$dataLoc[[x]])==1 && dataStore$dataLoc[[x]][1] %notin% commas[[1]]){
      data.frame(surveys[[x]] %>% filter(surveyCountry %in% countryFilter()) %>% select(crosstabs(),Response=dataStore$dataLoc[[x]][1],surveyId) %>% filter(!is.na(Response)) %>% group_by_at(crosstabs()) %>% mutate(total_n=n()) %>% group_by_at(vars(crosstabs(),res())) %>% mutate(n=n(), Percentage=round((n/total_n)*100,2),year=surveys[[x]]$surveyYear[1])) %>% distinct_at(vars(crosstabs(),Response,Percentage,year)) %>% group_by_at(crosstabs()) %>% top_n(8,Percentage) %>% drop_na %>% droplevels()
    } else {
      data.frame(surveys[[x]] %>% filter(surveyCountry %in% countryFilter()) %>% select(crosstabs(),dataStore$dataLoc[[x]][1]:dataStore$dataLoc[[x]][2],surveyId) %>% group_by_at(crosstabs()) %>% mutate(total_n=n()) %>% gather(key,value,dataStore$dataLoc[[x]][1]:dataStore$dataLoc[[x]][2]) %>% select(crosstabs(),Response=value,total_n) %>% group_by_all() %>% summarize(n=n()) %>% mutate(Percentage=round(n/total_n*100,2)),year=surveys[[x]]$surveyYear[1]) %>% drop_na() %>% group_by_at(crosstabs()) %>% top_n(10,Percentage) %>% droplevels()
    }
    return(y)
  }
  
  df<-reactive({
    if(input$dataSelect==7){
      df<-data.frame(rbind(new_df_all(1),new_df_all(2),new_df_all(3),new_df_all(4),new_df_all(5),new_df_all(6)),stringsAsFactors=TRUE)
    } else {
      df<-data.frame(new_df(as.numeric(input$dataSelect)))
    }
    levels(df$Response)<-gsub("'","'",levels(df$Response))
    levels(df$Response)<-gsub("'"," ",levels(df$Response))
    levels(df$Response)<-gsub("'"," ",levels(df$Response))
    levels(df$Response)<-gsub("[\\]'","'",levels(df$Response))
    df$Response<-factor(df$Response)
    levels(df$Response)<-str_wrap(levels(df$Response),width=35)
    if(!is.null(df$sub_question)){
      levels(df$sub_question)<-gsub("'","'",levels(df$sub_question))
      levels(df$sub_question)<-gsub("'"," ",levels(df$sub_question))
      levels(df$sub_question)<-gsub("'"," ",levels(df$sub_question))
      df$sub_question<-factor(df$sub_question)
      levels(df$sub_question)<-str_wrap(levels(df$sub_question),width=40)}
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
    if(input$dataSelect!=7){    
      if(dataStore$dataLoc[[1]][1] %in% names(multipart)){
        if(is.null(crosstabs())){
          plot<-ggplot(df(), aes(x=sub_question,fill=Response)) +
            ylab("Percentage") + xlab("") + plot_theme + 
            geom_bar_interactive(aes(tooltip = paste0("<strong>Response:</strong> ",enc2utf8(as.character(Response)),"<br> <strong>Percentage:</strong> ", round(Percentage,2),"%"),y=Percentage),stat="identity",size=2, show.legend=TRUE,drop=FALSE) +
            #{if(input$labels=="On") geom_text(aes(y=Percentage,label=paste0(Percentage,"%"),x=Response),size=2.5,vjust=-.5,color="#959595")} +
            scale_fill_manual(values=palette[1:length(levels(df()[[1]]))],name="Legend") +
            #scale_fill_discrete_sequential(palette="SunsetDark",name="Legend") +
            scale_y_continuous(limits=c(0,NA),expand=expand_scale(add=c(0,3.25))) +
            guides(fill=guide_legend(ncol=1))   
        } else if(!is.null(crosstabs())){
          plot<-ggplot(df(), aes(x=sub_question,fill=Response)) +
            ylab("Percentage") + xlab("") + plot_theme + facet_grid(~get(crosstabs())) +
            geom_bar_interactive(aes(tooltip = paste0("<strong>Crosstab:</strong> ",get(crosstabs()),"<br><strong>Category:</strong> ",sub_question,"<br><strong>Response:</strong> ",enc2utf8(as.character(Response)),"<br><strong>Percentage:</strong> ", round(Percentage,2),"%"),y=Percentage),stat="identity",size=2, show.legend=TRUE,drop=FALSE) +
            #{if(input$labels=="On") geom_text(aes(y=Percentage,label=paste0(Percentage,"%"),x=Response),size=2.5,vjust=-.5,color="#959595")} +
            scale_fill_manual(values=palette[1:length(levels(df()[[1]]))],name="Legend") +
            #scale_fill_discrete_sequential(palette="SunsetDark",name="Legend") +
            scale_y_continuous(limits=c(0,NA),expand=expand_scale(add=c(0,3.25))) +
            guides(fill=guide_legend(ncol=1))          
        }
      } else {
        if(is.null(crosstabs())){
          plot<-ggplot(df(), aes(x=Response,fill=Response)) +
            ylab("Percentage") + xlab("") + plot_theme +
            geom_bar_interactive(aes(tooltip = paste0("<strong>Response:</strong> ",enc2utf8(as.character(Response)),"<br> <strong>Percentage:</strong> ", round(Percentage,2),"%"),y=Percentage),stat="identity", size=2, show.legend=TRUE) +
            #{if(input$labels=="On") geom_text(aes(y=Percentage,label=paste0(Percentage,"%"),x=Response),size=2.5,vjust=-.5,color="#959595")} +
            scale_fill_manual(values=palette[1:length(levels(df()[[1]]))],name="Legend") +
            #scale_fill_discrete_sequential(palette="SunsetDark",name="Legend",drop=FALSE) +
            scale_x_discrete() +
            scale_y_continuous(limits=c(0,NA),expand=expand_scale(add=c(0,3.25))) +
            guides(fill=guide_legend(ncol=1))
        } else if(!is.null(crosstabs())){
            plot<-ggplot(df(), aes(x=Response,fill=get(crosstabs()))) +
              ylab("Percentage") + xlab("") + plot_theme + 
              geom_bar_interactive(aes(tooltip=paste0("<strong>Crosstab:</strong> ",get(crosstabs()),"<br><strong>Response:</strong> ",enc2utf8(as.character(Response)),"<br> <strong>Percentage:</strong> ", round(Percentage,2),"%"),y=Percentage),stat="identity",size=1.5,show.legend=TRUE,position="dodge") + 
              #{if(input$labels=="On") geom_text(aes(y=Percentage,label=paste0(Percentage,"%"),x=Response,group=factor(get(crosstabs()))),position=position_dodge(width=1),size=2.5,color="#959595",angle=90,hjust=-.15)} +
              scale_fill_manual(values=palette[1:length(levels(df()[[1]]))],name="Legend") +
              #scale_fill_discrete_sequential(palette="SunsetDark",name="Legend",drop=FALSE) +
              scale_x_discrete() +
              scale_y_continuous(limits=c(0,NA),expand=expand_scale(add=c(0,3.25))) +
              guides(fill=guide_legend(ncol=1))          
        }
      }
    } else if(input$dataSelect==7){
      if(is.null(crosstabs())){
        plot<-ggplot(df(),aes(x=year,color=Response)) +
          geom_line_interactive(aes(y=Percentage,x=(year),group=Response),position="identity") +
          geom_point_interactive(aes(tooltip=paste0("<strong>Response:</strong> ",enc2utf8(as.character(Response)),"<br><strong>Year:</strong> ", year,"<br><strong>Percentage:</strong> ",round(Percentage,2),"%"),y=Percentage,x=year,group=Response),position="identity",size=1) +
          #{if(input$labels=="On") geom_text(aes(y=Percentage,label=paste0(Percentage,"%"),x=year),size=2.5,vjust=-.5,color="#959595")} +
          #scale_color_manual(values=palette[1:length(levels(df()[[1]]))],name="Legend") +
          scale_color_discrete_sequential(palette="SunsetDark",name="Legend",labels) +
          scale_x_continuous(limits=c(2004,2017),breaks=c(2004,2006,2008,2011,2014,2017)) +
          scale_y_continuous(limits=c(0,NA),expand=expand_scale(add=c(0,3.25))) +
          plot_theme + theme(axis.text.x=element_text(angle=0,hjust=.50)) + guides(color=guide_legend(ncol=1))
      } else if(!is.null(crosstabs())) {
        plot<-ggplot(df(),aes(x=year,color=Response)) + facet_grid(get(crosstabs())~.) +
          geom_line_interactive(aes(y=Percentage,x=(year),group=Response),position="identity") +
          geom_point_interactive(aes(tooltip=paste0("<strong>Crosstab:</strong> ",get(crosstabs()),"<br><strong>Response:</strong> ",enc2utf8(as.character(Response)),"<br><strong>Year:</strong> ", year,"<br><strong>Percentage:</strong> ",round(Percentage,2),"%"),y=Percentage,x=year,group=Response),position="identity",size=1) +
          #{if(input$labels=="On") geom_text(aes(y=Percentage,label=paste0(Percentage,"%"),x=year),size=2.5,vjust=-.5,color="#959595")} +
          #scale_color_manual(values=palette[1:length(levels(df()[[1]]))],name="Legend") +
          scale_color_discrete_sequential(palette="SunsetDark",name="Legend") +
          scale_x_continuous(limits=c(2004,2017),breaks=c(2004,2006,2008,2011,2014,2017)) +
          scale_y_continuous(limits=c(0,NA),expand=expand_scale(add=c(0,3.25))) +
          plot_theme + theme(axis.text.x=element_text(angle=0,hjust=.50)) + guides(color=guide_legend(ncol=1))
      }
    }
  })
  
  output$girafe<-renderGirafe({
    girafe_options(girafe(ggobj=plot()),width_svg=6,height_svg=4,opts_zoom(.5,2),opts_toolbar(saveaspng=FALSE),opts_sizing(rescale=FALSE,width=1),opts_tooltip(css="font-family:arial;font-size:12px;background-color:#ffffff;padding:5px;border-radius:7px;box-shadow:2px 2px #555555;"))
  })
  
  output$noGraph<-renderUI({ if(nrow(df())==0){h4(strong("The selected question was not asked in this country."))} })
  #### ####
  
  output$error<-renderText({
    str(df())
  })
  
  ## return to questions list ##
  observeEvent(input$return,{updateTabsetPanel(session,"Questions","Questions") })
  
  output$savePNG<-downloadHandler(filename=function(){
    paste0("TRIP_Faculty_Survey",format(Sys.Date(),"%Y_%b_%d"),".png")},
  content=function(file){
    ggsave(file,plot(),device="png",width=9,height=6,dpi=300)
  })
  
  ## print question as title ##
  output$selectedQ<-renderText({paste(qid()[,1][as.numeric(dataStore$dataNum)])})
  #### ####
  
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
}

enableBookmarking(store = "url")
shinyApp(ui, server)

