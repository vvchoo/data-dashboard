# This does XXXXX
# Code developed by Vera Choo, with reference from: XXXX

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rsconnect)
library(RCurl)
library(ggiraph)

JAD<-readRDS(url("https://www.dropbox.com/s/nx09f4imh6b02kg/JAD.rds?dl=1"))
JAD_copy<-readRDS(url("https://www.dropbox.com/s/lb8zo1c5s7qmn24/JAD_original.rds?dl=1"))
palette<-readRDS(url("https://www.dropbox.com/s/y4paezgfjk8ddcm/palette.rds?dl=1"))



#### set up plot theme ####
plot_theme<-theme_bw() +
  theme(axis.text.x=element_text(angle=310,vjust=1,hjust=0,size=9),axis.ticks.y=element_blank(),axis.ticks.x=element_line(size=.25),axis.text.y=element_text(size=9), axis.title.x=element_text(vjust=0),axis.title.y=element_text(size=9),panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),legend.position="right",legend.direction="vertical",legend.key.width=unit(1.5, "points"),legend.key.height=unit(1.5, "points"),legend.margin=margin(0),legend.justification="top",legend.box.margin=margin(0),legend.text=element_text(size=9),legend.title=element_text(size=9),plot.margin=margin(0,60,0,1),strip.background=element_blank(),strip.text=element_text(size=9))


######################################################################################
#                                    USER INTERFACE                                  #
######################################################################################
ui <- function(req){
  fluidPage(
    tags$head(
      tags$title("TRIP Journal Article Database"),
      tags$style(HTML("
                      text{font-family: sans-serif}
                      .well{background-color:#e1eaf0;}
                      #download{font-weight:bold;width:100%;margin-top:10px;}
                      #download:hover{background-color:#edf2f5;transition: background-color .3s, color .3s;}
                      #downloadPlot{font-weight:bold;width:100%;margin-top:10px;}
                      #downloadPlot:hover{background-color:#edf2f5;transition: background-color .3s, color .3s;}
                      #clipbtn{background-color:#fff;font-weight:bold;width:100%;margin-top:10px;}
                      #clipbtn:hover{background-color:#edf2f5;transition: background-color .3s, color .3s;}
                      #tweet{background-color:#1DA1F2;color:#ffffff;font-weight:bold;width:100%;margin-top:10px;}
                      #tweet:hover{background-color:#65bff6;transition: background-color .3s, color .3s;}
                      #removeFocus{float:right;margin-bottom:5px;}
                      #removeTime{float:right;margin-bottom:5px;}
                      #removeMethod{float:right;margin-bottom:5px;}
                      #removeIdea{float:right;margin-bottom:5px;}
                      #removeIssue{float:right;margin-bottom:5px;}
                      #removeContemp{float:right;margin-bottom:5px;}
                      #removePolicy{float:right;margin-bottom:5px;}
                      #removeRegion{float:right;margin-bottom:5px;}
                      #removeLevel{float:right;margin-bottom:5px;}
                      #removeMaterial{float:right;margin-bottom:5px;}
                      #removeEpist{float:right;margin-bottom:5px;}
                      #removePara{float:right;margin-bottom:5px;}
                      
        "))
      ),
    titlePanel(h1(strong("Journal Article Database")),
               windowTitle="TRIP Journal Article Database"),
    br(),
    fluidRow(
      column(4,
             wellPanel(
               tags$div(id="yearFilter",
                        h4("Show by:"),
                        radioButtons("percentInput","Frequency or Percentage",choices=c("Frequency","Percentage"),inline=TRUE),
                        radioButtons("ygInput","X-axis: Year or Variable",c("Year","Variable"),inline=TRUE))),
             wellPanel(
               tags$div(id="newFilter",
                        h4("See only articles which are:"),
                        selectInput("addFilter","Filter choice",choices=c("Select...","Contemporary","Epistemology","Substantive Focus","Ideational","Issue Area","Level of Analysis","Material","Methodology","Paradigm","Policy Prescription","Region","Time Period")),
                        uiOutput("epistFilter"),uiOutput("contempFilter"),uiOutput("focusFilter"),uiOutput("ideaFilter"),uiOutput("issueFilter"),uiOutput("levelFilter"),uiOutput("materialFilter"),uiOutput("methodFilter"),uiOutput("paradigmFilter"),uiOutput("policyFilter"),uiOutput("regionFilter"),uiOutput("timeFilter"))),
             wellPanel(
               h4("Article year range:"),
               sliderInput("yearInput","Year",1980,2017,c(1980,2017),sep="")), #change when updated),
             wellPanel(
               h4("Share your data:"),
               actionButton("clipbtn","Copy URL",icon("clipboard")),br(),
               strong(a(actionButton("tweet","Tweet", onclick="window.open('http://twitter.com/intent/tweet?text=' + encodeURIComponent(window.location.href))"))),
               downloadButton("download","Download as .CSV"),
               downloadButton("downloadPlot","Save plot as .PNG"),br()
             )),
      column(8,verticalLayout(
        tableOutput("view"),
        girafeOutput("girafe",width="99%",height="600px")))),
    tags$style(type="text/css", ".shiny-output-error{visibility: hidden;}", ".shiny-output-error:before{visibility:hidden;}"))
  }

#######################################################################################
#                                       SERVER                                        #
#######################################################################################
server <- function(input, output, session) {
  #### add new filter button ####  
  observeEvent(input$addFilter, {
    if(input$addFilter=="Policy Prescription"){
      output$policyFilter<-renderUI({
        tagList(tags$div(id="policyCont",
                         actionButton("removePolicy","Remove"),  
                         selectInput("policyFilter", "Policy Prescription",multiple=TRUE,choices=c("Yes","No"))))})
    } else if(input$addFilter=="Contemporary"){
      output$contempFilter<-renderUI({
        tagList(tags$div(id="contempCont",
                         actionButton("removeContemp","Remove"),  
                         selectInput("contempFilter", "Contemporary",multiple=TRUE,choices=c("Yes","No"))))})
    } else if(input$addFilter=="Level of Analysis"){
      output$levelFilter<-renderUI({
        tagList(tags$div(id="levelCont",
                         actionButton("removeLevel","Remove"),  
                         selectInput("levelFilter", "Level of Analysis",multiple=TRUE,choices=c(names(table(JAD$Level))))))})
    } else if(input$addFilter=="Ideational"){
      output$ideaFilter<-renderUI({
        tagList(tags$div(id="ideaCont",
                         actionButton("removeIdea","Remove"),  
                         selectInput("ideaFilter", "Ideational",multiple=TRUE,choices=c("Yes","No"))))})
    } else if(input$addFilter=="Issue Area"){
      output$issueFilter<-renderUI({
        tagList(tags$div(id="issueCont",
                         actionButton("removeIssue","Remove"),  
                         selectInput("issueFilter", "Issue Area",multiple=TRUE,choices=c(names(table(JAD$IssueArea))))))})
    } else if(input$addFilter=="Region"){
      output$regionFilter<-renderUI({
        tagList(tags$div(id="regionCont",
                         actionButton("removeRegion","Remove"),  
                         selectInput("regionFilter", "Region",multiple=TRUE,choices=c(names(table(JAD$Region))))))})
    } else if(input$addFilter=="Material"){
      output$materialFilter<-renderUI({
        tagList(tags$div(id="materialCont",
                         actionButton("removeMaterial","Remove"),  
                         selectInput("materialFilter","Material",multiple=TRUE,choices=c("Yes","No"))))})
    } else if(input$addFilter=="Methodology"){
      output$methodFilter<-renderUI({
        tagList(tags$div(id="methodCont",
                         actionButton("removeMethod","Remove"),  
                         selectInput("methodFilter", "Methodology",multiple=TRUE,choices=c(names(table(JAD$Methodology))))))})
    } else if(input$addFilter=="Time Period"){
      output$timeFilter<-renderUI({
        tagList(tags$div(id="timeCont",
                         actionButton("removeTime","Remove"),  
                         selectInput("timeFilter", "Time Period",multiple=TRUE,choices=c(names(table(JAD$TimePeriod))))))})
    } else if(input$addFilter=="Substantive Focus"){ 
      output$focusFilter<-renderUI({
        tagList(tags$div(id="focusCont",
                         actionButton("removeFocus","Remove"),     
                         selectInput("focusFilter", "Substantive Focus",multiple=TRUE,choices=c(names(table(JAD$Focus))))))})
    } else if(input$addFilter=="Paradigm"){ 
      output$paradigmFilter<-renderUI({
        tagList(tags$div(id="paraCont",
                         actionButton("removePara","Remove"),     
                         selectInput("paradigmFilter", "Paradigm",multiple=TRUE,choices=c(names(table(JAD$Paradigm))))))})
    }else if(input$addFilter=="Epistemology"){ 
      output$epistFilter<-renderUI({
        tagList(tags$div(id="epistCont",
                         actionButton("removeEpist","Remove"),     
                         selectInput("epistFilter", "Epistemology",multiple=TRUE,choices=c(names(table(JAD$Epistemology))))))})
    }
  })
  
  observeEvent(input$removeEpist, {
    removeUI(selector="#epistCont")
    updateSelectInput(session, "addFilter", selected="Select...")
    updateSelectInput(session, "epistFilter", selected=character(0))
    session$sendCustomMessage(type="resetValue", message="epistFilter")
  })  
  observeEvent(input$removePara, {
    removeUI(selector="#paraCont")
    updateSelectInput(session, "addFilter", selected="Select...")
    updateSelectInput(session, "paradigmFilter", selected=character(0))
    session$sendCustomMessage(type="resetValue", message="paradigmFilter")
  })   
  observeEvent(input$removeFocus, {
    removeUI(selector="#focusCont")
    updateSelectInput(session, "addFilter", selected="Select...")
    updateSelectInput(session, "focusFilter", selected=character(0))
    session$sendCustomMessage(type="resetValue", message="focusFilter")
  })  
  observeEvent(input$removeTime, {
    removeUI(selector="#timeCont")
    updateSelectInput(session, "addFilter", selected="Select...")
    updateSelectInput(session, "timeFilter", selected=character(0))
    session$sendCustomMessage(type="resetValue", message="timeFilter")
  })  
  observeEvent(input$removeMethod, {
    removeUI(selector="#methodCont")
    updateSelectInput(session, "addFilter", selected="Select...")
    updateSelectInput(session, "methodFilter", selected=character(0))
    session$sendCustomMessage(type="resetValue", message="methodFilter")
  })  
  observeEvent(input$removeMaterial, {
    removeUI(selector="#materialCont")
    updateSelectInput(session, "addFilter", selected="Select...")
    updateSelectInput(session, "materialFilter", selected=character(0))
    session$sendCustomMessage(type="resetValue", message="materialFilter")
  })  
  observeEvent(input$removeRegion, {
    removeUI(selector="#regionCont")
    updateSelectInput(session, "addFilter", selected="Select...")
    updateSelectInput(session, "regionFilter", selected=character(0))
    session$sendCustomMessage(type="resetValue", message="regionFilter")
  })  
  observeEvent(input$removeIssue, {
    removeUI(selector="#issueCont")
    updateSelectInput(session, "addFilter", selected="Select...")
    updateSelectInput(session, "issueFilter", selected=character(0))
    session$sendCustomMessage(type="resetValue", message="issueFilter")
  })  
  observeEvent(input$removeIdea, {
    removeUI(selector="#ideaCont")
    updateSelectInput(session, "addFilter", selected="Select...")
    updateSelectInput(session, "ideaFilter", selected=character(0))
    session$sendCustomMessage(type="resetValue", message="ideaFilter")
  })  
  observeEvent(input$removeLevel, {
    removeUI(selector="#levelCont")
    updateSelectInput(session, "addFilter", selected="Select...")
    updateSelectInput(session, "levelFilter", selected=character(0))
    session$sendCustomMessage(type="resetValue", message="levelFilter")
  })  
  observeEvent(input$removeContemp, {
    removeUI(selector="#contempCont")
    updateSelectInput(session, "addFilter", selected="Select...")
    updateSelectInput(session, "contempFilter", selected=character(0))
    session$sendCustomMessage(type="resetValue", message="contempFilter")
  })
  observeEvent(input$removePolicy, {
    removeUI(selector="#policyCont")
    updateSelectInput(session, "addFilter", selected="Select...")
    updateSelectInput(session, "policyFilter", selected=character(0))
    session$sendCustomMessage(type="resetValue", message="policyFilter")
  })
  
  #### change x-axis filters ####
  observeEvent(input$ygInput, { 
    if(input$ygInput=="Variable"){
      insertUI(
        selector="#ygInput",
        where="afterEnd",
        ui=tags$div(id="varFilter",
                    selectInput("varFilter","Variable", c("Journal"="journal","Paradigm","Epistemology","Issue Area"="IssueArea","Time Period"="TimePeriod","Level","Methodology","Region","Substantive Focus"="Focus","Policy Prescription"="PolicyPrescription"))))
    } else {
      removeUI(selector="#varFilter")
    }
  })
  
  #### create filtered data set ####
  filtered<-reactive({
    data<-JAD
    data<-data %>% group_by(year) %>% mutate(year_distinct=n_distinct(pubID))  # total distinct articles a year #
    data<-data %>% ungroup() %>% mutate(var_distinct=n_distinct(pubID)) 
    
    ## filter conditions ##
    if(!is.null(input$contempFilter)){data<-data[data$Contemporary %in% input$contempFilter,]}
    if(!is.null(input$epistFilter)){data<-data[data$Epistemology %in% input$epistFilter,]}
    if(!is.null(input$focusFilter)){data<-data[data$Focus %in% input$focusFilter,]}
    if(!is.null(input$ideaFilter)){data<-data[data$Ideational %in% input$ideaFilter,]}
    if(!is.null(input$issueFilter)){data<-data[data$IssueArea %in% input$issueFilter,]}
    if(!is.null(input$levelFilter)){data<-data[data$Level %in% input$levelFilter,]}
    if(!is.null(input$methodFilter)){data<-data[data$Methodology %in% input$methodFilter,]}
    if(!is.null(input$materialFilter)){data<-data[data$Material %in% input$materialFilter,]}
    if(!is.null(input$regionFilter)){data<-data[data$Region %in% input$regionFilter,]}
    if(!is.null(input$timeFilter)){data<-data[data$TimePeriod %in% input$timeFilter,]}
    if(!is.null(input$paradigmFilter)){data<-data[data$Paradigm %in% input$paradigmFilter,]}
    if(!is.null(input$policyFilter)){data<-data[data$PolicyPrescription %in% input$policyFilter,]}
    
    data<-data %>% filter(year >= input$yearInput[1],year <= input$yearInput[2]) # change year range #
    
    pubIDs<-(data %>% distinct(pubID))[[1]]
    
    ## manipulate data for plotting ##
    if(input$percentInput=="Frequency" & input$ygInput=="Variable"){
      data<-data %>% distinct(pubID,.keep_all=TRUE) %>% group_by_at(input$varFilter,.drop=FALSE) %>% mutate(Count=n()) %>% select(Count,pubID) %>% distinct_at(vars(input$varFilter,Count),.keep_all=TRUE)}
    if(input$percentInput=="Frequency" & input$ygInput=="Year"){
      data<-data %>% distinct(pubID,.keep_all=TRUE) %>% group_by(year,.drop=FALSE) %>% mutate(Count=n()) %>% select(Count,pubID) %>% distinct(year,.keep_all=TRUE)}
    if(input$percentInput=="Percentage" & input$ygInput=="Year"){ 
      data<-data %>% distinct(pubID,.keep_all=TRUE) %>% group_by(year,.drop=FALSE) %>% mutate(Count=n()) %>% select(Count,year_distinct,pubID) %>% distinct(year,.keep_all=TRUE) %>% ungroup() %>% mutate(Percentage=Count/year_distinct*100)}
    if(input$percentInput=="Percentage" & input$ygInput=="Variable"){
      data<-data %>% distinct(pubID,.keep_all=TRUE) %>% group_by_at(input$varFilter,.drop=FALSE) %>% mutate(Count=n()) %>% distinct_at(vars(input$varFilter,Count,var_distinct),.keep_all=TRUE) %>% ungroup() %>% mutate(Percentage=Count/var_distinct*100)}
    
    list(data=as.data.frame(data),pubIDs=pubIDs)
  })
  
  #### create plot ####
  plot<-reactive({
    if(input$percentInput=="Percentage" & input$ygInput=="Variable"){
      ggplot(filtered()$data, aes_string(x=input$varFilter)) +
        ylab("Percentage") + xlab("") + plot_theme + 
        geom_bar_interactive(aes(tooltip = paste0("<strong>Variable:</strong> " , get(input$varFilter), "<br> <strong>Percentage:</strong> ", round(Percentage,2),"%"),y=Percentage),stat="identity", size=2, show.legend=TRUE,fill=palette[3]) +
        scale_y_continuous(expand=expand_scale(add=c(0,sqrt(max(filtered()$data$Count))))) +
        scale_x_discrete(drop=FALSE) +
        theme(legend.position="bottom")
    } else if (input$percentInput=="Frequency" & input$ygInput=="Variable"){
      ggplot(filtered()$data, aes_string(x=input$varFilter)) +
        ylab("Frequency") + xlab(paste(input$varFilter)) + plot_theme + 
        geom_bar_interactive(aes(tooltip=paste0("<strong>Variable:</strong> " ,get(input$varFilter), "<br> <strong>Count:</strong> ", round(Count,2)),y=Count),stat="identity", size=1, show.legend=TRUE,fill=palette[3]) +
        scale_y_continuous(expand=expand_scale(add=c(0,sqrt(max(filtered()$data$Count))))) +
        scale_x_discrete(drop=FALSE) +
        theme(legend.position="right",legend.direction="vertical",legend.key.size=unit(6.5,"points"),legend.margin=margin(0),legend.justification="top",legend.box.margin=margin(0),legend.text=element_text(size=7.5),legend.title=element_text(size=8)) +
        guides(fill=guide_legend(ncol=1))
    } else if (input$percentInput=="Percentage" & input$ygInput=="Year"){
      ggplot(filtered()$data, aes(x=year)) +
        scale_y_continuous(expand=expand_scale(add=c(0,sqrt(max(filtered()$data$Count))))) +
        scale_x_continuous(breaks=seq(1980,2017,5),labels=seq(1980,2017,5),name="Year") +
        ylab("Percentage") + xlab("Year") + plot_theme + 
        geom_bar_interactive(aes(tooltip = paste0("<strong>Year:</strong> " ,year, "<br> <strong>Percentage:</strong> ", round(Percentage,2),"%"),y=Percentage,fill=year),stat="identity", size=2, show.legend=TRUE,fill=palette[3]) +
        theme(legend.position="bottom")
    } else { # frequency by year #
      ggplot(filtered()$data, aes(x=year)) +
        scale_y_continuous(expand=expand_scale(add=c(0,sqrt(max(filtered()$data$Count))))) +
        scale_x_continuous(breaks=seq(1980,2017,5),labels=seq(1980,2017,5),name="Year") +
        ylab("Frequency") + xlab("Year") + plot_theme + 
        geom_bar_interactive(aes(tooltip = paste0("<strong>Year:</strong> " ,year, "<br> <strong>Count:</strong> ", round(Count,2)),y=Count,fill=year),stat="identity", size = 2, show.legend=TRUE,fill=palette[3]) +
        theme(legend.position="bottom")
    }
  })
  
  output$girafe<-renderGirafe({
    girafe_options(girafe(ggobj=plot()),width_svg=7,height_svg=6,opts_zoom(.5,2),opts_toolbar(saveaspng=FALSE),opts_sizing(rescale=FALSE,width=1),opts_tooltip(css="font-family:arial;font-size:12px;background-color:#ffffff;padding:5px;border-radius:7px;box-shadow:2px 2px #555555;"))
  })
  
  #### test printout #### 
  output$view<-renderText({
    }) 
  ########
  
  #### download csv file ####
  csvDownload<-reactive({
    data<-JAD_copy
    data<-data[data$pubID %in% filtered()$pubIDs,]
    data<-as.data.frame(data)
    data
  })
  
  output$download<-downloadHandler(filename=function(){
    paste("TRIP_JAD_", format(Sys.Date(), "%Y_%b_%d"), ".csv",sep="")
  },
  content=function(file){
    write.csv(csvDownload(), file, row.names=FALSE)
  })
  
  output$downloadPlot <- downloadHandler(filename=function(){
    paste("TRIP_PJD_", format(Sys.Date(), "%Y_%b_%d"),".png", sep="")
  },
  content=function(file){
    ggsave(file,plot(),device="png",width=6,height=3.5,dpi=300)
  })
  ########
  
  ## URL and bookmarking ##
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(function(url){updateQueryString(url)})
  observeEvent(input$clipbtn, {showModal(urlModal(URL(), title="Copy URL to clipboard"))})
  URL <- reactiveVal()
  onBookmarked(function(url){URL(url)})
  setBookmarkExclude("clipbtn")
}

enableBookmarking(store = "url")
shinyApp(ui, server)

