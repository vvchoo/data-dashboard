# This does XXXXX
# Code developed by Vera Choo, with reference from: XXXX

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rsconnect)
library(colorspace)
library(ggiraph)
#rsconnect::setAccountInfo(name='vvchoo', token='20318BE8E51FD829619E1D7095A496CB', secret='VUQ9WQai6Vt1CftmuLpcaxg5j3pHNgYReyWhCGZK')

PJD<-readRDS("pjd.rds")
PJD_copy<-readRDS("pjd_copy.rds")
palette<-readRDS("palette.rds")
p_store<-readRDS("p_store.rds")

## set up plot theme ##
plot_theme<-theme_bw() +
  theme(axis.text.x=element_text(angle=310,vjust=1,hjust=0,size=9),axis.ticks.y=element_blank(),axis.ticks.x=element_line(size=.25),axis.text.y=element_text(size=9), axis.title.x=element_text(vjust=0),axis.title.y=element_text(size=9),panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),legend.position="right",legend.direction="vertical",legend.key.width=unit(1.5, "points"),legend.key.height=unit(1.5, "points"),legend.margin=margin(0),legend.justification="top",legend.box.margin=margin(0),legend.text=element_text(size=9),legend.title=element_text(size=9),plot.margin=margin(1,60,2,1),strip.background=element_blank(),strip.text=element_text(size=9))


######################################################################################
#                                    USER INTERFACE                                  #
######################################################################################
ui <- function(req){
  fluidPage(
    tags$head(
      includeHTML("tracking.html"),
      tags$title("TRIP Policy Journal Database"),
      tags$style(HTML("
                      text{font-family: sans-serif}
                      .well{background-color:#F3F7FB;}
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
                      "))
      ),
    titlePanel(h1(strong("Policy Journal Database")),
               windowTitle="TRIP Policy Journal Database"),
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
                        selectInput("addFilter","Filter choice",choices=c("Select...","Contemporary","Substantive Focus","Ideational","Issue Area","Level of Analysis","Material","Methodology","Policy Prescription","Region","Time Period")),
                        uiOutput("contempFilter"),uiOutput("focusFilter"),uiOutput("ideaFilter"),uiOutput("issueFilter"),uiOutput("journalFilter"),uiOutput("levelFilter"),uiOutput("materialFilter"),uiOutput("methodFilter"),uiOutput("policyFilter"),uiOutput("regionFilter"),uiOutput("timeFilter"))),
             wellPanel(
               h4("Article year range:"),
               sliderInput("yearInput","Year",2000,2016,c(2000,2016),sep="")), #change when updated),
             wellPanel(
               textOutput("view"),
               h4("Share your data:"),
               actionButton("clipbtn","Copy URL",icon("clipboard")),br(),
               strong(a(actionButton("tweet","Tweet", onclick="window.open('http://twitter.com/intent/tweet?text=' + encodeURIComponent(window.location.href))"))),
               downloadButton("download","Download your data"),
               downloadButton("downloadPlot","Save plot as .PNG"),br()
             )),
      column(8,
        girafeOutput("girafe",width="99%",height="600px")#,
        #imageOutput("legend",width="800px")
        )),
    tags$style(type="text/css", ".shiny-output-error{visibility: hidden;}", ".shiny-output-error:before{visibility:hidden;}"),
    tags$script("
    Shiny.addCustomMessageHandler('resetValue', function(variableName) {
                Shiny.onInputChange(variableName, null);
});
                "))
}


#######################################################################################
#                                       SERVER                                        #
#######################################################################################
server <- function(input, output, session) {
  #### add new filter button ####  
  addInput<-function(id){
    ref<-p_store[p_store$names==id,]
    output[[paste0(ref$filter)]]<<-renderUI({
      tagList(tags$div(id=paste0(ref$container),
                       actionButton(paste0(ref$remove),"Remove"),
                       selectInput(paste0(ref$filter),paste0(id),multiple=TRUE,choices=unlist(strsplit(as.character(ref$choices),",")))))
    })
  }  
  observeEvent(input$addFilter, {addInput(input$addFilter)})
  
  #### remove filter buttons ####
  removeInput<-function(id){
    ref<-p_store[p_store$filter==id,]
    removeUI(selector=paste0("#",ref$container))
    updateSelectInput(session, "addFilter", selected="Select...")
    updateSelectInput(session, inputId=paste0(ref$filter), selected=character(0))
    session$sendCustomMessage(type="resetValue", paste0(ref$filter))
  }
  lapply(1:12, function(x){
    observeEvent(input[[paste0(p_store$remove)[x]]], {removeInput(p_store$filter[x])})
  })
  
  #### change x-axis filters ####
  observeEvent(input$ygInput, { 
    switch(input$ygInput,
           "Variable"={
              insertUI(
                selector="#ygInput",
                where="afterEnd",
                ui=tags$div(id="varFilter",
                            selectInput("varFilter","Variable", c("Journal"="journal","Issue Area"="IssueArea","Time Period"="TimePeriod","Contemporary","Level","Methodology","Region","Substantive Focus"="Focus","Policy Prescription"="PolicyPrescription"))))}, 
            "Year"={
              removeUI(selector="#varFilter")}
    )
  })
  
  #### create filtered data set ####
  filtered<-reactive({
    data<-PJD
    data<-data %>% group_by(year) %>% mutate(year_distinct=n_distinct(pubID))  # total distinct articles a year #
    data<-data %>% ungroup() %>% mutate(var_distinct=n_distinct(pubID)) 
    
    ## filter conditions ##
    if(!is.null(input$contempFilter)){data<-data[data$Contemporary %in% input$contempFilter,]}
    if(!is.null(input$focusFilter)){data<-data[data$Focus %in% input$focusFilter,]}
    if(!is.null(input$ideaFilter)){data<-data[data$Ideational %in% input$ideaFilter,]}
    if(!is.null(input$issueFilter)){data<-data[data$IssueArea %in% input$issueFilter,]}
    if(!is.null(input$levelFilter)){data<-data[data$Level %in% input$levelFilter,]}
    if(!is.null(input$methodFilter)){data<-data[data$Methodology %in% input$methodFilter,]}
    if(!is.null(input$materialFilter)){data<-data[data$Material %in% input$materialFilter,]}
    if(!is.null(input$regionFilter)){data<-data[data$Region %in% input$regionFilter,]}
    if(!is.null(input$timeFilter)){data<-data[data$TimePeriod %in% input$timeFilter,]}
    if(!is.null(input$policyFilter)){data<-data[data$PolicyPrescription %in% input$policyFilter,]}
    if(!is.null(input$journalFilter)){data<-data[data$journal %in% input$journalFilter,]}
    
    data<-data %>% filter(year >= input$yearInput[1],year <= input$yearInput[2]) # change year range #
    pubIDs<-(data %>% distinct(pubID))[[1]]
    
    ## manipulate data for plotting ##
    if(input$percentInput=="Frequency" & input$ygInput=="Variable"){
      data<-data %>% distinct(pubID,.keep_all=TRUE) %>% group_by_at(input$varFilter,.drop=FALSE) %>% mutate(Count=n()) %>% select(Count) %>% distinct_at(vars(input$varFilter,Count),.keep_all=TRUE)}
    if(input$percentInput=="Frequency" & input$ygInput=="Year"){
      data<-data %>% distinct(pubID,.keep_all=TRUE) %>% group_by(year,.drop=FALSE) %>% mutate(Count=n()) %>% select(Count) %>% tidyr::complete(year=2000:2016,fill=list(Count=0)) %>% distinct(year,.keep_all=TRUE)}
    if(input$percentInput=="Percentage" & input$ygInput=="Year"){ 
      data<-data %>% ungroup() %>% mutate(year_distinct=n_distinct(pubID)) %>% distinct(pubID,.keep_all=TRUE) %>% group_by(year,.drop=FALSE) %>% mutate(Count=n()) %>% select(year,Count,year_distinct,pubID) %>% tidyr::complete(year=2000:2016,fill=list(Percentage=0)) %>% distinct(year,.keep_all=TRUE) %>% ungroup() %>% mutate(Percentage=Count/year_distinct*100)}
    if(input$percentInput=="Percentage" & input$ygInput=="Variable"){
      data<-data %>% mutate(var_distinct=n_distinct(pubID)) %>% distinct(pubID,.keep_all=TRUE) %>% group_by_at(input$varFilter,.drop=FALSE) %>% mutate(Count=n()) %>% distinct_at(vars(input$varFilter,Count,var_distinct),.keep_all=TRUE) %>% ungroup() %>% mutate(Percentage=Count/var_distinct*100)}
    
    if(nrow(data)==0 & input$ygInput=="Year"){data<-data %>% ungroup() %>% tidyr::complete(year=2000:2016)}
    if(nrow(data)==0 & input$ygInput=="Variable"){data<-data %>% ungroup() %>% tidyr::complete(!!sym(input$varFilter))}
    data$Count[is.na(data$Count)]<-0
    if(!is.null(data$Percentage)){data$Percentage[is.na(data$Percentage)]<-0}
    
    list(data=as.data.frame(data),pubIDs=pubIDs)
  })
  
  #### create plot ####
  plot<-reactive({
    if(input$percentInput=="Percentage" & input$ygInput=="Variable"){
      ggplot(filtered()$data, aes_string(x=input$varFilter)) +
        ylab("Percentage") + xlab(paste(input$varFilter)) + plot_theme + 
        geom_bar_interactive(aes(tooltip = paste0("<strong>Variable:</strong> " , get(input$varFilter), "<br> <strong>Percentage:</strong> ", round(Percentage,2),"%"),y=Percentage),stat="identity", size=2, show.legend=TRUE,fill=palette[3]) +
        scale_y_continuous(expand=expand_scale(add=c(0,3.25))) +
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
        scale_y_continuous(expand=expand_scale(add=c(0,3.25))) +
        scale_x_continuous(breaks=seq(2000,2017,2),labels=seq(2000,2017,2),name="Year") +
        ylab("Percentage") + xlab("Year") + plot_theme + 
        geom_bar_interactive(aes(tooltip = paste0("<strong>Year:</strong> " ,year, "<br> <strong>Percentage:</strong> ", round(Percentage,2),"%"),y=Percentage,fill=year),stat="identity", size = 2, show.legend=TRUE,fill=palette[3]) +
        theme(legend.position="bottom")
    } else { # frequency by year #
      ggplot(filtered()$data, aes(x=year)) +
        scale_x_continuous(breaks=seq(2000,2017,2),labels=seq(2000,2017,2),name="Year") +
        scale_y_continuous(expand=expand_scale(add=c(0,3.25))) +
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
    data<-PJD_copy
    data<-data[data$pubID %in% filtered()$pubIDs,]
    data<-as.data.frame(data)
  })
  
  output$download<-downloadHandler(filename=function(){
    paste("TRIP_PJD_", format(Sys.Date(), "%Y_%b_%d"), ".csv",sep="")
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
  onBookmark(function(state){
    lapply(1:12, function(x) state$values[[paste0(p_store$filter[x])]]<-input[[paste0(p_store$filter[x])]])
  })
  onRestore(function(state){
    lapply(1:12, function(x) if(!is.null(state$values[[paste0(p_store$filter[x])]])){addInput(paste0(p_store$names[x]))})
  })
  onBookmarked(function(url){updateQueryString(url)})
  observeEvent(input$clipbtn, {showModal(urlModal(URL(), title="Copy URL to clipboard"))})
  URL <- reactiveVal()
  onBookmarked(function(url){URL(url)})
  setBookmarkExclude("clipbtn")
}

enableBookmarking(store = "url")
shinyApp(ui, server)

