# This does XXXXX
# Code developed by Vera Choo, with reference from: XXXX

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(rsconnect)
library(base64enc)
library(RCurl)
library(htmlwidgets)

#rsconnect::setAccountInfo(name='vvchoo', token='20318BE8E51FD829619E1D7095A496CB', secret='VUQ9WQai6Vt1CftmuLpcaxg5j3pHNgYReyWhCGZK')
JAD<-readRDS(url("https://www.dropbox.com/s/nx09f4imh6b02kg/JAD.rds?dl=1"))
JAD_copy<-readRDS(url("https://www.dropbox.com/s/lb8zo1c5s7qmn24/JAD_original.rds?dl=1"))


#### set up plot theme ####
plot_theme<-theme_bw() +
  theme(axis.text.x=element_text(angle=50), axis.title.x=element_text(vjust=1.5),axis.title.y=element_text(hjust=1.5))


######################################################################################
#                                    USER INTERFACE                                  #
######################################################################################
ui <- function(req){
  fluidPage(
    tags$head(
      tags$style(HTML("
                      .well{
                        background-color:#e1eaf0;
                      }
                      #download{
                        font-weight:bold;
                      }
                      #tweet{
                        float:right;
                        background-color:#00acee;
                        font-weight:bold;
                        color:#ffffff;
                      }
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
                        h4("See all articles which are:"),
                        selectInput("addFilter","Filter choice",choices=c("Select...","Contemporary","Epistemology","Substantive Focus","Ideational","Issue Area","Level of Analysis","Material","Methodology","Paradigm","Policy Prescription","Region","Time Period")),
                        uiOutput("epistFilter"),uiOutput("contempFilter"),uiOutput("focusFilter"),uiOutput("ideaFilter"),uiOutput("issueFilter"),uiOutput("levelFilter"),uiOutput("materialFilter"),uiOutput("methodFilter"),uiOutput("paradigmFilter"),uiOutput("policyFilter"),uiOutput("regionFilter"),uiOutput("timeFilter"))),
             wellPanel(
               h4("Article year range:"),
               sliderInput("yearInput","Year",1980,2017,c(1980,2017),sep="")), #change when updated),
             wellPanel(
               h4("Share your data:"),
               downloadButton("download","Download"),
               strong(a(actionButton("tweet","Tweet", onclick="window.open('http://twitter.com/intent/tweet?text=' + encodeURIComponent(window.location.href))")))
             )),
      column(8,verticalLayout(
        tableOutput("view"),
        plotlyOutput("plotly")))),
    tags$style(type="text/css", ".shiny-output-error{visibility: hidden;}", ".shiny-output-error:before{visibility:hidden;}"))
  }

#######################################################################################
#                                       SERVER                                        #
#######################################################################################
server <- function(input, output, session) {
  #### add new filter button ####  
  observeEvent(input$addFilter, {
    if(input$addFilter=="Paradigm") {
      output$paradigmFilter<-renderUI({
        tagList(
          ui=tags$div(selectInput("paradigmFilter", "Paradigm",multiple=TRUE,choices=c(names(table(JAD$Paradigm))))))})
    } else if(input$addFilter=="Epistemology"){
      output$epistFilter<-renderUI({
        tagList(
          ui=tags$div(selectInput("epistFilter", "Epistemology",multiple=TRUE,choices=c(names(table(JAD$Epistemology))))))})
    } else if(input$addFilter=="Policy Prescription"){
      output$policyFilter<-renderUI({
        tagList(
          ui=tags$div(selectInput("policyFilter", "Policy Prescription",multiple=TRUE,choices=c("Yes","No"))))})
    } else if(input$addFilter=="Contemporary"){
      output$contempFilter<-renderUI({
        tagList(
          ui=tags$div(selectInput("contempFilter", "Contemporary",multiple=TRUE,choices=c("Yes","No"))))})
    } else if(input$addFilter=="Level of Analysis"){
      output$levelFilter<-renderUI({
        tagList(
          ui=tags$div(selectInput("levelFilter", "Level of Analysis",multiple=TRUE,choices=c(names(table(JAD$Level))))))})
    } else if(input$addFilter=="Ideational"){
      output$ideaFilter<-renderUI({
        tagList(
          ui=tags$div(selectInput("ideaFilter", "Ideational",multiple=TRUE,choices=c("Yes","No"))))})
    } else if(input$addFilter=="Issue Area"){
      output$issueFilter<-renderUI({
        tagList(
          ui=tags$div(selectInput("issueFilter", "Issue Area",multiple=TRUE,choices=c(names(table(JAD$IssueArea))))))})
    } else if(input$addFilter=="Region"){
      output$regionFilter<-renderUI({
        tagList(
          ui=tags$div(selectInput("regionFilter", "Region",multiple=TRUE,choices=c(names(table(JAD$Region))))))})
    } else if(input$addFilter=="Material"){
      output$materialFilter<-renderUI({
        tagList(
          ui=tags$div(selectInput("materialFilter","Material",multiple=TRUE,choices=c("Yes","No"))))})
    } else if(input$addFilter=="Methodology"){
      output$methodFilter<-renderUI({
        tagList(
          ui=tags$div(selectInput("methodFilter", "Methodology",multiple=TRUE,choices=c(names(table(JAD$Methodology))))))})
    } else if(input$addFilter=="Time Period"){
      output$timeFilter<-renderUI({
        tagList(
          ui=tags$div(selectInput("timeFilter", "Time Period",multiple=TRUE,choices=c(names(table(JAD$TimePeriod))))))})
    } else if(input$addFilter=="Substantive Focus"){ 
      output$focusFilter<-renderUI({
        tagList(
          ui=tags$div(selectInput("focusFilter", "Substantive Focus",multiple=TRUE,choices=c(names(table(JAD$Focus))))))})
    }
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
    
    if(input$ygInput=="Year"){data<-data %>% group_by(year) %>% mutate(year_distinct=n_distinct(pubID))}  # total distinct articles a year #
    
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
    
    ## manipulate data for plotting ##
    if(input$percentInput=="Frequency" & input$ygInput=="Variable"){
      data<-data %>% group_by(input$varFilter) %>% mutate(filter=n_distinct(pubID),filter_n=n()) # find unique articles in x-axis #
      data<-data %>% mutate(Count=filter/filter_n)}
    if(input$percentInput=="Frequency" & input$ygInput=="Year"){
      data_n<-data %>% group_by(year, pubID) %>% count(pubID) # create counts of pubID #
      data<-left_join(data,data_n)
      data$Count<-ifelse(data$n!=1,data$Count<-(1/data$n),data$Count<-1)} # create actual sum variable for stat_summary #
    if(input$percentInput=="Percentage" & input$ygInput=="Year"){ # create new plotting variable for percentages #
      data<-data %>% group_by(year) %>% mutate(distinct_filtered=n_distinct(pubID)) # find number of distinct articles after filtering #
      data<-data %>% group_by(year) %>% mutate(year_filtered=n())  # find number of all observations in a year #
      data<-data %>% mutate(Percentage=((distinct_filtered/year_filtered)/year_distinct)*100)}  # create percentages to sum for stat_summary #
    if(input$percentInput=="Percentage" & input$ygInput=="Variable"){  # varFilter percentages #     
      data<-data %>% group_by(input$varFilter) %>% mutate(new_n=n_distinct(pubID),filter_n=n())  # find how articles in x-axis #
      data<-data %>% ungroup() %>% mutate(filter=n_distinct(pubID))# find unique articles #
      data<-data %>% mutate(Percentage=(new_n/filter/filter_n)*100)} # create actual percentages to sum for stat_summary #
    
    data<-as.data.frame(data)
  })
  
  #### create plot ####
  plot<-reactive({
    p<-if(input$percentInput=="Percentage" & input$ygInput=="Variable"){
      ggplot(filtered(), aes_string(x=input$varFilter)) +
        stat_summary(aes(y=Percentage),fun.y="sum",geom="bar",position="identity",fill="#a2c6da") +
        scale_x_discrete(drop=FALSE) +
        ylab("Percentage") +
        plot_theme 
    } else if (input$percentInput=="Frequency" & input$ygInput=="Variable"){
      ggplot(filtered(), aes_string(x=input$varFilter)) +
        geom_bar(stat="summary",aes(y=Count),fun.y="sum",fill="#a2c6da") +
        scale_x_discrete(drop=FALSE) +
        ylab("Frequency") +
        plot_theme 
    } else if (input$percentInput=="Percentage" & input$ygInput=="Year"){
      ggplot(filtered(), aes(x=year)) +
        stat_summary(aes(y=Percentage),fun.y="sum",geom="bar",position="identity",fill="#a2c6da") +
        scale_x_discrete(drop=FALSE) +
        scale_x_continuous(breaks=seq(1980,2017,5),labels=seq(1980,2017,5)) +
        ylab("Percentage") +
        xlab("Year") +
        plot_theme
    } else{ # frequency by year #
      ggplot(filtered(), aes(x=year)) +
        stat_summary(aes(y=Count),fun.y="sum",geom="bar",position="identity",fill="#a2c6da") +
        scale_x_discrete(drop=FALSE) +
        scale_x_continuous(labels=seq(1980,2017,5),breaks=seq(1980,2017,5)) +
        xlab("Year") +
        ylab("Frequency") +
        plot_theme
    }
    p
  })
  
  dash_plot<-reactive({ggplotly(plot(), height=700) %>%
      layout(margin=list(b=300,l=100),autosize=T,yaxis=list(hoverinfo="p"))})
  
  output$plotly <- renderPlotly({ 
    dash_plot()
  })
  
  #### test printout #### 
  output$view<-renderText({}) 
  ########
  
  #### download csv file ####
  pubID<-reactive({data.frame(as.integer(names(table(filtered()$pubID))))[[1]]})
  csvDownload<-reactive({
    data<-JAD_copy
    data<-data[data$pubID %in% pubID(),]
    data<-as.data.frame(data)
  })
  
  output$download<-downloadHandler(filename=function(){
    paste("TRIP_JAD_", format(Sys.Date(), "%Y_%b_%d"), ".csv",sep="")
  },
  content=function(file){
    write.csv(csvDownload(), file, row.names=FALSE)
  })
  
  ########
  
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  
  onBookmarked(function(url) {
    updateQueryString(url)
  })
}
enableBookmarking(store = "url")
shinyApp(ui, server)

