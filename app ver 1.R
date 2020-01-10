# This does XXXXX
# Code developed by Vera Choo, with reference from: XXXX

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(plotly)
library(rsconnect)

#rsconnect::setAccountInfo(name='vvchoo', token='20318BE8E51FD829619E1D7095A496CB', secret='VUQ9WQai6Vt1CftmuLpcaxg5j3pHNgYReyWhCGZK')
surveys<-list(
              fac04=readRDS(url("http://trip.wm.edu/charts/dashboard-data/faculty survey/fac_survey_1.rds")),
              fac06=readRDS(url("http://trip.wm.edu/charts/dashboard-data/faculty survey/fac_survey_2.rds")),
              fac08=readRDS(url("http://trip.wm.edu/charts/dashboard-data/faculty survey/fac_survey_3.rds")),
              fac11=readRDS(url("http://trip.wm.edu/charts/dashboard-data/faculty survey/fac_survey_4.rds")),
              fac14=readRDS(url("http://trip.wm.edu/charts/dashboard-data/faculty survey/fac_survey_5.rds")),
              fac17=readRDS(url("http://trip.wm.edu/charts/dashboard-data/faculty survey/fac_survey_6.rds"))
)

# create codebook #
qid_list<-list(
              qid_one=readRDS(url("http://trip.wm.edu/charts/dashboard-data/faculty survey/qid_list_1.rds")),
              qid_two=readRDS(url("http://trip.wm.edu/charts/dashboard-data/faculty survey/qid_list_2.rds")),
              qid_three=readRDS(url("http://trip.wm.edu/charts/dashboard-data/faculty survey/qid_list_3.rds")),
              qid_four=readRDS(url("http://trip.wm.edu/charts/dashboard-data/faculty survey/qid_list_4.rds")),
              qid_five=readRDS(url("http://trip.wm.edu/charts/dashboard-data/faculty survey/qid_list_5.rds")),
              qid_six=readRDS(url("http://trip.wm.edu/charts/dashboard-data/faculty survey/qid_list_6.rds")),
              qid_seven=readRDS(url("http://trip.wm.edu/charts/dashboard-data/faculty survey/qid_list_7.rds"))
)

`%notin%` <- Negate(`%in%`)

commas<-readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/commas.rds"))
multipart<-readRDS(url("http://trip.wm.edu/charts/dashboard-data/snap-polls/multipart.rds"))

plot_theme<-theme_bw() +
  theme(axis.text.x=element_text(angle=50,vjust=1,hjust=1))

#######################################################
#                   USER INTERFACE                    #
#######################################################
ui <- function(req){
  fluidPage(
  tags$head(
    tags$link(rel="me", href="https://twitter.com/trip_irsurvey"),
    tags$style(HTML("
                    .action-button{padding:10px;margin-top:-12px;display:block;background-color:#e1eaf0;transition:background-color .3s, color .3s;border-radius:5px;}
                    .action-button:hover{background-color:#edf2f5;transition:background-color .3s, color .3s;text-decoration:none;}
                    .well{background-color:#e1eaf0;}
                    #return{background-color:#f2f9fc;transition:background-color .3s, color .3s;border-color:#ffffff;width:100%;font-weight:bold;}
                    #return:hover{background-color:#fafdff;transition:background-color .3s, color .3s;}
                    #tweet{background-color:#f2f9fc;transition:background-color .3s, color .3s;border-color:#ffffff;width:100%;font-weight:bold;}
                    #tweet:hover{background-color:#fafdff;transition:background-color .3s, color .3s;}

                    #clipbtn{background-color:#fff;font-weight:bold;width:100%;}
                    #clipbtn:hover{background-color:#edf2f5;transition: background-color .3s, color .3s;}
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
                          fluidRow(column(9,
                            strong(h3(textOutput("selectedQ"))),
                            br())),
                          fluidRow(column(3,
                              wellPanel(
                                uiOutput("countryList"),br(),
                                selectInput("crosstabList","Crosstab:",
                                            c("Select..."="NULL","Gender"="gender","Rank","Area of study","etc.")),br(),
                                actionButton("clipbtn","Copy URL",icon("clipboard")),br(),
                                actionButton("tweet","Tweet",onclick="window.open('http://twitter.com/intent/tweet?text=' + encodeURIComponent(window.location.href))"),br(),
                                actionButton("return","Return to question list"))),
                          column(9,
                                 textOutput("error"),
                                 uiOutput("noGraph"),
                                 plotlyOutput("graph"),
                                 plotOutput("legend"))))),
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
      countryFilter<-c(names(table(surveys[[5]]$surveyCountry)[table(surveys[[5]]$surveyCountry)!=0]))
    } else {
      countryFilter<-as.character(input$countries)
    }
    countryFilter
  })
  
  #### question category filter ####
  observeEvent(input$dataSelect, {
    if(input$dataSelect!=7){
      output$section<-renderUI({
        selectInput("sectionSelect","Choose question category",c("All questions"="NULL",names(table(qid_list[[as.numeric(input$dataSelect)]]$Question.category))))
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
    if(input$crosstabList=="NULL"){
      crosstabs<-NULL
    } else { crosstabs<-input$crosstabList }
  })
  

    ## list of questions ##
  output$questions<-renderUI({
    lapply(1:nrow(qid()), function(x) fluidRow(actionLink(paste0("btn_",x),qid()[,1][x]), br()))
  })

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
      y <- if(length(dataStore$dataLoc[[1]])==1 && dataStore$dataLoc[[1]][1] %in% commas[[1]]){ #multiselect Qs
        len<-commas[grep(dataStore$dataLoc[[1]][1],commas[,1]),2]
        z<-data.frame(surveys[[x]] %>% select(crosstabs(),response=dataStore$dataLoc[[1]][1]))
        z<-z %>% drop_na() %>% separate(response,c(paste("response",1:len,sep="_")),sep=",") %>% group_by_at(crosstabs()) %>% 
          mutate(total_n=n()) %>% gather(key,response,response_1:paste0("response_",len)) %>% select(-key) %>% group_by_all() %>% 
          mutate(per=round(n()/total_n*100,2)) %>% distinct(response, .keep_all=TRUE) %>% drop_na() %>% top_n(8,per) %>% droplevels()
        z
      } else if(length(dataStore$dataLoc[[1]])==1 && dataStore$dataLoc[[1]] %notin% commas[[1]]){
        data.frame(surveys[[x]] %>% filter(surveyCountry %in% countryFilter()) %>% select(crosstabs(),response=dataStore$dataLoc[[1]][1]) %>% filter(!is.na(response)) %>% drop_na() %>% group_by_all() %>% summarize(n=n()) %>% mutate(per=round(n/sum(n)*100,2))) %>% group_by_at(crosstabs()) %>% drop_na %>% top_n(8,per) %>% droplevels()
      } else {
        data.frame(surveys[[x]] %>% filter(surveyCountry %in% countryFilter()) %>% select(crosstabs(),dataStore$dataLoc[[1]][1]:dataStore$dataLoc[[1]][2],surveyId) %>% gather(key,value,dataStore$dataLoc[[x]][1]:dataStore$dataLoc[[x]][2]) %>% select(crosstabs(),response=value,surveyId) %>% drop_na() %>% mutate(total_n=n_distinct(surveyId)) %>% select(-surveyId) %>% group_by_all() %>% summarize(n=n()) %>% mutate(per=round(n/total_n*100,2))) %>% top_n(8,per) %>% droplevels()
      }
      return(y)
    }
  
  new_df_all<-function(x){
    y <- if(length(dataStore$dataLoc[[x]])==1 && dataStore$dataLoc[[x]][1] %in% commas[[1]]){ #multiselect Qs
      z<-data.frame(surveys[[x]] %>% select(response=dataStore$dataLoc[[x]][1]))
      z<-data.frame(response=unlist(strsplit(as.character(z$response), ",")))
      z<-z %>% group_by(response) %>% drop_na() %>% summarize(n=n()) %>% mutate(per=round(n/sum(n)*100,2), year=surveys[[x]]$surveyYear[1]) %>% drop_na() %>% top_n(8,per) %>% droplevels()
      z
         } else if(length(dataStore$dataLoc[[x]])==1 && dataStore$dataLoc[[x]][1] %notin% commas[[1]]){
                data.frame(surveys[[x]] %>% filter(surveyCountry %in% countryFilter()) %>% select(response=dataStore$dataLoc[[x]][1],year=surveyYear,surveyId=standardId) %>% filter(!is.na(response)) %>% drop_na() %>% group_by(year,response) %>% summarize(n=n()) %>% mutate(per=round(n/sum(n)*100,2))) %>% top_n(8,per) %>% drop_na %>% droplevels()
         } else {
                data.frame(surveys[[x]] %>% filter(surveyCountry %in% countryFilter()) %>% select(crosstabs(),dataStore$dataLoc[[x]][1]:dataStore$dataLoc[[x]][2],year=surveyYear,surveyId=standardId) %>% gather(key,value,dataStore$dataLoc[[x]][1]:dataStore$dataLoc[[x]][2]) %>% drop_na() %>% mutate(total_n=n_distinct(surveyId)) %>% select(crosstabs(),response=value,year,total_n) %>% group_by_all() %>% summarize(n=n()) %>% mutate(per=round(n/total_n*100,2))) %>% drop_na() %>% top_n(8,per) %>% droplevels()
         }
    return(y)
  }
  
  df<-reactive({
    if(input$dataSelect==7){
      df<-data.frame(rbind(new_df_all(1),new_df_all(2),new_df_all(3),new_df_all(4),new_df_all(5),new_df_all(6)),stringsAsFactors=TRUE)
    } else {
      df<-data.frame(new_df(as.numeric(input$dataSelect)))
    }
   return(df)
  })

  ###################### G R A P H ######################
  ## create graph ##
  p<-reactive({
    if(nrow(df())!=0){
      if(input$dataSelect!=7){
        if(is.null(crosstabs())){
          p<-plot_ly(df(), x=~response, y=~per, color=~response, colors="YlOrRd",type="bar",hoverinfo='text',text= ~paste(response,'<br>Percentage: ', per,'%',sep=""),height=600) %>% layout(barmode='stack',margin=list(l=50,r=50,t=50,b=100),yaxis=list(range(c(0,100))))
        } else if(!is.null(crosstabs())){
          p<-plot_ly(df(), x=~response, y=~per, color=~get(crosstabs()), colors="YlOrRd", type="bar",hoverinfo='text',text=~paste(response,'<br>Percentage: ', per,'%',sep=""), height=600) %>% layout(bargap=5,legend=list(.08,.08),margin=list(l=50,r=50,t=50,b=100),yaxis=list(range(c(0,100))))
        }
      } else if(input$dataSelect==7){
        if(is.null(crosstabs())){
          p<-plot_ly(df(), x=~year, y=~per, color=~response, colors="RdYlBu", type="scatter", mode="lines",hoverinfo='text',text= ~paste(response,'<br>Percentage: ', per,'%',sep=""),height=500) %>%
            layout(barmode='stack',margin=list(l=50,r=50,t=50,b=100),yaxis=list(range(c(0,100))), legend=list())
        } else if(!is.null(crosstabs())){
          p<-plot_ly(df(), x=~year, y=~per, color=~response, colors="RdYlBu", linetype=~get(crosstabs()), type="scatter", mode="lines",hoverinfo='text',text= ~paste(response,'<br>Percentage: ', per,'%',sep="")) %>%
            layout(barmode='stack',margin=list(l=50,r=50,t=50,b=100),yaxis=list(range(c(0,100))))
        }
      }
    }
  })
  
  output$graph<-renderPlotly({ p() })
  output$noGraph<-renderUI({ if(nrow(df())==0){h4(strong("The selected question was not asked in this country."))} })
  #### ####

  output$error<-renderText({ })
  ## return to questions list ##
  observeEvent(input$return, {
    updateTabsetPanel(session,"Questions","Questions")
  })
  ## print question as title ##
  output$selectedQ<-renderText({paste(qid()[,1][as.numeric(dataStore$dataNum)])})
  #### ####
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
}

enableBookmarking(store = "url")
shinyApp(ui, server)

