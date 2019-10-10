library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(rsconnect)

rsconnect::setAccountInfo(name='vvchoo', token='20318BE8E51FD829619E1D7095A496CB', secret='VUQ9WQai6Vt1CftmuLpcaxg5j3pHNgYReyWhCGZK')

fac04<-readRDS(url("https://www.dropbox.com/s/re2y7xs78y8091k/fac04.rds?dl=1"))
fac06<-readRDS(url("https://www.dropbox.com/s/fb4u9fa59n48qpr/fac06.rds?dl=1"))
fac08<-readRDS(url("https://www.dropbox.com/s/r1jhp65mvxgnmpt/fac08.rds?dl=1"))
fac11<-readRDS(url("https://www.dropbox.com/s/ajuy8bixg3aqm4b/fac11.rds?dl=1"))
fac14<-readRDS(url("https://www.dropbox.com/s/skwh71zqxzjt5iv/fac14.rds?dl=1"))
fac17<-readRDS(url("https://www.dropbox.com/s/d92a52h87ifhbxp/fac17.rds?dl=1"))

# create codebook #
qid_overtime<-read.csv("https://www.dropbox.com/s/uu5n4n8q96u32sd/FacultySurvey_QIDovertime.csv?dl=1",stringsAsFactors=FALSE,na.strings=c("","NA"))
qid_overtime$Question_text<-gsub("\n"," ",qid_overtime$Question_text)
qid_overtime$Question_text<-trimws(qid_overtime$Question_text)
qid_overtime<-qid_overtime[,c(2,4:9)]
qid_all<-qid_overtime[,c(1:7)][!is.na(qid_overtime$X2004) & !is.na(qid_overtime$X2006) & !is.na(qid_overtime$X2008) & !is.na(qid_overtime$X2011) & !is.na(qid_overtime$X2014) & !is.na(qid_overtime$X2017),]

new_surveys<-list("FS2004"=fac04,"FS2006"=fac06,"FS2008"=fac08,"FS2011"=fac11,"FS2014"=fac14,"FS2017"=fac17)

plot_theme<-theme_bw() +
  theme(axis.text.x=element_text(angle=50,vjust=1,hjust=1))

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
  titlePanel(h1(strong("Faculty Survey"))),
    tabsetPanel(id="Questions",
                 tabPanel("Questions",
                          br(),
                          sidebarPanel(
                            selectInput("dataSelect","Choose Survey Year:",choices=c("2004 Faculty Survey"=2,"2006 Faculty Survey"=3,"2008 Faculty Survey"=4,"2011 Faculty Survey"=5,"2014 Faculty Survey"=6,"2017 Faculty Survey"=7,"All years (selected questions)"=1))), # updates go here
                          mainPanel(br(),uiOutput("questions"),tableOutput("print"))),
                 tabPanel("Graph",
                          fluidRow(column(9,
                            strong(h3(textOutput("selectedQ"))),
                            br())),
                          fluidRow(column(4,
                              wellPanel(
                                uiOutput("countryList"),br(),
                                actionButton("return","Return to question list"))),
                          column(8,
                                 textOutput("error"),
                                 uiOutput("noGraph"),
                                 plotlyOutput("graph"))))),
  tags$style(type="text/css", ".shiny-output-error{visibility: hidden;}", ".shiny-output-error:before{visibility:hidden;}"))

#######################################################
#                       SERVER                        #
#######################################################
server <- function(input, output, session) {

  ###################### C O U N T R I E S ######################
  observeEvent(input$dataSelect, {
    output$countryList<-renderUI({
      selectInput("countries","Choose Survey Country:",c("All Countries",names(table(new_surveys[[as.numeric(input$dataSelect)-1]]$surveyCountry)[table(new_surveys[[as.numeric(input$dataSelect)-1]]$surveyCountry)!=0])))
    })
  })
  
  countryFilter<-reactive({
    if(input$countries=="All Countries"){
      countryFilter<-c(names(table(new_surveys[[as.numeric(input$dataSelect)-1]]$surveyCountry)[table(new_surveys[[as.numeric(input$dataSelect)-1]]$surveyCountry)!=0]))
    } else {
      countryFilter<-as.character(input$countries)
    }
    countryFilter
  })
  
  ###################### Q U E S T I O N S ######################
  qid<-reactive({
    if(input$dataSelect==1){
      qid_all
    } else {
      qid_overtime[!is.na(qid_overtime[as.numeric(input$dataSelect)]),c(1,as.numeric(input$dataSelect))]
      }
    })
  dataStore<-reactiveValues(dataLoc=NULL,dataNum=NULL)

    ## list of questions ##
  output$questions<-renderUI({
    lapply(1:nrow(qid()), function(x) fluidRow(actionLink(paste0("btn_",x),qid()[,1][x]), br()))
  })

  observe({
    input_btn<-paste0("btn_", 1:nrow(qid()))
    lapply(input_btn, function(x) observeEvent(input[[x]],{
      i <- as.numeric(sub("btn_", "", x))
      dataStore$dataNum <- i
      if(length(qid())==2){dataStore$dataLoc<-strsplit(qid()[as.numeric(dataStore$dataNum),2],":")}
      if(length(qid())!=2){lapply(2:7, function(x) dataStore$dataLoc[[x-1]]<-strsplit(qid()[as.numeric(dataStore$dataNum),x],":"))}
      updateTabsetPanel(session, "Questions", "Graph")
      }))
  })
  
  ## construct dataframe ##
  new_df<-function(x){
    y <- ifelse(length(dataStore$dataLoc[[1]])==1,
           data.frame(new_surveys[[x]] %>% filter(surveyCountry %in% countryFilter()) %>% select(dataStore$dataLoc[[1]][1])) %>% drop_na(),
           data.frame(new_surveys[[x]] %>% filter(surveyCountry %in% countryFilter()) %>% select(dataStore$dataLoc[[1]][1]:dataStore$dataLoc[[1]][2]) %>%
                                               gather(key,value,dataStore$dataLoc[[1]][1]:dataStore$dataLoc[[1]][2]) %>%
                                               select(value)) %>% drop_na())
    return(y)
  }
  
  new_df_all<-function(x){
    y <- ifelse(length(dataStore$dataLoc[[x]])==1,
                data.frame(new_surveys[[x]] %>% filter(surveyCountry %in% countryFilter()) %>% select(dataStore$dataLoc[[x]][1])) %>% drop_na(),
                data.frame(new_surveys[[x]] %>% filter(surveyCountry %in% countryFilter()) %>% select(dataStore$dataLoc[[x]][1]:dataStore$dataLoc[[x]][2]) %>%
                             gather(key,value,dataStore$dataLoc[[x]][1]:dataStore$dataLoc[[x]][2]) %>%
                             select(value)) %>% drop_na())
    return(y)
  }
  
  df<-reactive({
    df<-data.frame(ifelse(input$dataSelect==2, new_df(1),
                   ifelse(input$dataSelect==3, new_df(2),
                   ifelse(input$dataSelect==4, new_df(3),
                   ifelse(input$dataSelect==5, new_df(4),
                   ifelse(input$dataSelect==6, new_df(5),
                   ifelse(input$dataSelect==7, new_df(6),
                   ifelse(input$dataSelect==1, 
                          cbind(new_df_all(1),new_df_all(2),new_df_all(3),new_df_all(4),new_df_all(5),new_df_all(6))))))))),
                   stringsAsFactors=TRUE)
   df
  })

  ###################### G R A P H ######################
  ## create graph ##
    output$graph<-renderPlotly({
      if(nrow(df()!=0)){
        p<-ggplot(data=df(),aes_string(x=df()[[1]], text=df()[[1]])) +
          geom_bar(stat="count",fill="#abcad4") +
          scale_x_discrete(drop=FALSE) +
          geom_text(stat="count",aes(label=..count..),vjust=-.5) +
          plot_theme
        ggplotly(p,tooltip="text")
      }
    })
    output$noGraph<-renderUI({
      if(nrow(df())==0){h4(strong("The selected question was not asked in this country."))}
    })
  
  ## print error ##
  output$error<-renderText({
    print(qid())
    print(dataStore$dataNum)
    print(dataStore$dataLoc)
    print(head(df()))
    print(str(df()))
  })
    
  ## return to questions list ##
  observeEvent(input$return, {
    updateTabsetPanel(session,"Questions","Questions")
  })
  
  ## print question as title ##
  output$selectedQ<-renderText({paste(qid()[,1][as.numeric(dataStore$dataNum)])})
  #### ####
}

shinyApp(ui, server)

