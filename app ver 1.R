library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
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
qid_list<-lapply(1:7, function(x) assign(paste0("qid_",x), read.csv(paste0("C:/Users/Vera/Desktop/FALL 2019/TRIP/WEB/SURVEYS/qid_",x,".csv"))))
for(x in 1:7){
  qid_list[[x]]$Question_text<-gsub("\\[ Country X \\]","RESPONDENT COUNTRY",qid_list[[x]]$Question_text)
  qid_list[[x]]$Question_text<-gsub("\\[ Country X\\]","RESPONDENT COUNTRY",qid_list[[x]]$Question_text)
}

new_surveys<-list("FS2004"=fac04,"FS2006"=fac06,"FS2008"=fac08,"FS2011"=fac11,"FS2014"=fac14,"FS2017"=fac17)
new_surveys[[6]][new_surveys[[6]]=="NULL"]<-NA
levels(new_surveys[[5]]$surveyCountry)<-(unlist(strsplit(levels(new_surveys[[5]]$surveyCountry), " 2014")))
levels(new_surveys[[6]]$surveyCountry)<-(unlist(strsplit(levels(new_surveys[[6]]$surveyCountry), " 2017")))
levels(new_surveys[[5]]$surveyCountry)<-unlist(strsplit(levels(new_surveys[[5]]$surveyCountry), " French"))
levels(new_surveys[[5]]$surveyCountry)<-unlist(strsplit(levels(new_surveys[[5]]$surveyCountry), " English"))

`%notin%` <- Negate(`%in%`)

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
                            selectInput("dataSelect","Choose Survey Year:",
                                        choices=c("2004 Faculty Survey"=1,"2006 Faculty Survey"=2,"2008 Faculty Survey"=3,"2011 Faculty Survey"=4,"2014 Faculty Survey"=5,"2017 Faculty Survey"=6,"All years (selected questions)"=7))), # updates go here
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
                                 plotlyOutput("graph"),
                                 plotOutput("legend"))))),
  tags$style(type="text/css", ".shiny-output-error{visibility: hidden;}", ".shiny-output-error:before{visibility:hidden;}"))

#######################################################
#                       SERVER                        #
#######################################################
server <- function(input, output, session) {

  ###################### C O U N T R I E S ######################
  observeEvent(input$dataSelect, {
    output$countryList<-renderUI({
      if(input$dataSelect==1){
        selectInput("countries","Choose Survey Country:",c("United States only"="United States","All Countries"))
      } else {
        selectInput("countries","Choose Survey Country:",c("All Countries",names(table(new_surveys[[as.numeric(input$dataSelect)-1]]$surveyCountry)[table(new_surveys[[as.numeric(input$dataSelect)-1]]$surveyCountry)!=0])))
      }
    })
  })
  countryFilter<-reactive({
    if(input$countries=="All Countries" & input$dataSelect!=1){
      countryFilter<-c(names(table(new_surveys[[as.numeric(input$dataSelect)-1]]$surveyCountry)[table(new_surveys[[as.numeric(input$dataSelect)-1]]$surveyCountry)!=0]))
    } else if(input$countries=="All Countries" & input$dataSelect==1){
      countryFilter<-c(names(table(new_surveys[[5]]$surveyCountry)[table(new_surveys[[5]]$surveyCountry)!=0]))
    } else {
      countryFilter<-as.character(input$countries)
    }
    countryFilter
  })
  
  ###################### Q U E S T I O N S ######################
  qid<-reactive({ qid_list[[as.numeric(input$dataSelect)]] })
  dataStore<-reactiveValues(dataLoc=NULL,dataNum=NULL)
  crosstabs<-reactive({
    if(input$crosstabList=="NULL"){ crosstabs<-NULL
    } else { crosstabs<-input$crosstabList }
  })

    ## list of questions ##
  output$questions<-renderUI({
    lapply(1:nrow(qid()), function(x) fluidRow(actionLink(paste0("btn_",x),qid()[,1][x]), br()))
  })

  ## generate listeners for action buttons ##
  if(input$dataSelect!=7){
    lapply(1:60, function(x){
      observeEvent(input[[paste0("btn_",x)]], {
        i <- as.numeric(sub("btn_", "", x))
        dataStore$dataNum <- i
        dataStore$dataLoc<-strsplit(qid()[as.numeric(dataStore$dataNum),2],"[,:]")
        updateTabsetPanel(session, "Questions", "Graph")
        })
    })
  } else if(input$dataSelect==7){
    lapply(1:13, function(x){
      observeEvent(input[[paste0("btn_",x)]], {
        i <- as.numeric(sub("btn_", "", x))
        dataStore$dataNum <- i
        dataStore$dataLoc<-lapply(1:6, function(x) unlist(strsplit(as.character(qid_list[[7]][as.numeric(dataStore$dataNum),x]),":")))
        updateTabsetPanel(session, "Questions", "Graph")
      })
    })
  }
  
  ## construct dataframe ##
  new_df<-function(x){
    y <- if(length(dataStore$dataLoc[[1]])==1){
           data.frame(new_surveys[[x]] %>% filter(surveyCountry %in% countryFilter()) %>% select(response=dataStore$dataLoc[[1]][1]) %>% filter(!is.na(response)) %>% group_by(response) %>% summarize(n=n()) %>% mutate(per=round(n/sum(n)*100,2))) %>% drop_na() %>% top_n(8)
    } else {
           data.frame(new_surveys[[x]] %>% filter(surveyCountry %in% countryFilter()) %>% select(dataStore$dataLoc[[1]][1]:dataStore$dataLoc[[1]][2]) %>% gather(key,value,dataStore$dataLoc[[1]][1]:dataStore$dataLoc[[1]][2]) %>% select(response=value) %>% filter(!is.na(response)) %>% group_by(response) %>% summarize(n=n()) %>% mutate(per=round(n/sum(n)*100,2))) %>% drop_na() %>% top_n(8)
    }
    return(y)
  }
  
  new_df_all<-function(x){
    y <- if(length(dataStore$dataLoc[[1]])==1 && dataStore$dataLoc[[1]][1] %in% commas){ #multiselect Qs
      z<-data.frame(new_surveys[[x]] %>% select(response=dataStore$dataLoc[[1]][1]))
      z<-data.frame(response=unlist(strsplit(as.character(z$response), ",")))
      z<-z %>% group_by(response) %>% drop_na() %>% summarize(n=n()) %>% mutate(per=round(n/sum(n)*100,2), year=2017) %>% drop_na() %>% top_n(8,per)
      z
         } else if(length(dataStore$dataLoc[[x]][[1]])==1 && dataStore$dataLoc[[1]][1] %notin% commas){
                data.frame(new_surveys[[x]] %>% filter(surveyCountry %in% countryFilter()) %>% select(response=dataStore$dataLoc[[x]][[1]][1],year=surveyYear,surveyId=standardId) %>% filter(!is.na(response)) %>% group_by(year,response) %>% summarize(n=n()) %>% mutate(per=round(n/sum(n)*100,2))) %>% drop_na() %>% top_n(8,per)
         } else {
                data.frame(new_surveys[[x]] %>% filter(surveyCountry %in% countryFilter()) %>% select(dataStore$dataLoc[[x]][[1]][1]:dataStore$dataLoc[[x]][[1]][2],year=surveyYear,surveyId=standardId) %>% gather(key,value,dataStore$dataLoc[[x]][[1]][1]:dataStore$dataLoc[[x]][[1]][2]) %>% select(response=value,year,surveyId) %>% filter(!is.na(response)) %>% group_by(year,response) %>% summarize(n=n()) %>% mutate(per=round(n/sum(n)*100,2))) %>% drop_na() %>% top_n(8,per)
         }
    return(y)
  }
  
  df<-reactive({
    if(input$dataSelect==1){
      df<-data.frame(rbind(new_df_all(1),new_df_all(2),new_df_all(3),new_df_all(4),new_df_all(5),new_df_all(6)),stringsAsFactors=TRUE)
    } else{
      df<-data.frame(if(input$dataSelect==2){new_df(1)
                     } else if(input$dataSelect==3){new_df(2)
                     } else if(input$dataSelect==4){new_df(3)
                     } else if(input$dataSelect==5){new_df(4)
                     } else if(input$dataSelect==6){new_df(5)
                     } else if(input$dataSelect==7){new_df(6)},stringsAsFactors=TRUE)
    }
   return(df)
  })

  ###################### G R A P H ######################
  ## create graph ##
  p<-reactive({
    if(nrow(df()!=0) & input$dataSelect!=1){
      p<-plot_ly(df(), x=~response, y=~per, color=~response, colors="YlOrRd",type="bar",hoverinfo='text',text= ~paste(response,'<br>Percentage: ', per,'%',sep="")) %>% layout(legend=list(x=100,y=0))
    } else if(nrow(df()!=0) & input$dataSelect==1){
      p<-plot_ly(df(), x=~year, y=~per, color=~response, colors="YlOrRd", type="scatter", mode="lines",width=800,height=600,hoverinfo='text',text= ~paste(response,'<br>Percentage: ', per,'%',sep="")) %>% layout(legend=list(xanchor="center",orientation='h',y=-1),autosize=F)
    }
  })
  output$graph<-renderPlotly({
    p()
  })
  output$noGraph<-renderUI({
    if(nrow(df())==0){h4(strong("The selected question was not asked in this country."))}
  })
  #### ####
  ## print error ##
  output$error<-renderText({
    str(df())
    print(df())
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

