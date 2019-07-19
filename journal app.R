library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(rsconnect)
#rsconnect::setAccountInfo(name='vvchoo', token='20318BE8E51FD829619E1D7095A496CB', secret='VUQ9WQai6Vt1CftmuLpcaxg5j3pHNgYReyWhCGZK')
JAD<-read.csv("https://www.dropbox.com/s/6s68rldeenq0z0e/TRIP_JAD_1980_2017.csv?dl=1",stringsAsFactors=FALSE,row.names=NULL)

## reorder variables ##
JAD<-JAD[c(1:10,23:25,36:38,11:22,26:35,39:97)]
JAD<-JAD[-c(2,3,4,7,8,9)]

## rename variables ##
names(JAD)<-c("pubID","journal","year","Paradigm","Ideational","Material","Epistemology","Contemporary","PolicyPrescription","IssueArea","Realism Taken Seriously","Liberalism Taken Seriously","Marxism Taken Seriously","Constructivism Taken Seriously","Non-paradigmatic Taken Seriously","Atheoretic/None Taken Seriously","Realism Synthesis","Liberalism Synthesis","Marxism Synthesis","Constructivism Synthesis","Non-paradigmatic Synthesis","No Synthesis","Pre-history to 476 AD","476 AD to October 1648","October 1648 to June 28, 1914","June 28, 1914 to June 28, 1919","June 28, 1919 to September 1, 1939","September 1, 1939 to August 1945","September 1945 to November 9, 1989","November 9, 1989 to September 1, 2001","September 1, 2001 to Present","No time period","Individual Level","State Level","International Level", "No Level","Analytic/non-formal","Counterfactual","Descriptive","Experimental","Formal Modeling","Policy analysis","Qualitative","Quantitative","Antarctica","Canada/Western Europe","East Asia","FSU / Eastern Europe","Global","Latin America","Middle East / North Africa","No region","Oceania","South Asia","Southeast Asia","Subsaharan Africa","United States","Alliances","Balance of Power","Bargaining Deterrence Strategy","Development","Diplomacy","Domestic Politics","Economic Interdependence","Environment","Ethnicity/Religion","Foreign Aid","Foreign Policy","Gender","Humanitarian","Intergovernmental Organization","Interstate Crisis","Interstate War","International Law","Intrastate Conflict","Discipline of IR","International Regimes","Migration","Monetary Policy","Non-governmental Organizations","North-South Relations","Public Health","Public Opinion","Regime Type","Regional Integration","Sanctions","Terrorism","Trade","Weapons Systems","Weapons of Mass Destruction Proliferation","Other")

## rewrite JAD yes/no ##
i<-11
for(i in 11:91){
  JAD[i]<-ifelse(JAD[i]=="Yes",JAD[i]<-names(JAD[i]),JAD[i]<-"")
  i<-i+1
}

## gather data set ##
JAD <- JAD %>% 
  gather(TimePeriod, TimePeriod_value,`Pre-history to 476 AD`:`No time period`) %>% 
  filter(TimePeriod_value!="") %>%
  gather(Level,Level_value,`Individual Level`:`No Level`) %>% 
  filter(Level_value!="") %>%
  gather(Focus,Focus_value,`Alliances`:`Other`) %>% 
  filter(Focus_value!="") %>%
  gather(Seriously,Seriously_value,`Realism Taken Seriously`:`Atheoretic/None Taken Seriously`) %>% 
  filter(Seriously_value!="") %>%
  gather(Synthesis,Synthesis_value,`Realism Synthesis`:`No Synthesis`) %>% 
  filter(Synthesis_value!="") %>%
  gather(Methodology,Methodology_value,`Analytic/non-formal`:`Quantitative`) %>% 
  filter(Methodology_value!="") %>%
  gather(Region,Region_value,`Antarctica`:`United States`) %>% 
  filter(Region_value!="") %>%
  select(pubID,journal,year,Paradigm,Epistemology,IssueArea,Ideational,Material,Contemporary,PolicyPrescription,TimePeriod,Level,Focus,Seriously,Synthesis,Methodology,Region)

JAD$TimePeriod <- factor(JAD$TimePeriod, levels=c("Pre-history to 476 AD","476 AD to October 1648","October 1648 to June 28, 1914","June 28, 1914 to June 28, 1919","June 28, 1919 to September 1, 1939","September 1, 1939 to August 1945","September 1945 to November 9, 1989","November 9, 1989 to September 1, 2001","September 1, 2001 to Present","No time period"))

## set up plot theme ##
plot_theme<-theme_bw() +
            theme(axis.text.x=element_text(angle=50), axis.title.x=element_text(vjust=1.5),axis.title.y=element_text(hjust=1.5))
allfilters<-c("hello")

######################################################################################
#                                    USER INTERFACE                                  #
######################################################################################
ui <- fluidPage(
          titlePanel("Journal Article Database"),
          fluidRow(
            column(4,
              wellPanel(
                h4(b("See all articles which..")),
              selectInput("addFilter","Filter by..",choices=c("Select...","Contemporary","Epistemology","Focus","Ideational","Issue Area","Level of Analysis","Material","Methodology","Paradigm","Policy Prescription","Region","Time Period"))),
            wellPanel(
                h4("Year range"),
               sliderInput("yearInput","Year",1980,2017,c(1980,2017),sep="")), #change when updated
          wellPanel(
                h4("Show by..."),
               radioButtons("percentInput","",choices=c("Frequency","Percentage")),
               radioButtons("ygInput","Show by..",c("Year","Variable")))),
            column(8,
                  plotlyOutput("plotly"),
                  tableOutput("view"))))
          #tags$style(type="text/css", ".shiny-output-error{visibility: hidden;}", ".shiny-output-error:before{visibility:hidden;}")


#######################################################################################
#                                       SERVER                                        #
#######################################################################################
server <- function(input, output, session) {
## add new filter button ##  
observeEvent(input$addFilter, {
      if(input$addFilter=="Paradigm") {
      insertUI(
        selector="#yearInput",
        where="afterEnd",
        ui=tags$div(selectInput("paradigmFilter", "Paradigm",multiple=TRUE,choices=c("All",names(table(JAD$Paradigm))))))
    } else if(input$addFilter=="Epistemology"){
      insertUI(
        selector="#yearInput",
        where="afterEnd",
        ui=tags$div(selectInput("epistFilter", "Epistemology",multiple=TRUE,choices=c("All",names(table(JAD$Epistemology))))))
    } else if(input$addFilter=="Policy Prescription"){
      insertUI(
        selector="#yearInput",
        where="afterEnd",
        ui=tags$div(selectInput("policyFilter", "Policy Prescription",multiple=TRUE,choices=c("All","Yes","No"))))
    } else if(input$addFilter=="Contemporary"){
      insertUI(
        selector="#yearInput",
        where="afterEnd",
        ui=tags$div(selectInput("contempFilter", "Contemporary",multiple=TRUE,choices=c("All","Yes","No"))))
    } else if(input$addFilter=="Level of Analysis"){
      insertUI(
        selector="#yearInput",
        where="afterEnd",
        ui=tags$div(selectInput("levelFilter", "Level of Analysis",multiple=TRUE,choices=c(names(table(JAD$Level))))))
    } else if(input$addFilter=="Ideational"){
      insertUI(
        selector="#yearInput",
        where="afterEnd",
        ui=tags$div(selectInput("ideaFilter", "Ideational",multiple=TRUE,choices=c("All","Yes","No"))))
    } else if(input$addFilter=="Issue Area"){
      insertUI(
        selector="#yearInput",
        where="afterEnd",
        ui=tags$div(selectInput("issueFilter", "Issue Area",multiple=TRUE,choices=c("All",names(table(JAD$IssueArea))))))
    } else if(input$addFilter=="Region"){
      insertUI(
        selector="#yearInput",
        where="afterEnd",
        ui=tags$div(selectInput("regionFilter", "Region",multiple=TRUE,choices=c("All",names(table(JAD$Region))))))
    } else if(input$addFilter=="Material"){
      insertUI(
        selector="#yearInput",
        where="afterEnd",
        ui=tags$div(selectInput("materialFilter","Material",multiple=TRUE,choices=c("All","Yes","No"))))
    } else if(input$addFilter=="Methodology"){
      insertUI(
        selector="#yearInput",
        where="afterEnd",
        ui=tags$div(selectInput("methodFilter", "Methodology",multiple=TRUE,choices=c("All",names(table(JAD$Methodology))))))
    } else if(input$addFilter=="Time Period"){
      insertUI(
        selector="#yearInput",
        where="afterEnd",
        ui=tags$div(selectInput("timeFilter", "Time Period",multiple=TRUE,choices=c("All",names(table(JAD$TimePeriod))))))
    } else if(input$addFilter=="Focus"){ 
      insertUI(
        selector="#yearInput",
        where="afterEnd",
        ui=tags$div(selectInput("focusFilter", "Focus area",multiple=TRUE,choices=c("All",names(table(JAD$Focus))))))
    }
})

## change x-axis filters ##
observeEvent(input$ygInput, { 
  if(input$ygInput=="Variable"){
  insertUI(
    selector="#ygInput",
    where="afterEnd",
    ui=tags$div(id="varFilter",selectInput("varFilter","X-Axis", c("Journal"="journal","Paradigm","Epistemology","Issue Area"="IssueArea","Time Period"="TimePeriod","Level","Methodology","Region","Focus","Policy Prescription"="PolicyPrescription"))))
  } else {
    removeUI(selector="div:has(> #varFilter)")
  }
})

#### create filtered data set ####
allfilters<-c("hello","world")
filtered<-reactive({
  data<-JAD
  
  if(input$ygInput=="Year"){data<-data %>% group_by(year) %>% mutate(year_distinct=n_distinct(pubID))}  # total distinct articles a year #
  
## filter conditions ##
  if(!is.null(input$contempFilter)){data<-data[data$Contemporary %in% input$contempFilter,]}
                                    #allfilters<-c(allfilters,input$contempFilter)
  if(!is.null(input$epistFilter)){data<-data[data$Epistemology %in% input$epistFilter,]}
                                  #allfilters<-c(allfilters,input$epistFilter)
  if(!is.null(input$focusFilter)){data<-data[data$Focus %in% input$focusFilter,]}
                                  #allfilters<-c(allfilters,input$focusFilter)
  if(!is.null(input$ideaFilter)){data<-data[data$Ideational %in% input$ideaFilter,]}
                                 #allfilters<-c(allfilters,input$ideaFilter)
  if(!is.null(input$issueFilter)){data<-data[data$IssueArea %in% input$issueFilter,]}
                                  #allfilters<-c(allfilters,input$issueFilter)
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
      data<-data %>% group_by_(input$varFilter) %>% mutate(filter=n_distinct(pubID)) %>% mutate(filter_n=n()) # find unique articles in x-axis #
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
      data<-data %>% group_by_(input$varFilter) %>% mutate(new_n=n_distinct(pubID)) %>% mutate(filter_n=n())  # find how articles in x-axis #
      data<-data %>% ungroup() %>% mutate(filter=n_distinct(pubID))# find unique articles #
      data<-data %>% mutate(Percentage=(new_n/filter/filter_n)*100)} # create actual percentages to sum for stat_summary #
  
  data<-as.data.frame(data)
})

output$view<-renderTable({
  table(allfilters)
})

## create plot ##
output$plotly <- renderPlotly({
  if(input$percentInput=="Percentage" & input$ygInput=="Variable"){ 
    p<-ggplot(filtered(), aes_string(x=input$varFilter)) +
       stat_summary(aes(y=Percentage),fun.y="sum",geom="bar",position="identity",fill="#a2c6da") +
       ylab("Percentage") +
       plot_theme 
  } else if (input$percentInput=="Frequency" & input$ygInput=="Variable") {
    p<-ggplot(filtered(), aes_string(x=input$varFilter)) +
      stat_summary(aes(y=Count),fun.y="sum",geom="bar",position="identity",fill="#a2c6da") +
       ylab("Frequency") +
       plot_theme 
  } else if (input$percentInput=="Percentage" & input$ygInput=="Year") {
    p<-ggplot(filtered(), aes(x=year)) +
       stat_summary(aes(y=Percentage),fun.y="sum",geom="bar",position="identity",fill="#a2c6da") +
       scale_x_continuous(breaks=seq(1980,2017,5),labels=seq(1980,2017,5)) +
       ylab("Percentage") +
       xlab("Year") +
       plot_theme
  } else { # frequency by year #
    p<-ggplot(filtered(), aes(x=year)) +
       stat_summary(aes(y=Count),fun.y="sum",geom="bar",position="identity",fill="#a2c6da") +
       scale_x_continuous(labels=seq(1980,2017,5),breaks=seq(1980,2017,5)) +
       xlab("Year") +
       ylab("Frequency") +
       plot_theme
  }
  
  ggplotly(p, height=700) %>%
    layout(margin=list(b=300,l=100),autosize=T)
})

}

shinyApp(ui = ui, server = server)

