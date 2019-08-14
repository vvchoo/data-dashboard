test <- JAD_ref
test <- test %>% filter(IssueArea=="International Security") %>% group_by(Paradigm) %>% mutate(new_n=n())
test <- rbind(test, test)
test <- test %>% group_by(Paradigm) %>% mutate(new_N=n_distinct(pubID)) %>% mutate(new_n=n()) %>% ungroup() %>% mutate(filter=n_distinct(pubID)) %>% mutate(filter_n=n())
test <- test %>% mutate(percent=new_N/new_n/filter/filter_n*100)
View(head(test))

## need to convert filter proportionally, as well as total N ##

table(JAD$Paradigm[JAD$IssueArea=="International Security"]) # realist = 13453
table(JAD$IssueArea=="International Security") # 74789

table(test$filter_n)

(460/2935)/13453
460/2935



ui <- fluidPage(
  titlePanel("Journal Article Database"),
  sidebarLayout(
    sidebarPanel(
      selectInput("addFilter","Filter by..",choices=c("Select...","Contemporary","Epistemology","Focus","Ideational","Issue Area","Level of Analysis","Material","Methodology","Paradigm","Policy Prescription","Region","Time Period")),
      sliderInput("yearInput","Year",1980,2017,c(1980,2017),sep=""), #change when updated
      radioButtons("percentInput","Percentage or frequency",choices=c("Frequency","Percentage")),
      radioButtons("ygInput","Show by..",c("Year","Variable"))),
    mainPanel(plotlyOutput("plotly"),
              tableOutput("view")))
  #tags$style(type="text/css", ".shiny-output-error{visibility: hidden;}", ".shiny-output-error:before{visibility:hidden;}")
)


prop.table(table(JAD_ref$Contemporary[JAD_ref$Focus_Alliances=="Yes"],JAD_ref$Epistemology[JAD_ref$Focus_Alliances=="Yes"],JAD_ref$Paradigm[JAD_ref$Focus_Alliances=="Yes"]),c(1,2))*100

JAD_ref$pubID[JAD_ref$Contemporary=="Yes" & JAD_ref$Paradigm=="Atheoretic/Non" & (JAD_ref$Focus_Alliances=="Yes" | JAD_ref$Focus_BalanceofPower=="Yes") & JAD_ref$Epistemology=="Non-Positivist/Post-Positivist"]



table(JAD_ref$Epistemology)

