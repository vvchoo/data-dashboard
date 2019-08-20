#######################################################
#                   USER INTERFACE                    #
#######################################################
ui <- navbarPage("Snap Poll (testing ver.)",
                 tabPanel("Questions",
                          sidebarPanel(
                            selectInput("dataSelect","Choose Survey Year:",choices=c("2004 Faculty Survey"="qid_2004","2006 Faculty Survey"="qid_2006","2008 Faculty Survey"="qid_2008","2011 Faculty Survey"="qid_2011","2014 Faculty Survey"="qid_2014","2017 Faculty Survey"="qid_2017","All years (selected questions)"="qid_questions"))), # updates go here
                          mainPanel("Questions",uiOutput("questions"),textOutput("test"))),
                 tabPanel("Graph",
                          sidebarPanel(
                            selectInput("test","test",c(1:10))),
                          mainPanel("Graph",plotOutput("graph"))),
                 tags$style(type="text/css", ".shiny-output-error{visibility: hidden;}", ".shiny-output-error:before{visibility:hidden;}")
)

#######################################################
#                       SERVER                        #
#######################################################
server <- function(input, output, session) {
  observeEvent(input$dataSelect, {
    if(input$dataSelect=="All years (selected questions)"){
      all_df<-new_surveys
      list<-lapply(3:8, function(x) unlist(strsplit(qid_overtime[26,x],":")))
      names<-list(names(fac04),names(fac06),names(fac08),names(fac11),names(fac14),names(fac17))
      matched_list<-lapply(1:6, function(x) ifelse(list[[x]]!="",grep(paste(list[[x]],collapse="|"),names(all_df[[x]]),NA)))
      
      for(x in 1:6){ifelse(length(matched_list[[x]])==0,all_df[[x]]<-NA,
                           ifelse(length(matched_list[[x]])==1,all_df[[x]]<-all_df[[x]][,matched_list[[x]][1]],
                           ifelse(length(matched_list[[x]])==2,all_df[[x]]<-all_df[[x]][,matched_list[[x]][1]:matched_list[[x]][2]])))}
    }
  })
  ## create graph ##
  output$graph<-renderPlot({
    ggplot(get(input$dataSelect),aes(x=gender)) +
      geom_bar(stat="count",fill="#ffbfd7") +
      theme(axis.text.x=element_text(angle=45)) + 
      theme_bw()
  })
  ## list of questions ##
  output$questions<-renderUI({
    lapply(1:10, function(x) fluidRow(actionLink(paste0(x),get(input$dataSelect)[[1]][x])))
  })
  
  #listen<-reactive()
  output$test<-renderText({names(input)})
}


shinyApp(ui = ui, server = server)

