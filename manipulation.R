library(shiny)
library(dplyr)
library(skimr)
library(tidyr)
library(ggplot2)
library(plotly)
library(rsconnect)
setwd("C:/Users/Vera/Desktop/TRIP/WEB/SURVEYS")
#fac11<-read.csv("Faculty Survey 4 Wave Data 1.0.csv",stringsAsFactors=FALSE)
#fac14<-read.csv("TRIP_FacultySurvey_2014_Full_2.0.1.csv",stringsAsFactors=FALSE)
fac17<-read.csv("TRIP_FacultySurvey_2017_Intl_Clean_1.1_new.csv",stringsAsFactors=TRUE,encoding="UTF-8")
#us17<-read.csv("C:/Users/Vera/Desktop/TRIP/CSVs/TRIP_FacultySurvey_2017_US_Clean_1.1.csv",stringsAsFactors=TRUE)
fac17_cb<-read.csv("TRIP_FacultySurvey_2017_Codebook.csv",stringsAsFactors=TRUE)
fac17[fac17==""]<-NA

# select only vars in codebook and split up contact groups # 
fac17_cb<-rbind(fac17_cb,data.frame(Question="contact_group",Subquestion="contact_group"))
fac17_unmatch<-fac17[names(fac17) %w/o% fac17_cb$Question] # questions NOT in the codebook #
fac17<-fac17[names(fac17) %in% fac17_cb$Question]

# new data frames #
fac17$contact_group<-gsub(" ","_",fac17$contact_group)
groups<-names(table(fac17$contact_group))

i<-2
for(i in 2:35){
  assign(paste0(tolower(groups[i])),fac17[fac17$contact_group==groups[i],])
  i<-i+1
}

# test ggplot #

fac17 %>% drop_na(Q89) %>%
  ggplot(aes(Q89)) +
  geom_bar(stat="count") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45,hjust=1))






















