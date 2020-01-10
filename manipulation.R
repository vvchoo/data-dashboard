# This does XXXXX
# Code developed by Vera Choo, with reference from: XXXX

library(shiny)
library(dplyr)
library(skimr)
library(tidyr)
library(ggplot2)
library(plotly)
library(rsconnect)
setwd("C:/Users/Vera/Desktop/FALL 2019/TRIP/WEB/SURVEYS")
#setwd("C:/Users/ejparajon/Google Drive/TRIP/Database Management/Shiny App material/Faculty Survey App/CSVs")
# fac04<-read.csv("FS2004.csv",stringsAsFactors=TRUE,na.strings=c("","NULL","NA"))
# fac06<-read.csv("FS2006.csv",stringsAsFactors=TRUE,na.strings=c("","NULL","NA"))
# fac08<-read.csv("FS2008.csv",stringsAsFactors=TRUE,na.strings=c("","NULL","NA"))
fac11<-read.csv("Faculty Survey 4 Wave Data 1.0.csv",stringsAsFactors=TRUE,na.strings=c("","NULL","NA"))
fac14<-read.csv("TRIP_FacultySurvey_2014_Full_2.0.1.csv",stringsAsFactors=TRUE,na.strings=c("","NULL","NA"))
fac17<-read.csv("TRIP_FacultySurvey_2017_Intl_Clean_1.1_new.csv",stringsAsFactors=TRUE,encoding="UTF-8",na.strings=c("","NULL","NA"))
#fac17_cb<-read.csv("TRIP_FacultySurvey_2017_Codebook.csv",stringsAsFactors=TRUE,na.strings="")

# create codebook #
qid_overtime<-read.csv("FacultySurvey_QIDovertime.csv",stringsAsFactors=FALSE,na.strings="")
qid_overtime$qid_all<-""
qid_overtime$qid_all[qid_overtime$`2004`!="" & qid_overtime$`2006`!="" & qid_overtime$`2008`!="" & qid_overtime$`2011`!="" & qid_overtime$`2014`!="" & qid_overtime$`2017`!=""]<-"ref"
names(qid_overtime)[4:9]<-c("2004","2006","2008","2011","2014","2017")
qid_overtime$Question_text<-gsub("\n"," ",qid_overtime$Question_text)
qid_overtime$Question_text<-trimws(qid_overtime$Question_text)
qid_overtime<-qid_overtime[,c(2,4:9)]
qid_all<-qid_overtime[,c(1:7)][qid_overtime$`2004`!="" & qid_overtime$`2006`!="" & qid_overtime$`2008`!="" & qid_overtime$`2011`!="" & qid_overtime$`2014`!="" & qid_overtime$`2017`!="",]
#qid_2004<-qid_overtime[,c(1:3)] %>% filter(2004!="")
#qid_2006<-qid_overtime[,c(1,2,4)] %>% filter(2006!="")
#qid_2008<-qid_overtime[,c(1,2,5)] %>% filter(2008!="")
#qid_2011<-qid_overtime[,c(1,2,6)] %>% filter(2011!="")
#qid_2014<-qid_overtime[,c(1,2,7)] %>% filter(2014!="")
#qid_2017<-qid_overtime[,c(1,2,8)] %>% filter(2017!="")

#qid_list<-list("qid_all"=qid_all, "qid_2004"=qid_2004,"qid_2006"=qid_2006,"qid_2008"=qid_2008,"qid_2011"=qid_2011,"qid_2014"=qid_2014,"qid_2017"=qid_2017,"qid_questions"=qid_questions)
#lapply(1:length(qid_list), function(i) write.csv(qid_list[[i]], file = paste0(names(qid_list[i]), ".csv"), row.names = FALSE))
#list<-lapply(1:7, function(i) noquote(paste0(qid_list[[i]][,3],collapse=",")))

# new datasets #
fac04<-fac11 %>% filter(surveyYear==2004) %>% select(surveyId=standardId,age=respondentBirthYear,gender=respondentGender,rank=fsqg_29,surveyCountry,surveyYear,fsqg_99,fsqg_108,fsqg_1139,fsqg_100,fsqg_737,fsqg_87_459:fsqg_87_465,fsqg_318_1837:fsqg_318_123129,fsqg_320,fsqg_1242_122629:fsqg_1242_122631,fsqg_1243_122632:fsqg_1243_122634,fsqg_59,fsqg_69_331:fsqg_69_339,fsqg_793,fsqg_88,fsqg_83,fsqg_706,fsqg_1238_122608:fsqg_1238_122613,fsqg_1090,fsqg_57,fsqg_84,fsqg_85_447:fsqg_85_455,fsqg_86,fsqg_1122_3555:fsqg_1122_3559,fsqg_66_5409:fsqg_66_5413,fsqg_1153_4940:fsqg_1153_4989,fsqg_1091_3437:fsqg_1091_3440,fsqg_1092_683:fsqg_1092_686,contains("fsqg_1093"),fsqg_39,fsqg_316_123058:fsqg_316_123086,fsqg_707,fsqg_708_400:fsqg_708_6067,fsqg_80,fsqg_81_427:fsqg_81_6080,fsqg_1,fsqg_2_1950,fsqg_6_38:fsqg_6_123128,fsqg_848_22:fsqg_848_5559,fsqg_7,fsqg_1043,fsqg_8,fsqg_819,fsqg_117,fsqg_327_1951:fsqg_327_1955,fsqg_1235_122596:fsqg_1235_122598,fsqg_13,fsqg_1236_122599:fsqg_1236_122601,fsqg_327_1951:fsqg_327_1955,fsqg_10,fsqg_1237_122602:fsqg_1237_122607)

fac06<-fac11 %>% filter(surveyYear==2006) %>% select(surveyId=standardId,age=respondentBirthYear,gender=respondentGender,rank=fsqg_29,surveyCountry,surveyYear,fsqg_180,fsqg_181,fsqg_1233_122567,fsqg_1234_122582:fsqg_1234_122595,fsqg_108,fsqg_1139,fsqg_647,fsqg_100,fsqg_178,fsqg_179_1042:fsqg_179_3951,contains("fsqg_1227"),contains("fsqg_1261"),fsqg_318_1837:fsqg_318_123129,fsqg_320,fsqg_1227_122510:fsqg_1227_122515,fsqg_1016_743:fsqg_1016_6264,fsqg_793,fsqg_88,fsqg_83,fsqg_706,fsqg_172,fsqg_173,fsqg_174_998:fsqg_174_1007,fsqg_144_737:fsqg_144_741,fsqg_1122_3555:fsqg_1122_3559,fsqg_1121_5484:fsqg_1121_5488,fsqg_1153_4940:fsqg_1153_4989,fsqg_1091_3437:fsqg_1091_3440,fsqg_1092_683:fsqg_1092_686,fsqg_1095_695:fsqg_1095_698,contains("fsqg_1093"),fsqg_39,fsqg_316_123058:fsqg_316_123086,fsqg_707,fsqg_708_400:fsqg_708_6067,fsqg_80,fsqg_81_427:fsqg_81_6080,fsqg_6_38:fsqg_6_123128,fsqg_1040_1917,fsqg_848_22:fsqg_848_5559,fsqg_1043,fsqg_31_175,fsqg_1044_176,fsqg_117,fsqg_122_629:fsqg_122_632,fsqg_13,fsqg_1225_122501:fsqg_1225_122503,fsqg_122_629:fsqg_122_632)

fac08<-fac11 %>% filter(surveyYear==2008) %>% select(surveyId=standardId,age=respondentBirthYear,gender=respondentGender,rank=fsqg_29,surveyCountry,surveyYear,fsqg_635,fsqg_180,fsqg_181,fsqg_1251_122693:fsqg_1251_122711,fsqg_1252_122712:fsqg_1252_122729,fsqg_1141,fsqg_1144,fsqg_108,fsqg_1139,fsqg_647,fsqg_100,fsqg_668,fsqg_178,fsqg_179_1042:fsqg_179_3951,fsqg_714_3952:fsqg_714_5701,fsqg_1248_122672:fsqg_1248_122678,fsqg_1248_122672:fsqg_1248_122678,fsqg_1247_122655:fsqg_1247_122671,fsqg_1016_743:fsqg_1016_6264,fsqg_793,fsqg_88,fsqg_83,fsqg_706,fsqg_1071,fsqg_1090,fsqg_1070,fsqg_1123_5467:fsqg_1123_5471,fsqg_1122_3555:fsqg_1122_3559,fsqg_1121_5484:fsqg_1121_5488,fsqg_1153_4940:fsqg_1153_4989,fsqg_1091_3437:fsqg_1091_3440,fsqg_1092_683:fsqg_1092_686,fsqg_1109_5807:fsqg_1109_5810,fsqg_1095_695:fsqg_1095_698,contains("fsqg_1093"),fsqg_39,fsqg_794,fsqg_1068,fsqg_707,fsqg_708_400:fsqg_708_6067,fsqg_80,fsqg_81_427:fsqg_81_6080,fsqg_663,fsqg_6_38:fsqg_6_123128,fsqg_1040_1917,fsqg_848_22:fsqg_848_5559,fsqg_1043,fsqg_819,fsqg_1045_5569,fsqg_1244_122635:fsqg_1244_122638,fsqg_1245_122639:fsqg_1245_122646)

fac11<-fac11 %>% filter(surveyYear==2011) %>% select(surveyId=standardId,age=respondentBirthYear,gender=respondentGender,rank=fsqg_29,surveyCountry,surveyYear,fsqg_635,fsqg_949,fsqg_180,fsqg_181,fsqg_879_4838:fsqg_879_4866,fsqg_808_4472:fsqg_1252_122730,fsqg_1144,fsqg_668,fsqg_737,fsqg_178,fsqg_179_1042:fsqg_179_3951,fsqg_714_3952:fsqg_714_5701,fsqg_621,fsqg_626,fsqg_1016_743:fsqg_1016_6264,fsqg_795,fsqg_793,fsqg_88,fsqg_83,fsqg_706,fsqg_992,fsqg_84,fsqg_1123_5467:fsqg_1123_5471,fsqg_1122_3555:fsqg_1122_3559,fsqg_1121_5484:fsqg_1121_5488,fsqg_1153_4940:fsqg_1153_4989,fsqg_1091_3437:fsqg_1091_3440,contains("fsqg_1272"),contains("fsqg_1017"),fsqg_630_3441:fsqg_630_3444,contains("fsqg_1256"),contains("fsqg_1257"),fsqg_39,fsqg_794,fsqg_707,fsqg_708_400:fsqg_708_6067,fsqg_80,fsqg_81_427:fsqg_81_6080,fsqg_663,fsqg_784_4244:fsqg_784_4247,fsqg_644,fsqg_848_22:fsqg_848_5559,fsqg_643_3524:fsqg_643_3539,fsqg_1044_176,fsqg_624,fsqg_852,fsqg_833,fsqg_834,fsqg_835,fsqg_639,fsqg_701,fsqg_1320,fsqg_1264_122851:fsqg_1264_122858)

fac14<-fac14 %>% select(surveyId=survey_id,age=qg_231_1891,gender=qg_99,rank=qg_92,surveyCountry=contact_group,qg_427_3218:qg_427_3229_other,qg_230,qg_408_3073,qg_113,qg_113,qg_241,qg_105,qg_153_1487:qg_153_4082,qg_151_1517:qg_151_4083,qg_171_1523,qg_263_2143,qg_271,qg_142,qg_131,qg_104,qg_146_1146:qg_146_1169,qg_399_3020:qg_399_3019,qg_507,qg_508,qg_509_4182:qg_509_4188,qg_510_4190:qg_510_4197_other,qg_159_644:qg_159_642,qg_154_686:qg_154_684,qg_470,qg_260,qg_127,qg_97,qg_95,qg_117,qg_102,qg_103,qg_145_1074:qg_145_1079,qg_423,qg_158_633:qg_158_629,qg_184_1424:qg_184_1427,qg_183_693:qg_183_697,qg_182_669:qg_182_673,qg_185_674:qg_185_677,contains("qg_388"),contains("qg_236"),contains("qg_188"),contains("qg_235"),qg_93,qg_141,qg_118,qg_149_892:qg_149_2985,qg_89,qg_144_3216:qg_144_1041,qg_221_1834:qg_221_1835,qg_221_1838:qg_221_3974,qg_115,qg_139,qg_111,qg_398_3001:qg_398_3010,qg_243,qg_256_2071:qg_256_2082,qg_429,qg_430,qg_255_2057:qg_255_4051,qg_479_3987,qg_479_3989,qg_479_3991) %>% mutate(surveyYear=2014)

fac17<-fac17 %>% select(surveyId=ResponseId,age=birth_year,gender,rank=qg_92,surveyCountry=contact_group,qg_230,qg_112b,Q102_1,Q102_2,Q102_3,Q102_4,Q102_5,Q102_6,Q102_7,Q102_8,Q102_9:Q102_11_TEXT,qg_105,Q105,qg_153,qg_151,qg_171_1,qg_263_1,Q143_1,qg_271,Q4492,qg_104,qg_146,qg_154,qg_470,qg_260,Q10,qg_97,qg_95,qg_117,qg_117a,qg_117b,qg_117c,Q298_1:Q298_5,qg_183_1:qg_183_5,qg_182_1:qg_182_5,qg_185_1:qg_185_4,contains("Q291"),contains("qg_235"),qg_93,qg_118,qg_149,qg_89,qg_144,contains("Q101"),qg_139,qg_243) %>% mutate(surveyYear=2017)

levels(fac14$age)<-list("20-34"=as.character(c(20:34)),"35-44"=as.character(c(35:44)),"45-54"=as.character(c(45:54)),"55-64"=as.character(c(55:64)),"65 and up"=as.character(c(65:90)))
fac17$age<-factor(as.character(2017-fac17$age))
levels(fac17$age)<-list("20-34"=as.character(c(20:34)),"35-44"=as.character(c(35:44)),"45-54"=as.character(c(45:54)),"55-64"=as.character(c(55:64)),"65 and up"=as.character(c(65:90)))

levels(fac04$gender)<-c("Female","Male")
levels(fac06$gender)<-c("Female","Male")
levels(fac08$gender)<-c("Female","Male")
levels(fac11$gender)<-c("Female","Male")
levels(fac17$gender)<-c("Female","Male")

levels(fac04$rank)<-list("Adjunct/visiting/lecturer"=levels(fac04$rank)[c(1,5,7,13,26,27)],
                         "Associate"=levels(fac04$rank)[c(3,4,19)],
                         "Assistant"=levels(fac04$rank)[c(2,14,15)],
                         "Full/chaired/emeritus"=levels(fac04$rank)[c(6,10,12,18)],
                         "Other (including fellows/post-docs)"=levels(fac04$rank)[c(8,9,11,16,17,20,23,24,25)])
levels(fac06$rank)<-list("Adjunct/visiting/lecturer"=levels(fac06$rank)[c(1,5,7,13,26,27)],
                         "Associate"=levels(fac06$rank)[c(3,4,19)],
                         "Assistant"=levels(fac06$rank)[c(2,14,15)],
                         "Full/chaired/emeritus"=levels(fac06$rank)[c(6,10,12,18)],
                         "Other (including fellows/post-docs)"=levels(fac06$rank)[c(8,9,11,16,17,20,23,24,25)])
levels(fac08$rank)<-list("Adjunct/visiting/lecturer"=levels(fac08$rank)[c(1,5,7,13,26,27)],
                         "Associate"=levels(fac08$rank)[c(3,4,19)],
                         "Assistant"=levels(fac08$rank)[c(2,14,15)],
                         "Full/chaired/emeritus"=levels(fac08$rank)[c(6,10,12,18)],
                         "Other (including fellows/post-docs)"=levels(fac08$rank)[c(8,9,11,16,17,20,23,24,25)])
levels(fac11$rank)<-list("Adjunct/visiting/lecturer"=levels(fac11$rank)[c(1,5,7,13,26,27)],
                         "Associate"=levels(fac11$rank)[c(3,4,19)],
                         "Assistant"=levels(fac11$rank)[c(2,14,15)],
                         "Full/chaired/emeritus"=levels(fac11$rank)[c(6,10,12,18)],
                         "Other (including fellows/post-docs)"=levels(fac11$rank)[c(8,9,11,16,17,20,23,24,25)])
levels(fac14$rank)<-list("Adjunct/visiting/lecturer"=levels(fac14$rank)[c(2,3,7,14,24,25:29,35,48,50,52,53)],
                         "Associate"=levels(fac14$rank)[c(1,7,8,9,10,41,45,46,36,38,39,41)],
                         "Assistant"=levels(fac14$rank)[c(4,5,6,30)],
                         "Full/chaired/emeritus"=levels(fac14$rank)[c(13,17,19,21,22,23,37,40)],
                         "Other (including fellows/post-docs)"=levels(fac14$rank)[c(11,12,15,16,18,20,24,31:33,34,42,43,44,47,49,51,54,55)])
levels(fac17$rank)<-list("Adjunct/visiting/lecturer"=levels(fac17$rank)[c(1,2,10,11,12)],
                         "Associate"=levels(fac17$rank)[c(5,18)],
                         "Assistant"=levels(fac17$rank)[c(3,4)],
                         "Full/chaired/emeritus"=levels(fac17$rank)[c(6,7,8,9,16,17)],
                         "Other (including fellows/post-docs)"=levels(fac17$rank)[c(13,14,15)])
levels(fac08$fsqg_668)<-unlist(strsplit(levels(fac08$fsqg_668),"\\."))

#lapply(1:length(fac06), function(x) grep("Demonym", fac06[[x]]))

levels(fac08[[103]])<-"RESPONDENT COUNTRY"
levels(fac06[[59]])<-"RESPONDENT COUNTRY"
levels(fac11$fsqg_822)[2]<-"Don't know"
levels(fac11$fsqg_1016_748)<-"RESPONDENT COUNTRY's government"
levels(fac11$fsqg_1112_757)<-"RESPONDENT COUNTRY'government"
levels(fac14$qg_154_686)<-"RESPONDENT COUNTRY's government"
levels(fac14$qg_149_892)<-"RESPONDENT COUNTRY's foreign policy"
levels(fac17$qg_230)[1:2]<-"RESPONDENT COUNTRY"
levels(fac17$qg_230)[2:3]<-"Other"
levels(fac17$qg_118)[1]<-"RESPONDENT COUNTRY's Foreign Policy"
levels(fac17$qg_154)<-gsub("\\$\\{e://Field/CountryAdj\\}","RESPONDENT COUNTRY's",levels(fac17$qg_154))
levels(fac17$qg_93)<-gsub("\\$\\{e://Field/CountryAdj\\}","RESPONDENT COUNTRY's",levels(fac17$qg_93))
levels(fac17$qg_149)<-gsub("\\$\\{e://Field/CountryAdj\\}","RESPONDENT COUNTRY's",levels(fac17$qg_149))

# relevel institutions
crosswalk<-read.csv("InstitutionsCrosswalk_20180411.csv",na.strings=c(""),encoding="UTF-8")

# undergrad programs
fac17<-as.data.frame(cbind(fac17,undergrad_1=as.character(crosswalk$standardized[match(tolower(fac17$Q298_1),crosswalk$original)])))
fac17<-as.data.frame(cbind(fac17,undergrad_2=as.character(crosswalk$standardized[match(tolower(fac17$Q298_2),crosswalk$original)])))
fac17<-as.data.frame(cbind(fac17,undergrad_3=as.character(crosswalk$standardized[match(tolower(fac17$Q298_3),crosswalk$original)])))
fac17<-as.data.frame(cbind(fac17,undergrad_4=as.character(crosswalk$standardized[match(tolower(fac17$Q298_4),crosswalk$original)])))
fac17<-as.data.frame(cbind(fac17,undergrad_5=as.character(crosswalk$standardized[match(tolower(fac17$Q298_5),crosswalk$original)])))
fac17<-fac17 %>% select(-c(Q298_1:Q298_5))

# masters programs
fac17<-as.data.frame(cbind(fac17,masters_1=as.character(crosswalk$standardized[match(tolower(fac17$qg_183_1),crosswalk$original)])))
fac17<-as.data.frame(cbind(fac17,masters_2=as.character(crosswalk$standardized[match(tolower(fac17$qg_183_2),crosswalk$original)])))
fac17<-as.data.frame(cbind(fac17,masters_3=as.character(crosswalk$standardized[match(tolower(fac17$qg_183_3),crosswalk$original)])))
fac17<-as.data.frame(cbind(fac17,masters_4=as.character(crosswalk$standardized[match(tolower(fac17$qg_183_4),crosswalk$original)])))
fac17<-as.data.frame(cbind(fac17,masters_5=as.character(crosswalk$standardized[match(tolower(fac17$qg_183_5),crosswalk$original)])))
fac17<-fac17 %>% select(-c(qg_183_1:qg_183_5))

# phd programs
fac17<-as.data.frame(cbind(fac17,phd_1=as.character(crosswalk$standardized[match(tolower(fac17$qg_182_1),crosswalk$original)])))
fac17<-as.data.frame(cbind(fac17,phd_2=as.character(crosswalk$standardized[match(tolower(fac17$qg_182_2),crosswalk$original)])))
fac17<-as.data.frame(cbind(fac17,phd_3=as.character(crosswalk$standardized[match(tolower(fac17$qg_182_3),crosswalk$original)])))
fac17<-as.data.frame(cbind(fac17,phd_4=as.character(crosswalk$standardized[match(tolower(fac17$qg_182_4),crosswalk$original)])))
fac17<-as.data.frame(cbind(fac17,phd_5=as.character(crosswalk$standardized[match(tolower(fac17$qg_182_5),crosswalk$original)])))
fac17<-fac17 %>% select(-c(qg_182_1:qg_182_5))

# scholars
scholars<-read.csv("PeopleCrosswalk_20180608.csv",na.strings=c(""),encoding="UTF-8")
scholars$original<-tolower(scholars$original)
fac17<-as.data.frame(cbind(fac17,scholar_1=as.character(scholars$standardized[match(tolower(fac17$qg_185_1),scholars$original)])))
fac17<-as.data.frame(cbind(fac17,scholar_2=as.character(scholars$standardized[match(tolower(fac17$qg_185_2),scholars$original)])))
fac17<-as.data.frame(cbind(fac17,scholar_3=as.character(scholars$standardized[match(tolower(fac17$qg_185_3),scholars$original)])))
fac17<-as.data.frame(cbind(fac17,scholar_4=as.character(scholars$standardized[match(tolower(fac17$qg_185_4),scholars$original)])))
fac17<-fac17 %>% select(-c(qg_185_1:qg_185_4))

### reorder fac04 ##
fac04_levels<-lapply(1:length(fac04), function(x) levels(fac04[[x]]))
# for(x in 1:length(fac04)){
#   if(length(fac04_levels[[x]])>=3 & length(fac04_levels[[x]])<=7){
#     print(x)
#     print(fac04_levels[x])
#   }
# }

fac04[[4]]<-factor(fac04[[4]],levels(fac04[[4]])[c(1,3,2,4,5)])
fac04[[8]]<-factor(fac04[[8]],levels(fac04[[8]])[c(2,1,3)])
fac04[[9]]<-factor(fac04[[9]],levels(fac04[[9]])[c(1,4,3,2)])
fac04[[10]]<-factor(fac04[[10]],levels(fac04[[10]])[c(3,2,1,5,4)])
fac04[[11]]<-factor(fac04[[11]],levels(fac04[[11]])[c(5,3,6,2,4,1)])
fac04[[24]]<-factor(fac04[[24]],levels(fac04[[24]])[c(1,3,2,4)])
fac04[[25]]<-factor(fac04[[25]],levels(fac04[[25]])[c(1,3,2)])
fac04[[26]]<-factor(fac04[[26]],levels(fac04[[26]])[c(1,3,2)])
fac04[[27]]<-factor(fac04[[27]],levels(fac04[[27]])[c(1,3,2)])
fac04[[28]]<-factor(fac04[[28]],levels(fac04[[28]])[c(1,3,2)])
fac04[[29]]<-factor(fac04[[29]],levels(fac04[[29]])[c(1,3,2)])
fac04[[30]]<-factor(fac04[[30]],levels(fac04[[30]])[c(1,3,2)])
fac04[[41]]<-factor(fac04[[41]],levels(fac04[[41]])[c(1,3,2)])
fac04[[42]]<-factor(fac04[[42]],levels(fac04[[42]])[c(4,2,3,1,5)])
fac04[[45]]<-factor(fac04[[45]],levels(fac04[[45]])[c(3,1,2)])
fac04[[46]]<-factor(fac04[[46]],levels(fac04[[46]])[c(3,1,2)])
fac04[[47]]<-factor(fac04[[47]],levels(fac04[[47]])[c(3,1,2)])
fac04[[48]]<-factor(fac04[[48]],levels(fac04[[48]])[c(3,1,2)])
fac04[[49]]<-factor(fac04[[49]],levels(fac04[[49]])[c(3,1,2)])
fac04[[50]]<-factor(fac04[[50]],levels(fac04[[50]])[c(3,1,2)])
fac04[[51]]<-factor(fac04[[51]],trimws(levels(fac04[[51]])[c(1,3,2)]))
fac04[[52]]<-factor(fac04[[52]],levels(fac04[[52]])[c(2,1,3)])
fac04[[63]]<-factor(fac04[[63]],levels(fac04[[63]])[c(1,3,2)])
fac04[[200]]<-factor(fac04[[200]],trimws(levels(fac04[[200]])[c(3,1,5,2,4)]))
fac04[[201]]<-factor(fac04[[201]],trimws(levels(fac04[[201]])[c(4,2,1,3,5)]))
fac04[[202]]<-factor(fac04[[202]],trimws(levels(fac04[[202]])))
fac04[[204]]<-factor(fac04[[204]],trimws(levels(fac04[[204]])[c(1,2,4,5,3)]))
fac04[[210]]<-factor(fac04[[210]],levels(fac04[[210]])[c(1,3,2)])
fac04[[211]]<-factor(fac04[[211]],levels(fac04[[211]])[c(1,3,2)])
fac04[[212]]<-factor(fac04[[212]],levels(fac04[[212]])[c(1,3,2)])
fac04[[214]]<-factor(fac04[[214]],levels(fac04[[214]])[c(1,3,2)])
fac04[[215]]<-factor(fac04[[215]],levels(fac04[[215]])[c(1,3,2)])
fac04[[216]]<-factor(fac04[[216]],levels(fac04[[216]])[c(1,3,2)])

#### reorder fac06 ####
fac06_levels<-lapply(1:length(fac06), function(x) levels(fac06[[x]]))
# for(x in 1:length(fac06)){
#   if(length(fac06_levels[[x]])>=3 & length(fac06_levels[[x]])<=7){
#     print(x)
#     print(fac06_levels[x])
#   }
# }

fac06[[4]]<-factor(fac06[[4]],levels(fac06[[4]])[c(1,3,2,4,5)])
fac06[[24]]<-factor(fac06[[24]],levels(fac06[[24]])[c(2,1,3)])
fac06[[25]]<-factor(fac06[[25]],levels(fac06[[25]])[c(2,3,4,1)])
fac06[[27]]<-factor(fac06[[27]],levels(fac06[[27]])[c(3,2,1,5,4)])
fac06[[56]]<-factor(fac06[[56]],trimws(levels(fac06[[56]])))
fac06[[68]]<-factor(fac06[[68]],levels(fac06[[68]])[c(1,3,2)])
fac06[[69]]<-factor(fac06[[69]],levels(fac06[[69]])[c(4,2,3,1,5)])
fac06[[228]]<-factor(fac06[[228]],levels(fac06[[228]])[c(4,2,1,3,5)])
fac06[[231]]<-factor(fac06[[231]],trimws(levels(fac06[[231]])))
fac06[[237]]<-factor(fac06[[237]],levels(fac06[[237]])[c(1,3,2)])
fac06[[238]]<-factor(fac06[[238]],levels(fac06[[238]])[c(1,3,2)])
fac06[[239]]<-factor(fac06[[239]],levels(fac06[[239]])[c(1,3,2)])

#### reorder fac08 ####
fac08_levels<-lapply(1:length(fac08), function(x) levels(fac08[[x]]))
# for(x in 1:length(fac08)){
#   if(length(fac08_levels[[x]])>=3 & length(fac08_levels[[x]])<=7){
#     print(x)
#     print(fac08_levels[x])
#   }
# }

fac08[[4]]<-factor(fac08[[4]],levels(fac08[[4]])[c(1,3,2,4,5)])
fac08[[7]]<-factor(fac08[[7]],levels(fac08[[7]])[c(1,2,4,3)])
fac08[[47]]<-factor(fac08[[47]],trimws(levels(fac08[[47]])[c(3,1,2,4)]))
fac08[[48]]<-factor(fac08[[48]],trimws(levels(fac08[[48]])[c(1,3,2)]))
fac08[[49]]<-factor(fac08[[49]],levels(fac08[[49]])[c(2,1,3)])
fac08[[50]]<-factor(fac08[[50]],levels(fac08[[50]])[c(2,3,4,1)])
fac08[[52]]<-factor(fac08[[52]],levels(fac08[[52]])[c(3,2,1,5,4)])
fac08[[84]]<-factor(fac08[[84]],levels(fac08[[84]])[c(5,2,3,4,6,1)])
fac08[[85]]<-factor(fac08[[85]],levels(fac08[[85]])[c(5,2,3,4,6,1)])
fac08[[86]]<-factor(fac08[[86]],levels(fac08[[86]])[c(5,2,3,4,6,1)])
fac08[[87]]<-factor(fac08[[87]],levels(fac08[[87]])[c(5,2,3,4,6,1)])
fac08[[88]]<-factor(fac08[[88]],levels(fac08[[88]])[c(5,2,3,4,6,1)])
fac08[[89]]<-factor(fac08[[89]],levels(fac08[[89]])[c(5,2,3,4,6,1)])
fac08[[90]]<-factor(fac08[[90]],levels(fac08[[90]])[c(5,2,3,4,6,1)])
fac08[[92]]<-factor(fac08[[92]],levels(fac08[[92]])[c(5,2,3,4,6,1)])
fac08[[93]]<-factor(fac08[[93]],levels(fac08[[93]])[c(5,2,3,4,6,1)])
fac08[[94]]<-factor(fac08[[94]],levels(fac08[[94]])[c(5,2,3,4,6,1)])
fac08[[95]]<-factor(fac08[[95]],levels(fac08[[95]])[c(5,2,3,4,6,1)])
fac08[[97]]<-factor(fac08[[97]],levels(fac08[[97]])[c(5,2,3,4,1)])
fac08[[98]]<-factor(fac08[[98]],levels(fac08[[98]])[c(5,2,3,4,1)])
fac08[[100]]<-factor(fac08[[100]],levels(fac08[[100]])[c(2,3,4,5,1)])
fac08[[112]]<-factor(fac08[[112]],levels(fac08[[112]])[c(1,3,2)])
fac08[[113]]<-factor(fac08[[113]],levels(fac08[[113]])[c(4,2,3,1,5)])
fac08[[116]]<-factor(fac08[[116]],trimws(levels(fac08[[116]])[c(3,2,4,1)]))
fac08[[117]]<-factor(fac08[[117]],trimws(levels(fac08[[117]])[c(1,3,2)]))
fac08[[118]]<-factor(fac08[[118]],trimws(levels(fac08[[118]])[c(2,1,5,4,3)]))
fac08[[263]]<-factor(fac08[[263]],trimws(levels(fac08[[263]])[c(4,2,1,3,5)]))


#### reorder fac11 ####
fac11_levels<-lapply(1:length(fac11), function(x) levels(fac11[[x]]))
# for(x in 1:length(fac11)){
#   if(length(fac11_levels[[x]])>=3 & length(fac11_levels[[x]])<=7){
#     print(x)
#     print(fac11_levels[x])
#   }
# }

fac11[[4]]<-factor(fac11[[4]],levels(fac11[[4]])[c(1,3,2,4,5)])
fac11[[7]]<-factor(fac11[[7]],levels(fac11[[7]])[c(1,2,4,3)])
fac11[[8]]<-factor(fac11[[8]],levels(fac11[[8]])[c(1,3,2,4)])
fac11[[78]]<-factor(fac11[[78]],levels(fac11[[78]])[c(1,5,2,3,4,6)])
fac11[[79]]<-factor(fac11[[79]],levels(fac11[[79]])[c(1,5,2,3,4,6)])
fac11[[80]]<-factor(fac11[[80]],levels(fac11[[80]])[c(1,5,2,3,4,6)])
fac11[[81]]<-factor(fac11[[81]],levels(fac11[[81]])[c(1,5,2,3,4,6)]) # percentage 6-10
fac11[[118]]<-factor(fac11[[118]],levels(fac11[[18]])[c(4,1,3,2,5)])
fac11[[140]]<-factor(fac11[[140]],levels(fac11[[140]])[c(2,1,3)])
fac11[[141]]<-factor(fac11[[141]],levels(fac11[[141]])[c(4,2,1,3)])
fac11[[142]]<-factor(fac11[[142]],levels(fac11[[142]])[c(1,3,2)])
fac11[[143]]<-factor(fac11[[143]],levels(fac11[[143]])[c(4,2,1,3)])
fac11[[144]]<-factor(fac11[[144]],levels(fac11[[144]])[c(4,2,1,3)])
fac11[[157]]<-factor(fac11[[157]],levels(fac11[[157]])[c(4,1,3,2,5)])
fac11[[174]]<-factor(fac11[[174]],levels(fac11[[174]])[c(4,2,1,3,5)])
fac11[[177]]<-factor(fac11[[177]],levels(fac11[[177]])[c(5,1,4,3,6,2)])
fac11[[179]]<-factor(fac11[[179]],trimws(levels(fac11[[179]])[c(2,4,5,1,3)]))
fac11[[180]]<-factor(fac11[[180]],trimws(levels(fac11[[180]])[c(3,2,4,1)]))
fac11[[181]]<-factor(fac11[[181]],trimws(levels(fac11[[181]])[c(2,3,1)]))
fac11[[230]]<-factor(fac11[[230]],trimws(levels(fac11[[230]])[c(1,3,2)]))
fac11[[246]]<-factor(fac11[[246]],trimws(levels(fac11[[246]])[c(4,2,1,3,5,6)]))
fac11[[247]]<-factor(fac11[[247]],trimws(levels(fac11[[247]])[c(4,2,1,3,5,6)]))
fac11[[248]]<-factor(fac11[[248]],trimws(levels(fac11[[248]])[c(5,3,1,2,4,6)]))
fac11[[249]]<-factor(fac11[[249]],trimws(levels(fac11[[249]])[c(4,2,3,1,5)]))
fac11[[250]]<-factor(fac11[[250]],trimws(levels(fac11[[250]])[c(2,3,4,1)]))
fac11[[251]]<-factor(fac11[[251]],trimws(levels(fac11[[251]])))
fac11[[252]]<-factor(fac11[[252]],trimws(levels(fac11[[252]])[c(3,1,2,4)]))
fac11[[253]]<-factor(fac11[[253]],trimws(levels(fac11[[253]])))
fac11[[255]]<-factor(fac11[[255]],trimws(levels(fac11[[255]])[c(1,3,2)]))
fac11[[284]]<-factor(fac11[[284]],trimws(levels(fac11[[284]])))
fac11[[285]]<-factor(fac11[[285]],trimws(levels(fac11[[285]])))
fac11[[286]]<-factor(fac11[[286]],trimws(levels(fac11[[286]])))
fac11[[310]]<-factor(fac11[[310]],levels(fac11[[310]])[c(1,2,6,3,4,5,7)])
fac11[[321]]<-factor(fac11[[321]],levels(fac11[[321]])[c(1,2,6,3,4,5,7)])
fac11[[322]]<-factor(fac11[[322]],levels(fac11[[322]])[c(1,2,6,3,4,5,7)])
fac11[[323]]<-factor(fac11[[323]],levels(fac11[[323]])[c(1,2,6,3,4,5,7)])
fac11[[324]]<-factor(fac11[[324]],levels(fac11[[324]])[c(1,2,6,3,4,5)])
fac11[[325]]<-factor(fac11[[325]],levels(fac11[[325]])[c(1,2,6,3,4,5,7)])
fac11[[412]]<-factor(fac11[[412]],levels(fac11[[412]])[c(1,3,2)])
fac11[[413]]<-factor(fac11[[413]],levels(fac11[[413]])[c(1,3,2)])
fac11[[414]]<-factor(fac11[[414]],levels(fac11[[414]])[c(1,3,2)])
fac11[[415]]<-factor(fac11[[415]],levels(fac11[[415]])[c(1,3,2)])
fac11[[416]]<-factor(fac11[[416]],levels(fac11[[416]])[c(1,3,2)])
fac11[[417]]<-factor(fac11[[417]],levels(fac11[[417]])[c(1,3,2)])
fac11[[424]]<-factor(fac11[[424]],levels(fac11[[424]])[c(3,1,2)])
fac11[[425]]<-factor(fac11[[425]],levels(fac11[[425]])[c(3,1,2)])
fac11[[426]]<-factor(fac11[[426]],levels(fac11[[426]])[c(3,1,2)])
fac11[[427]]<-factor(fac11[[427]],levels(fac11[[427]])[c(3,1,2)])
fac11[[428]]<-factor(fac11[[428]],levels(fac11[[428]])[c(3,1,2)])
fac11[[429]]<-factor(fac11[[429]],levels(fac11[[429]])[c(3,1,2)])
fac11[[430]]<-factor(fac11[[430]],levels(fac11[[424]])[c(2,1,3)])
fac11[[431]]<-factor(fac11[[431]],levels(fac11[[431]])[c(2,1,3)])
fac11[[432]]<-factor(fac11[[432]],levels(fac11[[432]])[c(2,1,3)])
fac11[[433]]<-factor(fac11[[433]],levels(fac11[[433]])[c(2,1,3)])
fac11[[434]]<-factor(fac11[[434]],levels(fac11[[434]])[c(2,1,3)])
fac11[[445]]<-factor(fac11[[445]],levels(fac11[[445]])[c(1,3,2)])
fac11[[446]]<-factor(fac11[[446]],levels(fac11[[446]])[c(1,3,2)])
fac11[[447]]<-factor(fac11[[447]],levels(fac11[[447]])[c(1,3,2)])
fac11[[448]]<-factor(fac11[[448]],levels(fac11[[448]])[c(1,3,2)])
fac11[[449]]<-factor(fac11[[449]],levels(fac11[[449]])[c(1,3,2)])
fac11[[450]]<-factor(fac11[[450]],levels(fac11[[450]])[c(1,3,2)])
fac11[[463]]<-factor(fac11[[463]],levels(fac11[[463]])[c(1,2,6,3,4,5,7)])
fac11[[464]]<-factor(fac11[[464]],levels(fac11[[464]])[c(1,2,6,3,4,5,7)])
fac11[[465]]<-factor(fac11[[465]],levels(fac11[[465]])[c(1,2,6,3,4,5,7)])
fac11[[466]]<-factor(fac11[[466]],levels(fac11[[466]])[c(1,2,6,3,4,5,7)])
fac11[[467]]<-factor(fac11[[467]],levels(fac11[[467]])[c(1,2,6,3,4,5,7)])
fac11[[468]]<-factor(fac11[[468]],levels(fac11[[468]])[c(1,2,6,3,4,5,7)])
fac11[[469]]<-factor(fac11[[469]],levels(fac11[[469]])[c(1,2,6,3,4,5,7)])
fac11[[470]]<-factor(fac11[[470]],levels(fac11[[470]])[c(1,2,6,3,4,5,7)])
fac11[[471]]<-factor(fac11[[471]],levels(fac11[[471]])[c(5,2,3,4,6,1)])
fac11[[472]]<-factor(fac11[[472]],levels(fac11[[472]])[c(5,2,3,4,6,1)])
fac11[[473]]<-factor(fac11[[473]],levels(fac11[[473]])[c(5,2,3,4,6,1)])
fac11[[474]]<-factor(fac11[[474]],levels(fac11[[474]])[c(5,2,3,4,6,1)])
fac11[[475]]<-factor(fac11[[475]],levels(fac11[[475]])[c(5,2,3,4,6,1)])
fac11[[476]]<-factor(fac11[[476]],levels(fac11[[476]])[c(5,2,3,4,6,1)])
fac11[[477]]<-factor(fac11[[477]],levels(fac11[[477]])[c(5,2,3,4,6,1)])
fac11[[479]]<-factor(fac11[[479]],levels(fac11[[479]])[c(5,2,3,4,6,1)])
fac11[[480]]<-factor(fac11[[480]],levels(fac11[[480]])[c(5,2,3,4,6,1)])
fac11[[481]]<-factor(fac11[[481]],levels(fac11[[481]])[c(5,2,3,4,6,1)])
fac11[[482]]<-factor(fac11[[482]],levels(fac11[[482]])[c(5,2,3,4,6,1)])
fac11[[483]]<-factor(fac11[[483]],levels(fac11[[483]])[c(3,2,4,1)])
fac11[[484]]<-factor(fac11[[484]],levels(fac11[[484]])[c(5,2,3,4,1)])
fac11[[485]]<-factor(fac11[[485]],levels(fac11[[485]])[c(5,2,3,4,1)])
fac11[[486]]<-factor(fac11[[486]],levels(fac11[[486]])[c(3,2,4,1)])
fac11[[487]]<-factor(fac11[[487]],levels(fac11[[487]])[c(2,3,4,5,1)])
fac11[[495]]<-factor(fac11[[495]],levels(fac11[[495]])[c(3,4,2,5,1)])
fac11[[496]]<-factor(fac11[[496]],levels(fac11[[496]])[c(3,4,2,5,1)])
fac11[[497]]<-factor(fac11[[497]],levels(fac11[[497]])[c(3,4,2,5,1)])
fac11[[498]]<-factor(fac11[[498]],levels(fac11[[498]])[c(3,4,2,5,1)])
fac11[[499]]<-factor(fac11[[499]],levels(fac11[[499]])[c(3,4,2,5,1)])
fac11[[500]]<-factor(fac11[[500]],levels(fac11[[500]])[c(3,4,2,5,1)])
fac11[[501]]<-factor(fac11[[501]],levels(fac11[[501]])[c(3,4,2,5,1)])
fac11[[548]]<-factor(fac11[[548]],levels(fac11[[548]])[c(5,3,6,2,4,1)])
levels(fac11[[572]])[1]<-levels(fac11[[572]])[2]
fac11[[574]]<-factor(fac11[[574]],levels(fac11[[574]])[c(5,1,4,3,6,2)])
fac11[[575]]<-factor(fac11[[575]],levels(fac11[[575]])[c(1,3,2)])
fac11[[576]]<-factor(fac11[[576]],levels(fac11[[576]])[c(4,2,3,1,5)])
levels(fac11[[694]])[2]<-"Levels of analysis"
fac11[[709]]<-factor(fac11[[709]],levels(fac11[[709]])[c(4,2,1,3,5)])
fac11[[710]]<-factor(fac11[[710]],levels(fac11[[710]])[c(1,5,2,3,4,6)])
fac11[[711]]<-factor(fac11[[711]],levels(fac11[[711]])[c(1,5,2,3,4,6)])
fac11[[712]]<-factor(fac11[[712]],levels(fac11[[712]])[c(1,5,2,3,4,6)])
fac11[[713]]<-factor(fac11[[713]],levels(fac11[[713]])[c(1,5,2,3,4,6)])
fac11[[714]]<-factor(fac11[[714]],levels(fac11[[714]])[c(1,5,2,3,4,6)])
fac11[[715]]<-factor(fac11[[715]],levels(fac11[[715]])[c(1,5,2,3,4,6)])
fac11[[716]]<-factor(fac11[[716]],levels(fac11[[716]])[c(1,5,2,3,4,6)])
fac11[[717]]<-factor(fac11[[717]],levels(fac11[[717]])[c(1,5,2,3,4,6)])
fac11[[718]]<-factor(fac11[[718]],levels(fac11[[718]])[c(1,5,2,3,4,6)])
fac11[[719]]<-factor(fac11[[719]],levels(fac11[[719]])[c(1,5,2,3,4,6)])
fac11[[720]]<-factor(fac11[[720]],levels(fac11[[720]])[c(1,5,2,3,4,6)])


#### reorder fac11 ####
fac14_levels<-lapply(1:length(fac14), function(x) levels(fac14[[x]]))
# for(x in 1:length(fac14)){
#   if(length(fac14_levels[[x]])>=3 & length(fac14_levels[[x]])<=7){
#     print(x)
#     print(fac14_levels[x])
#   }
# }

fac14[[3]]<-factor(fac14[[3]],levels(fac14[[3]])[c(1,2,4,3)])
fac14[[4]]<-factor(fac14[[4]],levels(fac14[[4]])[c(1,3,2,4,5)])
fac14[[15]]<-factor(fac14[[15]],levels(fac14[[15]])[c(1,2,4,3)])
fac14[[16]]<-factor(fac14[[16]],levels(fac14[[16]])[c(1,3,2,4)])
fac14[[85]]<-factor(fac14[[85]],levels(fac14[[85]])[c(1,3,2)])
fac14[[86]]<-factor(fac14[[86]],levels(fac14[[86]])[c(4,1,3,2,5)])
fac14[[112]]<-factor(fac14[[112]],levels(fac14[[112]])[c(5,2,4,3,1)])
fac14[[113]]<-factor(fac14[[113]],levels(fac14[[113]])[c(4,1,3,2)])
fac14[[152]]<-factor(fac14[[152]],levels(fac14[[152]])[c(1,4,3,2)])
fac14[[153]]<-factor(fac14[[153]],levels(fac14[[153]])[c(4,2,1,3,5)])
fac14[[156]]<-factor(fac14[[156]],levels(fac14[[156]])[c(2,1,3,4)])
fac14[[169]]<-factor(fac14[[169]],levels(fac14[[169]])[c(3,1,4,2)])
fac14[[170]]<-factor(fac14[[170]],levels(fac14[[170]])[c(3,1,4,2)])
fac14[[171]]<-factor(fac14[[171]],levels(fac14[[171]])[c(3,1,4,2)])
fac14[[172]]<-factor(fac14[[172]],levels(fac14[[172]])[c(3,1,4,2)])
fac14[[173]]<-factor(fac14[[173]],levels(fac14[[173]])[c(3,1,4,2)])
fac14[[174]]<-factor(fac14[[174]],levels(fac14[[174]])[c(3,1,4,2)])
fac14[[390]]<-factor(fac14[[390]],levels(fac14[[390]])[c(4,2,1,3,5)])
fac14[[391]]<-factor(fac14[[391]],levels(fac14[[391]])[c(4,2,1,3,5)])
fac14[[404]]<-factor(fac14[[404]],levels(fac14[[404]])[c(1,2,4,5,6,3)])
fac14[[424]]<-factor(fac14[[424]],levels(fac14[[424]])[c(5,3,2,1,4)])
fac14[[441]]<-factor(fac14[[441]],levels(fac14[[441]])[c(1,2,6,3,4,5,7)])
fac14[[442]]<-factor(fac14[[442]],levels(fac14[[442]])[c(1,2,6,3,4,5,7)])
fac14[[443]]<-factor(fac14[[443]],levels(fac14[[443]])[c(1,5,2,3,4,6)])



#### reorder fac11 ####
fac17_levels<-lapply(1:length(fac17), function(x) levels(fac17[[x]]))
# for(x in 1:length(fac17)){
#   if(length(fac17_levels[[x]])>=3 & length(fac17_levels[[x]])<=7){
#     print(x)
#     print(fac17_levels[x])
#   }
# }

fac17[[4]]<-factor(fac17[[4]],levels(fac17[[4]])[c(1,3,2,4,5)])
fac17[[6]]<-factor(fac17[[6]],levels(fac17[[6]])[c(1,3,4,5,6,2)])
fac17[[7]]<-factor(fac17[[7]],levels(fac17[[7]])[c(4,2,1,3,5)])
fac17[[36]]<-factor(fac17[[36]],levels(fac17[[36]])[c(1,3,4,2)])
fac17[[37]]<-factor(fac17[[37]],levels(fac17[[37]])[c(4,2,1,3,5)])
fac17[[61]]<-factor(fac17[[61]],levels(fac17[[61]])[c(4,2,1,3,5)])
fac17[[62]]<-factor(fac17[[62]],levels(fac17[[62]])[c(1,2,4,5,6,3)])










surveys<-list("FS2004"=fac04,"FS2006"=fac06,"FS2008"=fac08,"FS2011"=fac11,"FS2014"=fac14,"FS2017"=fac17)

#lapply(1:6, function(i) write.csv(new_surveys[[i]], file = paste0(names(new_surveys[i]), ".csv"), row.names = FALSE))




















