
#symbol experiment (IEC) - 01 


library(ggplot2)
library(psych)
library(stargazer)
library(car)
library(Hmisc)
library(xtable)
library(lubridate)
library(multilevel)
library(dplyr)

#Data arrangement#######

                                    
IEC_raw <- read.csv("C:/Users/mr.saar/Google Drive/R/IEC/experiment 12-2015/11_2016/IECdata.csv")

IEC_raw = IEC_raw %>% 
  filter(V10==1) #exclude incomplete questionnaires

TIMER <- read.csv("C:/Users/mr.saar/Google Drive/R/IEC/experiment 12-2015/11_2016/timer calculation.csv")

IEC_00 = IEC_raw %>%
  select(V4,V5)

IEC_00 =   
IEC_00 %>%
  mutate(ID = c(1:1313),
         IP = IEC_raw$V6,
         START_DATE = IEC_raw$V8,
         END_DATE = IEC_raw$V9,
         UID = IEC_raw$uid,
         TIMER_total = TIMER$HOURS*60 + TIMER$MINUTES) %>%  #total time of filling survey in minutes 
  select(-c(V4,V5))



#Variables####

       
IEC_00 =   
  IEC_00 %>%

#TREATMENT (0=control; 1=placebo; 2=treatment)
  mutate(SYMBOL = ifelse(IEC_raw$DO.BR.FL_4=='Figures (TREATMENT)',2,
                         ifelse(IEC_raw$DO.BR.FL_4=='Figures (PLACEBO)',1,0))) %>%

##OUTCOME VARIABLES: attitudes about IEC
  mutate(EVALUATION_01 = IEC_raw$Q34_4, # "How would you evaluate IEC's treatment of power interruptions?" (1-10)
         EVALUATION_02 = IEC_raw$Q35_4, # "How would you evaluate the quality of the services of the IEC?" (1-10) 
         SATISFACTION = IEC_raw$Q36_4, # "How would you rate your satisfaction of the services of the IEC?" (1-10)
         TRUST = IEC_raw$Q37_4)  # "to what extent to you have confidence in the IEC? (1-10)"

IEC_00 = IEC_00 %>%
mutate(EVALUATION_index = (EVALUATION_01 + EVALUATION_02)/2) #complex  index

#MODERATOR: performance (power supply reliability) 
IEC_00 = IEC_00 %>%
  mutate(OUTAGE_q01 = IEC_raw$Q42,
         OUTAGE_q02 = ifelse(IEC_raw$Q43==6,0,IEC_raw$Q43),
         OUTAGE_q03 = IEC_raw$Q44) %>%
  mutate(OUTAGE_q02 = Recode(OUTAGE_q02,"NA=0")) %>%
  mutate(OUTAGE_q02.1 = OUTAGE_q02,
         OUTAGE_q01.1 = Recode(OUTAGE_q01,"3=1;1=0;2=0;NA=0"))%>%
  mutate(OUTAGE_q02.1 = Recode(OUTAGE_q02,"3=2;4=3;5=3;NA=0"),
         OUTAGE_q03.1 = OUTAGE_q03-1) %>%
  mutate(OUTAGE_q01.1 = ifelse(OUTAGE_q02.1==0,0,OUTAGE_q01.1))

IEC_00 = IEC_00 %>%
  mutate(OUTAGE_01 = ifelse(OUTAGE_q01==1 | OUTAGE_q02==0,0,1)) %>% # 0: no outage; 1: outage
  mutate(OUTAGE_02 = ifelse(OUTAGE_01==0 | OUTAGE_q02.1==0,0,ifelse(OUTAGE_q02==1,0.5,1)))%>% # 0: no outage; 0.5: outage less than 6 hours; 1: outage more than 6 hours
  mutate(OUTAGE_03 = (OUTAGE_q02.1+OUTAGE_q01.1+OUTAGE_q03.1)/6) #outage experience index

IEC_00 = IEC_00 %>%
  mutate(OUTAGE_long = ifelse(OUTAGE_q02>2,1,0))  #long outage: 1=more than 12 hours

#City
IEC_00$CITY_text = IEC_raw$Q58
IEC_00$CITY = IEC_00$CITY_text


IEC_00$CITY = ifelse(IEC_00$CITY_text %in% c(" מודיעין",
                                             "Modiin",
                                             "מודיעיו",
                                             "מודיעין-מכבים-רעות",
                                             "מודיעין ",
                                             "מודיעין מכבים רעות",
                                             "מודיעין מכבים",
                                             "מודיעין עילית",
                                             "מודיעין",
                                             "מכבים רעות",
                                             "מכבים",
                                             "רעות"),"MODIIN",
                     ifelse(IEC_00$CITY_text %in% c(" נס ציונה",
                                                    "נס ציונה ",
                                                    "נס ציונה",
                                                    "נס צינה"),"NES TZIONA",
                            ifelse(IEC_00$CITY_text %in% c("ramat gan",
                                                           "רג",
                                                           "רמת-גן",
                                                           "רמת  גן",
                                                           "רמת גו",
                                                           "רמת גן ",
                                                           "רמת גן",
                                                           "רמתגן"),"RAMAT GAN",
                                   ifelse(IEC_00$CITY_text %in% c("rishon lezion",
                                                                  "ראשון לציון ",
                                                                  "ראשון לציון",
                                                                  "ראשון",
                                                                  "ראשלצ",
                                                                  "רישון לציון"),"RISHON LEZION",       
                                          ifelse(IEC_00$CITY_text %in% c("herzeliya",
                                                                         "herzlia",
                                                                         "האצליה",
                                                                         "הרצל",
                                                                         "הרצליה"),"HERZELIA",
                                                 ifelse(IEC_00$CITY_text %in% c("הוד-השרון",
                                                                                "הוד השרון",
                                                                                "הודהשרון "),"HOD HASHARON",
                                                        ifelse(IEC_00$CITY_text %in% c("kfar saba",
                                                                                       "כס",
                                                                                       "כפס",
                                                                                       "כפר סבא"),"KFAR SABA",
                                                               ifelse(IEC_00$CITY_text %in% c("רעננה ",
                                                                                              "רעננה"),"RAANANA",
                                                                      ifelse(IEC_00$CITY_text %in% c("rn, varui",
                                                                                                     "רמהש",
                                                                                                     "רמת השרון"),"RAMAT HASHARON",
                                                                             ifelse(IEC_00$CITY_text %in% c(" באר שבע",
                                                                                                            "באר שבע"),"BEER SHEVA",
                                                                                    ifelse(IEC_00$CITY_text %in% c("באר יעקב"),"BEER YAAKOV",
                                                                                           ifelse(IEC_00$CITY_text %in% c("gvatim",
                                                                                                                          "גבעתיים"),"GIVATAIM",
                                                                                                  ifelse(IEC_00$CITY_text %in% c("חיפה"),"HAIFA",
                                                                                                         ifelse(IEC_00$CITY_text %in% c("חולון"),"HULON",
                                                                                                                ifelse(IEC_00$CITY_text %in% c("ירושלים"),"JERUSALEM",
                                                                                                                       ifelse(IEC_00$CITY_text %in% c("נתניה"),"NATANIA",
                                                                                                                              ifelse(IEC_00$CITY_text %in% c("פרדס חנה"),"PARDES HANA",
                                                                                                                                     ifelse(IEC_00$CITY_text %in% c("פתח-תקוה",
                                                                                                                                                                    "פתח תקוה",
                                                                                                                                                                    "פתח תקווה"),"PETACH TIKVA",
                                                                                                                                            ifelse(IEC_00$CITY_text %in% c("יפו",
                                                                                                                                                                           "תא",
                                                                                                                                                                           "תל אביב ",
                                                                                                                                                                           "תל אביב",
                                                                                                                                                                           "תל אביייב"),"TEL AVIV" ,"OTHER")))))))))))))))))))       


IEC_00$CITY[IEC_raw$uid==5346719] = "RAMAT GAN"
IEC_00$CITY[IEC_raw$uid %in% c(5341682,	5342253,	5341563,	5342094,
                                  5341566,	5342526,	5345082,	5344664)] = "RISHON LEZION"
IEC_00$CITY[IEC_raw$uid %in% c(5346951,
                                  5330528)] = "RAMAT HASHARON"

#Short city names
IEC_00 =  IEC_00 %>%
  mutate(CITY_short = Recode(CITY,"'HERZELIA'='HZL';
                             'HOD HASHARON'='HDS';
                             'KFAR SABA'='KFS';
                             'RAANANA'='RNA';
                             'RAMAT HASHARON'='RSR';
                             'RAMAT GAN'='RGN';
                             'RISHON LEZION'='RLZ';
                             'MODIIN'='MDN';
                             'NES TZIONA'='NTZ';else='OTHER'")) 

#Areas
IEC_00 = IEC_00 %>%
  mutate(AREA = Recode(CITY_short,"c('HDS','HZL','KFS','RNA','RSR')='SHARON';
                                  c('MDN','RGN','NTZ','RLZ')='SHFELA'"))

IEC_00 = IEC_00 %>%
  mutate(AREA_01 = as.numeric(Recode(AREA,"'SHARON'=0;'SHFELA'=1"))) # sharon=0

##CONTROLS
IEC_00 = IEC_00 %>%
#Attitudes vis-a-vis government  
  mutate(GOV_EVALUATION = (IEC_raw$Q9-1)/6, # "How would you evaluate the functioning of the public sector in Israel?" (1-7)
  GOV_SATISFACTION = (IEC_raw$Q10-1)/6, # "To what extent are you satisfied with the quality of services of the public sector in Israel?" (1-7)
  GOV_TRUST = (IEC_raw$Q11-1)/6) %>% # "To what extent do you have confidence in the public sector in Israel" (1-7)
  mutate(GOV_ATTITUDES = ( GOV_EVALUATION +  GOV_SATISFACTION +  GOV_TRUST)/3, #Index

  INTEREST = Recode(IEC_raw$Q59,"1=1;2=0.75;3=0.5;4=0.25;5=0"), # "To what extent are you interested in affairs of society and economy?" --> reversed
  GENDER = Recode(IEC_raw$Q62,"1=0;2=1;3=NA"),# 0: male; 1:female
  AGE = IEC_raw$Q53_1,
  EDUCATION = (IEC_raw$Q54-1)/7, # "What is the highest level of education you have successfully completed?"
  INCOME = (IEC_raw$Q55-1)/4, # "According to Israel's Central Bureau of Statistics the monthly average gross income per household is approximately 18,000 NIS. Is your income"
  HOUSE = Recode(IEC_raw$Q56,"2=0"), # Do you own a house? 0: NO; 1: Yes
  IDEOLOGY = (IEC_raw$Q57_4)/10) # "How would you rate your ideological perception on a socio-economic scale, where 0 means social-democratic approach, 10 means capitalistic approach, and 5 is the middle"


IEC_00 = IEC_00 %>%
  mutate(ROADS_EVALUATION = IEC_raw$Q31_4,
         ROADS_SATISFACTION = IEC_raw$Q32_4,
         GAS_EVALUATION = IEC_raw$Q39_4,
         GAS_SATISFACTION = IEC_raw$Q40_4)


##random order of attitudes questions
IEC_00$IEC_ATTITUDES_order = factor(IEC_raw$DO.BL.Specificfacilities.OUTCOME.)

IEC_00$IEC_ATTITUDES_first[IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q34|Q35|Q36|Q37|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q34|Q35|Q37|Q36|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q34|Q36|Q35|Q37|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q34|Q36|Q37|Q35|Q30|Q31|Q32|Q38|Q39|Q40"|
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q34|Q37|Q35|Q36|Q30|Q31|Q32|Q38|Q39|Q40"|
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q34|Q37|Q36|Q35|Q30|Q31|Q32|Q38|Q39|Q40"]="EVALUATION_01"
IEC_00$IEC_ATTITUDES_first[IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q35|Q34|Q36|Q37|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q35|Q34|Q37|Q36|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q35|Q36|Q34|Q37|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q35|Q36|Q37|Q34|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q35|Q37|Q34|Q36|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q35|Q37|Q36|Q34|Q30|Q31|Q32|Q38|Q39|Q40"] = "EVALUATION_02"
IEC_00$IEC_ATTITUDES_first[IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q36|Q34|Q35|Q37|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q36|Q34|Q37|Q35|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q36|Q35|Q34|Q37|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q36|Q35|Q37|Q34|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q36|Q37|Q34|Q35|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q36|Q37|Q35|Q34|Q30|Q31|Q32|Q38|Q39|Q40"] = "SATISFACTION"
IEC_00$IEC_ATTITUDES_first[IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q37|Q34|Q35|Q36|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q37|Q34|Q36|Q35|Q30|Q31|Q32|Q38|Q39|Q40" |
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q37|Q35|Q34|Q36|Q30|Q31|Q32|Q38|Q39|Q40" |
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q37|Q35|Q36|Q34|Q30|Q31|Q32|Q38|Q39|Q40" |
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q37|Q36|Q34|Q35|Q30|Q31|Q32|Q38|Q39|Q40" |
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q37|Q36|Q35|Q34|Q30|Q31|Q32|Q38|Q39|Q40"] = "TRUST"

IEC_00$IEC_ATTITUDES_second[IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q35|Q34|Q36|Q37|Q30|Q31|Q32|Q38|Q39|Q40"| 
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q35|Q34|Q37|Q36|Q30|Q31|Q32|Q38|Q39|Q40"| 
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q36|Q34|Q35|Q37|Q30|Q31|Q32|Q38|Q39|Q40"| 
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q36|Q34|Q37|Q35|Q30|Q31|Q32|Q38|Q39|Q40"|    
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q37|Q34|Q35|Q36|Q30|Q31|Q32|Q38|Q39|Q40"| 
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q37|Q34|Q36|Q35|Q30|Q31|Q32|Q38|Q39|Q40"] = "EVALUATION_01"
IEC_00$IEC_ATTITUDES_second[IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q34|Q35|Q36|Q37|Q30|Q31|Q32|Q38|Q39|Q40"| 
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q34|Q35|Q37|Q36|Q30|Q31|Q32|Q38|Q39|Q40"|
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q36|Q35|Q34|Q37|Q30|Q31|Q32|Q38|Q39|Q40"| 
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q36|Q35|Q37|Q34|Q30|Q31|Q32|Q38|Q39|Q40"| 
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q37|Q35|Q34|Q36|Q30|Q31|Q32|Q38|Q39|Q40" |
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q37|Q35|Q36|Q34|Q30|Q31|Q32|Q38|Q39|Q40"] = "EVALUATION_02"
IEC_00$IEC_ATTITUDES_second[IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q34|Q36|Q35|Q37|Q30|Q31|Q32|Q38|Q39|Q40"| 
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q34|Q36|Q37|Q35|Q30|Q31|Q32|Q38|Q39|Q40"|
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q35|Q36|Q34|Q37|Q30|Q31|Q32|Q38|Q39|Q40"| 
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q35|Q36|Q37|Q34|Q30|Q31|Q32|Q38|Q39|Q40"| 
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q37|Q36|Q34|Q35|Q30|Q31|Q32|Q38|Q39|Q40" |
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q37|Q36|Q35|Q34|Q30|Q31|Q32|Q38|Q39|Q40"] = "SATISFACTION"
IEC_00$IEC_ATTITUDES_second[IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q34|Q37|Q35|Q36|Q30|Q31|Q32|Q38|Q39|Q40"|
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q34|Q37|Q36|Q35|Q30|Q31|Q32|Q38|Q39|Q40"|
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q35|Q37|Q34|Q36|Q30|Q31|Q32|Q38|Q39|Q40"| 
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q35|Q37|Q36|Q34|Q30|Q31|Q32|Q38|Q39|Q40"| 
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q36|Q37|Q34|Q35|Q30|Q31|Q32|Q38|Q39|Q40"| 
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q36|Q37|Q35|Q34|Q30|Q31|Q32|Q38|Q39|Q40"] = "TRUST"

IEC_00$IEC_ATTITUDES_third[IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q35|Q36|Q34|Q37|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q35|Q37|Q34|Q36|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q36|Q35|Q34|Q37|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q36|Q37|Q34|Q35|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q37|Q35|Q34|Q36|Q30|Q31|Q32|Q38|Q39|Q40" |
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q37|Q36|Q34|Q35|Q30|Q31|Q32|Q38|Q39|Q40"] = "EVALUATION_01"
IEC_00$IEC_ATTITUDES_third[IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q34|Q36|Q35|Q37|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q34|Q37|Q35|Q36|Q30|Q31|Q32|Q38|Q39|Q40"|
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q36|Q34|Q35|Q37|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q36|Q37|Q35|Q34|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q37|Q34|Q35|Q36|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q37|Q36|Q35|Q34|Q30|Q31|Q32|Q38|Q39|Q40"] = "EVALUATION_02"
IEC_00$IEC_ATTITUDES_third[IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q34|Q35|Q36|Q37|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q34|Q37|Q36|Q35|Q30|Q31|Q32|Q38|Q39|Q40"|
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q35|Q34|Q36|Q37|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q35|Q37|Q36|Q34|Q30|Q31|Q32|Q38|Q39|Q40"|  
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q37|Q34|Q36|Q35|Q30|Q31|Q32|Q38|Q39|Q40"|
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q37|Q35|Q36|Q34|Q30|Q31|Q32|Q38|Q39|Q40"] = "SATISFACTION"
IEC_00$IEC_ATTITUDES_third[IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q34|Q35|Q37|Q36|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q34|Q36|Q37|Q35|Q30|Q31|Q32|Q38|Q39|Q40"|
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q35|Q34|Q37|Q36|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q35|Q36|Q37|Q34|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q36|Q34|Q37|Q35|Q30|Q31|Q32|Q38|Q39|Q40"| 
                             IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q36|Q35|Q37|Q34|Q30|Q31|Q32|Q38|Q39|Q40"] = "TRUST" 

IEC_00$IEC_ATTITUDES_fourth[IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q35|Q36|Q37|Q34|Q30|Q31|Q32|Q38|Q39|Q40"| 
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q35|Q37|Q36|Q34|Q30|Q31|Q32|Q38|Q39|Q40"|
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q36|Q35|Q37|Q34|Q30|Q31|Q32|Q38|Q39|Q40"| 
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q36|Q37|Q35|Q34|Q30|Q31|Q32|Q38|Q39|Q40"|
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q37|Q35|Q36|Q34|Q30|Q31|Q32|Q38|Q39|Q40" |
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q37|Q36|Q35|Q34|Q30|Q31|Q32|Q38|Q39|Q40"] = "EVALUATION_01"
IEC_00$IEC_ATTITUDES_fourth[IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q34|Q36|Q37|Q35|Q30|Q31|Q32|Q38|Q39|Q40"|
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q34|Q37|Q36|Q35|Q30|Q31|Q32|Q38|Q39|Q40"|
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q36|Q34|Q37|Q35|Q30|Q31|Q32|Q38|Q39|Q40"| 
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q36|Q37|Q34|Q35|Q30|Q31|Q32|Q38|Q39|Q40"| 
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q37|Q34|Q36|Q35|Q30|Q31|Q32|Q38|Q39|Q40" |
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q37|Q36|Q34|Q35|Q30|Q31|Q32|Q38|Q39|Q40"] = "EVALUATION_02"
IEC_00$IEC_ATTITUDES_fourth[IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q34|Q35|Q37|Q36|Q30|Q31|Q32|Q38|Q39|Q40"| 
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q34|Q37|Q35|Q36|Q30|Q31|Q32|Q38|Q39|Q40"|
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q35|Q34|Q37|Q36|Q30|Q31|Q32|Q38|Q39|Q40"| 
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q35|Q37|Q34|Q36|Q30|Q31|Q32|Q38|Q39|Q40"| 
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q37|Q34|Q35|Q36|Q30|Q31|Q32|Q38|Q39|Q40"| 
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q37|Q35|Q34|Q36|Q30|Q31|Q32|Q38|Q39|Q40"] = "SATISFACTION"  
IEC_00$IEC_ATTITUDES_fourth[IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q34|Q35|Q36|Q37|Q30|Q31|Q32|Q38|Q39|Q40"| 
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q34|Q36|Q35|Q37|Q30|Q31|Q32|Q38|Q39|Q40"|
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q35|Q34|Q36|Q37|Q30|Q31|Q32|Q38|Q39|Q40"| 
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q35|Q36|Q34|Q37|Q30|Q31|Q32|Q38|Q39|Q40"|
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q36|Q34|Q35|Q37|Q30|Q31|Q32|Q38|Q39|Q40"| 
                              IEC_00$IEC_ATTITUDES_order=="Q28|Q29|Q33|Q36|Q35|Q34|Q37|Q30|Q31|Q32|Q38|Q39|Q40"] = "TRUST"  


IEC_00$EVALUATION_01_order = ifelse(IEC_00$IEC_ATTITUDES_first=="EVALUATION_01",1,
                                    ifelse(IEC_00$IEC_ATTITUDES_second=="EVALUATION_01",2,
                                           ifelse(IEC_00$IEC_ATTITUDES_third=="EVALUATION_01",3,4)))
IEC_00$EVALUATION_02_order = ifelse(IEC_00$IEC_ATTITUDES_first=="EVALUATION_02",1,
                                    ifelse(IEC_00$IEC_ATTITUDES_second=="EVALUATION_02",2,
                                           ifelse(IEC_00$IEC_ATTITUDES_third=="EVALUATION_02",3,4)))
IEC_00$SATISFACTION_order = ifelse(IEC_00$IEC_ATTITUDES_first=="SATISFACTION",1,
                                   ifelse(IEC_00$IEC_ATTITUDES_second=="SATISFACTION",2,
                                          ifelse(IEC_00$IEC_ATTITUDES_third=="SATISFACTION",3,4)))
IEC_00$TRUST_order = ifelse(IEC_00$IEC_ATTITUDES_first=="TRUST",1,
                            ifelse(IEC_00$IEC_ATTITUDES_second=="TRUST",2,
                                   ifelse(IEC_00$IEC_ATTITUDES_third=="TRUST",3,4)))
##
#recognize other symbols
##
IEC_00 = IEC_00 %>% 
#1st symbol - a (MEKOROT)
  mutate(MEKOROT = ifelse(SYMBOL==0,IEC_raw$Q14,
                        ifelse(SYMBOL==1,IEC_raw$Q79,IEC_raw$Q21))) %>%
  mutate(MEKOROT_recognize = ifelse(MEKOROT==2,1,0))
#2nd symbol - b (IROADS / netivei israel)
  IEC_00 = IEC_00 %>% 
  
    mutate(IROADS = ifelse(SYMBOL==0,IEC_raw$Q18,
                        ifelse(SYMBOL==1,IEC_raw$Q81,IEC_raw$Q25))) %>%
  mutate(IROADS_recognize = ifelse(IROADS==3,1,0)) 
#3rd symbol - c (EAPC)
  IEC_00 = IEC_00 %>% 
  mutate(EAPC = ifelse(SYMBOL==0,IEC_raw$Q16,0)) %>%
mutate(EAPC_recognize = ifelse(EAPC==1,1,0))

#3rd symbol - d (NTA)
  IEC_00 = IEC_00 %>% 
    mutate(NTA = ifelse(SYMBOL==1,IEC_raw$Q83,0)) %>%
    mutate(NTA_recognize = ifelse(NTA==1,1,0)) 

#3rd symbol - e (IEC)  
  IEC_00 = IEC_00 %>% 
    mutate(IEC_symbol = ifelse(SYMBOL==2,IEC_raw$Q27,0)) %>%
    mutate(IEC_symbol_recognize = ifelse(IEC_symbol==1,1,0))

  
#Self reflection on the effects of symbols vs. performance
  IEC_00 = IEC_00 %>% 
mutate(INFLUENCE_PERFORMANCE = IEC_raw$Q61_1,
       INFLUENCE_SYMBOL = IEC_raw$Q70_1)

#Guessing purpose of survey
  IEC_00 = IEC_00 %>%
    mutate(GUESS_text = IEC_raw$Q60) %>%
    mutate(GUESS_filter = ifelse(UID %in% c(5334787, 5341400, 5331055),1,0))
  
  
  
#FILTERING
#work in IEC
  IEC_00 = IEC_00 %>% 
    mutate(IEC_work = ifelse(IEC_raw$Q47_3==1,1,0),
           IEC_work_relative = ifelse(IEC_raw$Q49_3==1,1,0))    

IEC_00$IEC_work[is.na(IEC_00$IEC_work)]=0
IEC_00$IEC_work_relative[is.na(IEC_00$IEC_work_relative)]=0

IEC_00 = IEC_00 %>% 
  mutate(IEC_work_01 = ifelse(IEC_work+IEC_work_relative==0,0,1)) %>% # subject or her family have worked at IEC

#debt to IEC
mutate(IEC_debt = ifelse(IEC_raw$Q51_5==1,1,0))
IEC_00$IEC_debt[is.na(IEC_00$IEC_debt)]=0

#recognize symbol
IEC_00 = IEC_00 %>% 
  mutate(SYMBOL_not_recognize = ifelse(IEC_symbol %in% c("2","3","4","5"),1,0)) %>%

#TIMING
mutate(TIMER_total_filter = ifelse(TIMER_total>30 | IEC_00$TIMER_total<3,1,0))

#IMC
IEC_00 = IEC_00 %>% 
  mutate(IMC = ifelse(IEC_raw$Q68_1!=1 & IEC_raw$Q68_2!=1 & IEC_raw$Q68_3!=1 & IEC_raw$Q68_4!=1 &
                      IEC_raw$Q68_5!=1 & IEC_raw$Q68_6!=1 & IEC_raw$Q68_7!=1 & IEC_raw$Q68_8!=1 &
                      IEC_raw$Q68_9!=1 & IEC_raw$Q68_10!=1,0,1)) %>%
  mutate(IMC = Recode(IMC,"NA=0"))

#NA's
IEC_00 = IEC_00 %>% 
  mutate(MISSING = ifelse(is.na(IEC_raw$Q34_4) | is.na(IEC_raw$Q37_4),1,0))


#datasets#######


#Main dataset
IEC_01 = IEC_00 %>%
  distinct(IP,.keep_all=TRUE) %>% #exclude double IP
  filter(AGE>=18 & SYMBOL_not_recognize==0 & IMC==0 & AREA!="OTHER" & TIMER_total_filter==0 & GUESS_filter==0)

#data set including other cities
IEC_02 = IEC_00 %>%
  distinct(IP,.keep_all=TRUE) %>% #exclude double IP
  filter(AGE>=18 & SYMBOL_not_recognize==0 & IMC==0 & TIMER_total_filter==0 & GUESS_filter==0)

#data excluding those who recognize control symbols
IEC_03 = IEC_00 %>%
  distinct(IP,.keep_all=TRUE) %>% #exclude double IP
  filter(AGE>=18 & SYMBOL_not_recognize==0 & IMC==0 & AREA!="OTHER" & TIMER_total_filter==0 & GUESS_filter==0 & NTA_recognize==0 & EAPC_recognize==0)


  
IEC_000 = IEC_00[,c("ID","IP","START_DATE","END_DATE","TIMER_total","SYMBOL",
                     "EVALUATION_01","EVALUATION_02","EVALUATION_index","SATISFACTION","TRUST",
                     "OUTAGE_q01","OUTAGE_q02","OUTAGE_q03","OUTAGE_01","OUTAGE_02","OUTAGE_03","OUTAGE_long",
                     "AREA","CITY","CITY_short",
                     "GOV_EVALUATION","GOV_SATISFACTION","GOV_TRUST","GOV_ATTITUDES",
                     "INTEREST","GENDER","AGE","EDUCATION","INCOME","HOUSE","IDEOLOGY",
                     "IEC_ATTITUDES_first","IEC_ATTITUDES_second","IEC_ATTITUDES_third","IEC_ATTITUDES_fourth",
                     "MEKOROT_recognize","IROADS_recognize","EAPC_recognize","NTA_recognize","IEC_symbol_recognize",
                     "IEC_work", "IEC_work_relative","IEC_work_01","IEC_debt",
                     "SYMBOL_not_recognize","TIMER_total_filter","IMC","MISSING","GUESS_filter")]
                  
write.csv(IEC_000,file = "PROMSYMB_30.1.17.csv")

rm(IEC_000,IEC_raw,TIMER)


                      

library(nnet)
x=multinom(factor(SYMBOL)~AGE+GENDER+INCOME+EDUCATION+HOUSE+GOV_ATTITUDES+IDEOLOGY+INTEREST+OUTAGE_03+MEKOROT_recognize+IROADS_recognize,data=IEC_01)
stargazer(x,type = "html",out="x.htm")

#Inter-item Reliablity####
alpha(IEC_01[,c("EVALUATION_01","EVALUATION_02")])
alpha(IEC_01[,c("GOV_EVALUATION","GOV_SATISFACTION","GOV_TRUST")])

#ICC tests####
Null.Model.OUTAGE<-lme(OUTAGE_03~1,random=~1|CITY,data=IEC_01,na.action=na.omit)
VarCorr(Null.Model.OUTAGE)
GmeanRel(Null.Model.OUTAGE)
mean(GmeanRel(Null.Model.OUTAGE)$MeanRel)

Null.Model.TRUST<-lme(TRUST~1,random=~1|CITY,data=IEC_01,na.action=na.omit)
VarCorr(Null.Model.TRUST)
GmeanRel(Null.Model.TRUST)
mean(GmeanRel(Null.Model.TRUST)$MeanRel)

Null.Model.OUTAGE<-lme(OUTAGE_03~1,random=~1|CITY,data=IEC_01,na.action=na.omit)
VarCorr(Null.Model.OUTAGE)
GmeanRel(Null.Model.OUTAGE)
mean(GmeanRel(Null.Model.OUTAGE)$MeanRel)



####  ##  ##  ##  ##  ###
#regression models#######
####  ##  ##  ##  ##  ###


IEC_01 = IEC_01  %>%
  mutate(SYMBOL_01 = Recode(SYMBOL,"0=0;1=2;2=1"),
         SYMBOL_02 = Recode(SYMBOL,"0=2;1=0;2=1")) 

tmod_trust1 = lm(TRUST~factor(SYMBOL_01),data=IEC_01,na.action=na.omit)
tmod_trust2 = lm(TRUST~factor(SYMBOL_01)+AREA_01,data=IEC_01,na.action=na.omit)
tmod_trust3 = lm(TRUST~factor(SYMBOL_01)*AREA_01,data=IEC_01,na.action=na.omit)

tmod_sat1 = lm(SATISFACTION~factor(SYMBOL_01),data=IEC_01,na.action=na.omit)
tmod_sat2 = lm(SATISFACTION~factor(SYMBOL_01)+AREA_01,data=IEC_01,na.action=na.omit)
tmod_sat3 = lm(SATISFACTION~factor(SYMBOL_01)*AREA_01,data=IEC_01,na.action=na.omit)

tmod_eval_index1 = lm(EVALUATION_index~factor(SYMBOL_01),data=IEC_01,na.action=na.omit)
tmod_eval_index2 = lm(EVALUATION_index~factor(SYMBOL_01)+AREA_01,data=IEC_01,na.action=na.omit)
tmod_eval_index3 = lm(EVALUATION_index~factor(SYMBOL_01)*AREA_01,data=IEC_01,na.action=na.omit)


stargazer(tmod_trust1,tmod_trust2,tmod_trust3,
          tmod_sat1,tmod_sat2,tmod_sat3,
          tmod_eval_index1,tmod_eval_index2,tmod_eval_index3,
          type="html",
          dep.var.labels=c("Trust in IEC","Satisfaction with IEC services","Evaluation of IEC performance"),
          covariate.labels=c("Treatment","Control cartoon","Area (shfela = 1)","Area x tratment","Area x control cartoon"),
          out="models 1 6.2.17.htm")


tmod_trust1x = lm(TRUST~factor(SYMBOL_02),data=IEC_01,na.action=na.omit)
tmod_trust2x = lm(TRUST~factor(SYMBOL_02)+AREA_01,data=IEC_01,na.action=na.omit)
tmod_trust3x = lm(TRUST~factor(SYMBOL_02)*AREA_01,data=IEC_01,na.action=na.omit)


tmod_sat1x = lm(SATISFACTION~factor(SYMBOL_02),data=IEC_01,na.action=na.omit)
tmod_sat2x = lm(SATISFACTION~factor(SYMBOL_02)+AREA_01,data=IEC_01,na.action=na.omit)
tmod_sat3x = lm(SATISFACTION~factor(SYMBOL_02)*AREA_01,data=IEC_01,na.action=na.omit)

tmod_eval_index1x = lm(EVALUATION_index~factor(SYMBOL_02),data=IEC_01,na.action=na.omit)
tmod_eval_index2x = lm(EVALUATION_index~factor(SYMBOL_02)+AREA_01,data=IEC_01,na.action=na.omit)
tmod_eval_index3x = lm(EVALUATION_index~factor(SYMBOL_02)*AREA_01,data=IEC_01,na.action=na.omit)




#Robust analyses
tmod_trust_robust_01 = lm(TRUST~factor(SYMBOL_01)+AREA_01+OUTAGE_q02+OUTAGE_q03,data=IEC_01,na.action=na.omit)
tmod_trust_robust_02 = lm(TRUST~factor(SYMBOL_01)+AREA_01+OUTAGE_q02+OUTAGE_q03+GOV_ATTITUDES+AGE+INCOME+HOUSE+GENDER+IDEOLOGY+INTEREST+IEC_work_01,data=IEC_01,na.action=na.omit)
tmod_trust_robust_03 = lm(TRUST~factor(SYMBOL_01)+AREA_01,data=IEC_01[IEC_01$TRUST_order==1,],na.action=na.omit)

tmod_sat_robust_01 = lm(SATISFACTION~factor(SYMBOL_01)+AREA_01+OUTAGE_q02+OUTAGE_q03,data=IEC_01,na.action=na.omit)
tmod_sat_robust_02 = lm(SATISFACTION~factor(SYMBOL_01)+AREA_01+OUTAGE_q02+OUTAGE_q03+GOV_ATTITUDES+AGE+INCOME+HOUSE+GENDER+IDEOLOGY+INTEREST+IEC_work_01,data=IEC_01,na.action=na.omit)
tmod_sat_robust_03 = lm(SATISFACTION~factor(SYMBOL_01)+AREA_01,data=IEC_01[IEC_01$SATISFACTION_order==1,],na.action=na.omit)

tmod_eval_robust_01 = lm(EVALUATION_index~factor(SYMBOL_01)+AREA_01+OUTAGE_q02+OUTAGE_q03,data=IEC_01,na.action=na.omit)
tmod_eval_robust_02 = lm(EVALUATION_index~factor(SYMBOL_01)+AREA_01+OUTAGE_q02+OUTAGE_q03+GOV_ATTITUDES+AGE+INCOME+HOUSE+GENDER+IDEOLOGY+INTEREST+IEC_work_01,data=IEC_01,na.action=na.omit)
tmod_eval_robust_03 = lm(EVALUATION_index~factor(SYMBOL_01)+AREA_01,data=IEC_01[(IEC_01$EVALUATION_01_order==1&IEC_01$EVALUATION_02_order==2)|(IEC_01$EVALUATION_01_order==2&IEC_01$EVALUATION_02_order==1),],na.action=na.omit)


stargazer(tmod_trust_robust_01,tmod_trust_robust_02,tmod_trust_robust_03,
          tmod_sat_robust_01,tmod_sat_robust_02,tmod_sat_robust_03,
          tmod_eval_robust_01,tmod_eval_robust_02,tmod_eval_robust_03,
          type="html",
          dep.var.labels=c("Trust in IEC","Satisfaction with IEC services","Evaluation of IEC performance"),
          covariate.labels=c("Treatment","Control cartoon","Area (shfela = 1)",
                             "length of outages","outages experienced by relatives",
                             "Attitudes vis-a-vis government","Age","Income","Home ownership",
                             "Gender (Gender (Women=1))","Ideology","Interest in politics",
                             "relative IEC worker"),
           out="models 6.2.17.htm")






### ### ### ### ###
#Plots#######
### ### ### ### ###

#Plot symbol effect (figure 4)

# distribution of power outage across cities and areas (figure 2)

IEC_01 = IEC_01 %>%
  mutate(OUTAGE_order = Recode(CITY_short,
"'MDN'=9;'RGN'=8;'RLZ'=7;'NTZ'=6;'HDS'=5;'RSR'=4;'HZL'=3;'KFS'=2;'RNA'=1")) %>%
  mutate(OUTAGE_q02.2 = Recode(OUTAGE_q02,"0=0;1=1;2=1;3=3;4=4;5=5")) %>%
  mutate(OUTAGE_q02.3 = Recode(OUTAGE_q02.2,"0=5;1=4;3=3;4=1;5=0"))
  


g2.cities = ggplot(data = IEC_01, aes(x = factor(OUTAGE_order),fill=factor(OUTAGE_q02.3)))+
  geom_bar(position = "fill",colour="black")+
  scale_fill_manual(values = c("5"="gray100","4"="gray83","3"="gray50","1"="gray33","0"="gray10"),
                    labels = c("more than 48 hours","24-48 hours","12-24 hours","1-12 hours","non"),
                    breaks = c("0","1","3","4","5"),
                    name="Longest outage 
experienced during  
past 6 months")+
  scale_x_discrete(name="Cities",labels = c("RNA","KFS","HZL","RSR","HDS","NTZ","RLZ","RGN","MDN"))+
  scale_y_continuous(name="Relative frequency")+ 
  theme(axis.text.x = element_text(size=10,angle=90),panel.background = element_rect((fill = "white")))
ggsave("g2.cities.tiff",dpi=400)

g2.areas = ggplot(data = IEC_01, aes(x = AREA,fill=factor(OUTAGE_q02.3)))+
  geom_bar(position = "fill",colour="black")+
  scale_fill_manual(values = c("5"="gray100","4"="gray83","3"="gray50","1"="gray33","0"="gray10"),
                    labels = c("more than 48 hours","24-48 hours","12-24 hours","1-12 hours","non"),
                    breaks = c("0","1","3","4","5"),
                    name="Longest outage experienced during 
                    past 6 months")+
  scale_x_discrete(name="areas",breaks=c("SHFELA","SHARON"))+
  scale_y_continuous(name="Relative frequency")+ 
  theme(axis.text.x = element_text(size=10,angle=90),panel.background = element_rect((fill = "white")))
ggsave("g2.areas.tiff",dpi=400)


summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}




t=filter(IEC_01,!(TRUST %in% c(NA)))
t = t %>%
  mutate(SYMBOL_bi = ifelse(SYMBOL==2,1,0))

TDAT_EVALUATION_index_bi <- summarySE(t, measurevar="EVALUATION_index", groupvars=c("SYMBOL_bi","AREA"))
TDAT_SATISFACTION_bi <- summarySE(t, measurevar="SATISFACTION", groupvars=c("SYMBOL_bi","AREA"))
TDAT_TRUST_bi <- summarySE(t, measurevar="TRUST", groupvars=c("SYMBOL_bi","AREA"))

#Trust

 ggplot(TDAT_TRUST_bi, aes(x=AREA, y=TRUST,fill=factor(SYMBOL_bi))) + 
  geom_bar(position=position_dodge(), stat="identity",width = 0.7,colour="black",size=.3) +
  geom_errorbar(aes(ymin=TRUST, ymax=TRUST+ci), width=.2,position=position_dodge(0.7),size=.3) +
  scale_y_continuous(name = "", breaks = c(1:7),limits = c(0,7))+
  scale_x_discrete(name="",labels=c("Sharon","Shfela"))+
  scale_fill_manual(name="condition",values=c("gray90","gray20"),labels=c("control  (combined)","treatment")) +
  ggtitle("Trust in IEC")+ 
  theme_classic()
ggsave(filename = "trust.tiff",dpi=300,width = 4)

#Satisfaction
ggplot(TDAT_SATISFACTION_bi, aes(x=AREA, y=SATISFACTION,fill=factor(SYMBOL_bi))) + 
  geom_bar(position=position_dodge(), stat="identity",width = 0.7,colour="black",size=.3) +
  geom_errorbar(aes(ymin=SATISFACTION, ymax=SATISFACTION+ci), width=.2,position=position_dodge(0.7),size=.3) +
  scale_y_continuous(name = "", breaks = c(1:7),limits = c(0,7))+
  scale_x_discrete(name="Area",labels=c("Sharon","Shfela"))+
  scale_fill_manual(name="condition",values=c("gray90","gray20"),labels=c("control  (combined)","treatment")) +
  ggtitle("SATISFACTION")+ 
  theme_classic()
ggsave(filename = "satisfaction.tiff",dpi=300,width = 4)

#Evaluation
ggplot(TDAT_EVALUATION_index_bi, aes(x=AREA, y=EVALUATION_index,fill=factor(SYMBOL_bi))) + 
  geom_bar(position=position_dodge(), stat="identity",width = 0.7,colour="black",size=.3) +
  geom_errorbar(aes(ymin=EVALUATION_index, ymax=EVALUATION_index+ci), width=.2,position=position_dodge(0.7),size=.3) +
  scale_y_continuous(name = "", breaks = c(1:7),limits = c(0,7))+
  scale_x_discrete(name="Area",labels=c("Sharon","Shfela"))+
  scale_fill_manual(name="condition",values=c("gray90","gray20"),labels=c("control (combined)","treatment")) +
  ggtitle("EVALUATION")+ 
  theme_classic()
ggsave(filename = "evaluation.tiff",dpi=300,width = 4)




#plot of participants' self-reflections regarding the effects of symbols vs. performance (figure 4)
require(scales)

g33=
  ggplot(IEC_01, aes(x=factor(INFLUENCE_PERFORMANCE))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_x_discrete(name = "",breaks = c(1:10),labels=c("very little","2","3","4","5","6","7","8","9","very much"))+
  scale_y_continuous(name = "" ,labels = percent)+
  ggtitle("Attitudes influenced by performance") +
  theme(plot.title = element_text(hjust = 0.5),panel.background = element_rect((fill = "white")))
ggsave(filename = "figure 4.1.tiff",dpi=300)

g34=
  ggplot(IEC_01, aes(x=factor(INFLUENCE_SYMBOL))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_x_discrete(name = "",breaks = c(1:10),labels=c("very little","2","3","4","5","6","7","8","9","very much"))+
  ggtitle("Attitudes influenced by promotional symbols") +
  scale_y_continuous(name = "" ,labels = percent)+
  theme(plot.title = element_text(hjust = 0.5),panel.background = element_rect((fill = "white")))
ggsave(filename = "figure 4.2.tiff",dpi=300)

### ### ### ### ###
#online appendix#######
### ### ### ### ###

#randomization validation - comparison of condition samples (Table 1)#######

t = IEC_01 %>% group_by(factor(SYMBOL)) %>%
  summarise(AGE.mean = mean(AGE))            



t = IEC_01 %>% group_by(SYMBOL) %>%
  summarise(AGE.mean = mean(AGE),AGE.sd = sd(AGE),
            GENDER.mean = mean(GENDER,na.rm=TRUE),GENDER.sd = sd(GENDER,na.rm=TRUE),
            INCOME.mean = mean(INCOME),INCOME.sd = sd(INCOME),
            EDUCATION.mean = mean(EDUCATION),EDUCATION.sd = sd(EDUCATION),
            HOUSE.mean = mean(HOUSE),HOUSE.sd = sd(HOUSE),
            GOV_ATTITUDES.mean = mean(GOV_ATTITUDES),GOV_ATTITUDES.sd = sd(GOV_ATTITUDES),
            IDEOLOGY.mean = mean(IDEOLOGY),IDEOLOGY.sd = sd(IDEOLOGY),
            INTEREST.mean = mean(INTEREST),INTEREST.sd = sd(INTEREST),
            AREA.mean = mean(AREA_01),AREA.sd = sd(AREA_01),
            OUTAGE_q01.mean = mean(OUTAGE_q01),OUTAGE_q01.sd = sd(OUTAGE_q01),
            OUTAGE_q02.mean = mean(OUTAGE_q02),OUTAGE_q02.sd = sd(OUTAGE_q02),
            OUTAGE_q03.mean = mean(OUTAGE_q03),OUTAGE_q03.sd = sd(OUTAGE_q03),
            MEKOROT_recognize.mean = mean(MEKOROT_recognize),MEKOROT_recognize.sd = sd(MEKOROT_recognize),
            IROADS_recognize.mean = mean(IROADS_recognize),IROADS_recognize.sd = sd(IROADS_recognize))

COMPCOND = data.frame(cbind(1:14,1:14)) %>%
  mutate(variable = c('Age','Gender (Women=1)','Income','Education','Home Ownership','Attitudes vis-a-vis government',
                      'Political Ideology','Interest in Politics','Area (Sharon=0)',
                      'Number of power outages','Longest power outage','Number of power outages for relatives',
                      'Recognizes 1st symbol','Recognizes 2nd symbol')) %>%
  
  mutate(control.x = round(as.numeric(c(t[1,2],t[1,4],t[1,6],t[1,8],t[1,10],t[1,12],t[1,14],t[1,16],
                                        t[1,18],t[1,20],t[1,22],t[1,24],t[1,26],t[1,28])),3),
         control.sd= round(as.numeric(c(t[1,3],t[1,5],t[1,7],t[1,9],t[1,11],t[1,13],t[1,15],t[1,17],
                                        t[1,19],t[1,21],t[1,23],t[1,25],t[1,27],t[1,29])),3),
         placebo.x = round(as.numeric(c(t[2,2],t[2,4],t[2,6],t[2,8],t[2,10],t[2,12],t[2,14],t[2,16],
                                        t[2,18],t[2,20],t[2,22],t[2,24],t[2,26],t[2,28])),3),
         placebo.sd= round(as.numeric(c(t[2,3],t[2,5],t[2,7],t[2,9],t[2,11],t[2,13],t[2,15],t[2,17],
                                        t[2,19],t[2,21],t[2,23],t[2,25],t[2,27],t[2,29])),3),
         treatment.x = round(as.numeric(c(t[3,2],t[3,4],t[3,6],t[3,8],t[3,10],t[3,12],t[3,14],t[3,16],
                                          t[3,18],t[3,20],t[3,22],t[3,24],t[3,26],t[3,28])),3),
         treatment.sd= round(as.numeric(c(t[3,3],t[3,5],t[3,7],t[3,9],t[3,11],t[3,13],t[3,15],t[3,17],
                                          t[3,19],t[3,21],t[3,23],t[3,25],t[3,27],t[3,29])),3)) %>%
  
  mutate(F.value = c(summary(aov(AGE ~ factor(SYMBOL), data = IEC_01))[[1]][["F value"]][1],
                     summary(aov(GENDER ~ factor(SYMBOL), data = IEC_01))[[1]][["F value"]][1],
                     summary(aov(INCOME ~ factor(SYMBOL), data = IEC_01))[[1]][["F value"]][1],
                     summary(aov(EDUCATION ~ factor(SYMBOL), data = IEC_01))[[1]][["F value"]][1],
                     summary(aov(HOUSE ~ factor(SYMBOL), data = IEC_01))[[1]][["F value"]][1],
                     summary(aov(GOV_ATTITUDES ~ factor(SYMBOL), data = IEC_01))[[1]][["F value"]][1],
                     summary(aov(IDEOLOGY ~ factor(SYMBOL), data = IEC_01))[[1]][["F value"]][1],
                     summary(aov(INTEREST ~ factor(SYMBOL), data = IEC_01))[[1]][["F value"]][1],
                     summary(aov(AREA_01 ~ factor(SYMBOL), data = IEC_01))[[1]][["F value"]][1],
                     summary(aov(OUTAGE_q01 ~ factor(SYMBOL), data = IEC_01))[[1]][["F value"]][1],
                     summary(aov(OUTAGE_q02 ~ factor(SYMBOL), data = IEC_01))[[1]][["F value"]][1],
                     summary(aov(OUTAGE_q03 ~ factor(SYMBOL), data = IEC_01))[[1]][["F value"]][1],
                     summary(aov(MEKOROT_recognize ~ factor(SYMBOL), data = IEC_01))[[1]][["F value"]][1],
                     summary(aov(IROADS_recognize ~ factor(SYMBOL), data = IEC_01))[[1]][["F value"]][1])) %>%

  mutate(p.value = c(summary(aov(AGE ~ factor(SYMBOL), data = IEC_01))[[1]][["Pr(>F)"]][1],
                     summary(aov(GENDER ~ factor(SYMBOL), data = IEC_01))[[1]][["Pr(>F)"]][1],
                     summary(aov(INCOME ~ factor(SYMBOL), data = IEC_01))[[1]][["Pr(>F)"]][1],
                     summary(aov(EDUCATION ~ factor(SYMBOL), data = IEC_01))[[1]][["Pr(>F)"]][1],
                     summary(aov(HOUSE ~ factor(SYMBOL), data = IEC_01))[[1]][["Pr(>F)"]][1],
                     summary(aov(GOV_ATTITUDES ~ factor(SYMBOL), data = IEC_01))[[1]][["Pr(>F)"]][1],
                     summary(aov(IDEOLOGY ~ factor(SYMBOL), data = IEC_01))[[1]][["Pr(>F)"]][1],
                     summary(aov(INTEREST ~ factor(SYMBOL), data = IEC_01))[[1]][["Pr(>F)"]][1],
                     summary(aov(AREA_01 ~ factor(SYMBOL), data = IEC_01))[[1]][["Pr(>F)"]][1],
                     summary(aov(OUTAGE_q01 ~ factor(SYMBOL), data = IEC_01))[[1]][["Pr(>F)"]][1],
                     summary(aov(OUTAGE_q02 ~ factor(SYMBOL), data = IEC_01))[[1]][["Pr(>F)"]][1],
                     summary(aov(OUTAGE_q03 ~ factor(SYMBOL), data = IEC_01))[[1]][["Pr(>F)"]][1],
                     summary(aov(MEKOROT_recognize ~ factor(SYMBOL), data = IEC_01))[[1]][["Pr(>F)"]][1],
                     summary(aov(IROADS_recognize ~ factor(SYMBOL), data = IEC_01))[[1]][["Pr(>F)"]][1])) %>%
  select(-c(X1,X2))

write.table(COMPCOND,file="clipboard-2000",sep="\t",row.names = FALSE) # output table to clipboard

rm(t)        




#Summary statistics tables#######

#summary statistics (APPENDIX C)

stargazer(IEC_01[,c("EVALUATION_index","SATISFACTION","TRUST",
                    "AREA_01","OUTAGE_q01","OUTAGE_q02","OUTAGE_q03","AGE","GENDER",
                    "INCOME","EDUCATION","HOUSE","GOV_ATTITUDES","IDEOLOGY","INTEREST",
                    "IEC_work_01","MEKOROT_recognize","IROADS_recognize")], 
          covariate.labels = c("Evaluation of IEC performance","Satisfaction with IEC service",
                               "Trust in IEC","Area (Shfela = 1)",
                               "Number of outages","Length of longest outage","Number of outages for relatives",
                               "Age","Gender (women = 1)","Income","Education","Home Ownership",
                               "Attitudes vis-a-vis government","Political Ideology","Interest in Politics",
                               "Relative IEC worker","Recognizes 1st symbol","Recognizes 2nd symbol"),
          type = "text", title="Summary statistics", digits=3, out="summary statistics 31.1.17.htm")



#bivariate correlation matrix


corstarsl <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .01, "***", ifelse(p < .05, "** ", ifelse(p < .1, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}



print(xtable(corstarsl(IEC_01[,
                              c("EVALUATION_index","SATISFACTION","TRUST",
                                "AREA_01","OUTAGE_q01","OUTAGE_q02","OUTAGE_q03","AGE","GENDER",
                                "INCOME","EDUCATION","HOUSE","GOV_ATTITUDES","IDEOLOGY","INTEREST",
                                "IEC_work_01","MEKOROT_recognize","IROADS_recognize")])),
      type="html",file="cormatrix_IEC 31.1.17.htm")


####additional regression models#####


#1: Random effect models

ap_1_trust_1 = lme(TRUST~factor(SYMBOL_01),random=~1|CITY,data=IEC_01,na.action=na.omit)
ap_1_trust_2 = lme(TRUST~factor(SYMBOL_01)+AREA_01,random=~1|CITY,data=IEC_01,na.action=na.omit)
ap_1_trust_3 = lme(TRUST~factor(SYMBOL_01)*AREA_01,random=~1|CITY,data=IEC_01,na.action=na.omit)

ap_1_sat_1 = lme(SATISFACTION~factor(SYMBOL_01),random=~1|CITY,data=IEC_01,na.action=na.omit)
ap_1_sat_2 = lme(SATISFACTION~factor(SYMBOL_01)+AREA_01,random=~1|CITY,data=IEC_01,na.action=na.omit)
ap_1_sat_3 = lme(SATISFACTION~factor(SYMBOL_01)*AREA_01,random=~1|CITY,data=IEC_01,na.action=na.omit)

ap_1_eval_1 = lme(EVALUATION_index~factor(SYMBOL_01),random=~1|CITY,data=IEC_01,na.action=na.omit)
ap_1_eval_2 = lme(EVALUATION_index~factor(SYMBOL_01)+AREA_01,random=~1|CITY,data=IEC_01,na.action=na.omit)
ap_1_eval_3 = lme(EVALUATION_index~factor(SYMBOL_01)*AREA_01,random=~1|CITY,data=IEC_01,na.action=na.omit)

stargazer(ap_1_trust_1,ap_1_trust_2,ap_1_trust_3,
          ap_1_sat_1,ap_1_sat_2,ap_1_sat_3,
          ap_1_eval_1,ap_1_eval_2,ap_1_eval_3,
          type="html",
          dep.var.labels=c("Trust in IEC","Satisfaction with IEC services","Evaluation of IEC performance"),
          covariate.labels=c("Treatment","Control cartoon","Area (shfela = 1)","Area x tratment","Area x control cartoon"),
          out="appendix models 1 6.2.17.htm")

ap_1_trust_1x = lme(TRUST~factor(SYMBOL_02),random=~1|CITY,data=IEC_01,na.action=na.omit)
ap_1_trust_2x = lme(TRUST~factor(SYMBOL_02)+AREA_01,random=~1|CITY,data=IEC_01,na.action=na.omit)
ap_1_trust_3x = lme(TRUST~factor(SYMBOL_02)*AREA_01,random=~1|CITY,data=IEC_01,na.action=na.omit)

ap_1_sat_1x = lme(SATISFACTION~factor(SYMBOL_02),random=~1|CITY,data=IEC_01,na.action=na.omit)
ap_1_sat_2x = lme(SATISFACTION~factor(SYMBOL_02)+AREA_01,random=~1|CITY,data=IEC_01,na.action=na.omit)
ap_1_sat_3x = lme(SATISFACTION~factor(SYMBOL_02)*AREA_01,random=~1|CITY,data=IEC_01,na.action=na.omit)

ap_1_eval_1x = lme(EVALUATION_index~factor(SYMBOL_02),random=~1|CITY,data=IEC_01,na.action=na.omit)
ap_1_eval_2x = lme(EVALUATION_index~factor(SYMBOL_02)+AREA_01,random=~1|CITY,data=IEC_01,na.action=na.omit)
ap_1_eval_3x = lme(EVALUATION_index~factor(SYMBOL_02)*AREA_01,random=~1|CITY,data=IEC_01,na.action=na.omit)


#2: fixed effects + including other cities

IEC_02 = IEC_02  %>%
  mutate(SYMBOL_01 = Recode(SYMBOL,"0=0;1=2;2=1"),
         SYMBOL_02 = Recode(SYMBOL,"0=2;1=0;2=1"),
         AREA_02 = ifelse(AREA=="SHARON",0,1))

IEC_03 = IEC_03  %>%
  mutate(SYMBOL_01 = Recode(SYMBOL,"0=0;1=2;2=1"),
         SYMBOL_02 = Recode(SYMBOL,"0=2;1=0;2=1"),
         AREA_02 = ifelse(AREA=="SHARON",0,1))

ap_2_trust_1 = lm(TRUST~factor(SYMBOL_01)+OUTAGE_q02+OUTAGE_q03+CITY,data=IEC_01,na.action=na.omit)
ap_2_trust_2 = lm(TRUST~factor(SYMBOL_01)+OUTAGE_q02+OUTAGE_q03+CITY,data=IEC_02,na.action=na.omit)
ap_2_trust_3 = lm(TRUST~factor(SYMBOL_01)+AREA_01,data=IEC_03,na.action=na.omit)

ap_2_sat_1 = lm(SATISFACTION~factor(SYMBOL_01)+OUTAGE_q02+OUTAGE_q03+CITY,data=IEC_01,na.action=na.omit)
ap_2_sat_2 = lm(SATISFACTION~factor(SYMBOL_01)+OUTAGE_q02+OUTAGE_q03+CITY,data=IEC_02,na.action=na.omit)
ap_2_sat_3 = lm(SATISFACTION~factor(SYMBOL_01)+AREA_01,data=IEC_03,na.action=na.omit)

ap_2_eval_1 = lm(EVALUATION_index~factor(SYMBOL_01)+OUTAGE_q02+OUTAGE_q03+CITY,data=IEC_01,na.action=na.omit)
ap_2_eval_2 = lm(EVALUATION_index~factor(SYMBOL_01)+OUTAGE_q02+OUTAGE_q03+CITY,data=IEC_02,na.action=na.omit)
ap_2_eval_3 = lm(EVALUATION_index~factor(SYMBOL_01)+AREA_01,data=IEC_03,na.action=na.omit)


stargazer(ap_2_trust_1,ap_2_trust_2,ap_2_trust_3,
          ap_2_sat_1,ap_2_sat_2,ap_2_sat_3,
          ap_2_eval_1,ap_2_eval_2,ap_2_eval_3,
          type="html",omit="CITY",omit.labels = "Fixed Effects for Cities",
          covariate.labels=c("Treatment","Control cartoon",
                             "length of outages","outages experienced by relatives",
                             "Area (shfela = 1)"),
          dep.var.labels=c("Trust in IEC","Satisfaction with IEC services","Evaluation of IEC performance"),
          out="appendix models 2 6.2.17.htm")


ap_2_trust_1x = lm(TRUST~factor(SYMBOL_02)+OUTAGE_q02+OUTAGE_q03+CITY,data=IEC_01,na.action=na.omit)
ap_2_trust_2x = lm(TRUST~factor(SYMBOL_02)+OUTAGE_q02+OUTAGE_q03+CITY,data=IEC_02,na.action=na.omit)

ap_2_sat_1x = lm(SATISFACTION~factor(SYMBOL_02)+OUTAGE_q02+OUTAGE_q03+CITY,data=IEC_01,na.action=na.omit)
ap_2_sat_2x = lm(SATISFACTION~factor(SYMBOL_02)+OUTAGE_q02+OUTAGE_q03+CITY,data=IEC_02,na.action=na.omit)
ap_2_sat_3x = lm(SATISFACTION~factor(SYMBOL_02)+AREA_01,data=IEC_03,na.action=na.omit)

ap_2_eval_1x = lm(EVALUATION_index~factor(SYMBOL_02)+OUTAGE_q02+OUTAGE_q03+CITY,data=IEC_01,na.action=na.omit)
ap_2_eval_2x = lm(EVALUATION_index~factor(SYMBOL_02)+OUTAGE_q02+OUTAGE_q03+CITY,data=IEC_02,na.action=na.omit)
ap_2_eval_3x = lm(EVALUATION_index~factor(SYMBOL_02)+AREA_01,data=IEC_03,na.action=na.omit)


#3: Excluding those who recognize the control symbols
ap_3_trust_1 = lm(TRUST~factor(SYMBOL_01),data=IEC_03,na.action=na.omit)
ap_3_trust_2 = lm(TRUST~factor(SYMBOL_01)+AREA_01,data=IEC_03,na.action=na.omit)
ap_3_trust_3 = lm(TRUST~factor(SYMBOL_01)*AREA_01,data=IEC_03,na.action=na.omit)

ap_3_sat_1 = lm(SATISFACTION~factor(SYMBOL_01),data=IEC_03,na.action=na.omit)
ap_3_sat_2 = lm(SATISFACTION~factor(SYMBOL_01)+AREA_01,data=IEC_03,na.action=na.omit)
ap_3_sat_3 = lm(SATISFACTION~factor(SYMBOL_01)*AREA_01,data=IEC_03,na.action=na.omit)

ap_3_eval_1 = lm(EVALUATION_index~factor(SYMBOL_01),data=IEC_03,na.action=na.omit)
ap_3_eval_2 = lm(EVALUATION_index~factor(SYMBOL_01)+AREA_01,data=IEC_03,na.action=na.omit)
ap_3_eval_3 = lm(EVALUATION_index~factor(SYMBOL_01)*AREA_01,data=IEC_03,na.action=na.omit)

stargazer(ap_3_trust_1,ap_3_trust_2,ap_3_trust_3,
          ap_3_sat_1,ap_3_sat_2,ap_3_sat_3,
          ap_3_eval_1,ap_3_eval_2,ap_3_eval_3,
          type="html",
          dep.var.labels=c("Trust in IEC","Satisfaction with IEC services","Evaluation of IEC performance"),
          covariate.labels=c("Treatment","Control cartoon","Area (shfela = 1)","Area x tratment","Area x control cartoon"),
          out="appendix models 3 6.2.17.htm")

ap_3_trust_1x = lm(TRUST~factor(SYMBOL_02),data=IEC_03,na.action=na.omit)
ap_3_trust_2x = lm(TRUST~factor(SYMBOL_02)+AREA_01,data=IEC_03,na.action=na.omit)
ap_3_trust_3x = lm(TRUST~factor(SYMBOL_02)*AREA_01,data=IEC_03,na.action=na.omit)

ap_3_sat_1x = lm(SATISFACTION~factor(SYMBOL_02),data=IEC_03,na.action=na.omit)
ap_3_sat_2x = lm(SATISFACTION~factor(SYMBOL_02)+AREA_01,data=IEC_03,na.action=na.omit)
ap_3_sat_3x = lm(SATISFACTION~factor(SYMBOL_02)*AREA_01,data=IEC_03,na.action=na.omit)

ap_3_eval_1x = lm(EVALUATION_index~factor(SYMBOL_02),data=IEC_03,na.action=na.omit)
ap_3_eval_2x = lm(EVALUATION_index~factor(SYMBOL_02)+AREA_01,data=IEC_03,na.action=na.omit)
ap_3_eval_3x = lm(EVALUATION_index~factor(SYMBOL_02)*AREA_01,data=IEC_03,na.action=na.omit)


###

