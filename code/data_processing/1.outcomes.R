#### 0. packages #####
rm(list = ls())

library(lubridate)
require(tidyverse)
library(readxl)
library(dplyr)
# library(ggplot2)


path = "/Users/yuanshengzhang/Desktop/20240422"
data_path = paste0(path,"/data/")
wd_path = paste0(path,"/data_processing/")
code_path = paste0(path,"/code/")
setwd(wd_path)

#### 1. Baseline #####
PatientBaseline <- read.csv(paste0(data_path,"Data_20231214/FUP121_PatientBaseline_14DEC23_v3.csv"), header=TRUE, sep = ",", stringsAsFactors=FALSE) 
dim(PatientBaseline) # 2992   57

# 1.1 time format -------------------------------------------------------------
# but for death and dropout date we will deal with it after merge them together
PatientBaseline = PatientBaseline %>% mutate(across(contains(c('assdate'), ignore.case = TRUE), lubridate::dmy))
PatientBaseline$assperiod = 0
#head(PatientBaseline)

# Reading the second sheet and only cells from A1 to C10
cause_death <- read_excel(paste0(data_path,"Data_20231214/FUP121_PatientBaseline_14DEC23_ddz_DeathClassification_2ndObserver.xlsx"), sheet = 1, skip = 1, col_names = TRUE)


cause_death_simple<-cause_death[,c("patid", "out", "deathdate", "Death cause")]
cause_death_simple$`Death cause` <-ifelse(cause_death_simple$`Death cause`  =="0", NA,cause_death_simple$`Death cause` )
dim(cause_death_simple)
# Remove rows where all elements are NA
cause_death_simple <- cause_death_simple[!is.na(cause_death_simple$patid), ]
dim(cause_death_simple) #2992


#### 2. Case #####
Cases <- read.csv(paste0(data_path,"Data_20231214/FUP121_Cases_14DEC23.csv"), header=TRUE, sep = ",", stringsAsFactors=FALSE) 
dim(Cases)# 3051   
length(unique(Cases$patid))# 2992

# 2.1 time format ---------------------------------------------------------

Cases$tpxdate <-as.Date(Cases$tpxdate)
Cases = Cases %>%
  mutate(across(contains(c('listdate','etiodate_0','dialysisdate'), ignore.case = TRUE), lubridate::dmy)) %>%
  mutate(across(contains(c('coldisch'), ignore.case = TRUE), lubridate::hm)) %>%
  select("patid","soascaseid","centreid","organ","age_at_tpx","resec_organtype",
         "tpxdate","listdate","etiodate_0","etio_0","etioyy_0","etiomm_0","coldisch","dialysisdate","dialysistype",
         "donsex","donage","dontype","sumhlamismatch")

res <- hms(Cases$coldisch)        # format to 'hours:minutes:seconds'
Cases$coldisch<-hour(res)*60 + minute(res)       # convert hours to minutes, and add minutes
# head(Cases)
Cases$dialysistime<-Cases$tpxdate-Cases$dialysisdate

Cases$dialysistime <- ifelse(Cases$dialysistype=="None",0,Cases$dialysistime)




rm( list = Filter( exists, c("res") ) )


#### 3. Case #####
Cases2 <- read.csv (paste0(data_path,"ER_Data_20211113/ER_BLPERITRANSPLANT_KIDNEY.csv"), header=TRUE, sep = ",", stringsAsFactors=FALSE) 
dim(Cases2)# 3051   
length(unique(Cases2$patid))# 2992

# 3.1 time format ---------------------------------------------------------

Cases2 = Cases2 %>%
  mutate(across(contains(c('listdate','etiodate_0','dialysisdate'), ignore.case = TRUE), lubridate::dmy)) %>%
  mutate(across(contains(c('coldisch'), ignore.case = TRUE), lubridate::hm)) %>%
  rename(blood_pf = ec) %>% 
  rename(pregnant_before = ss)%>% 
  select("patid","soascaseid","pnfcause","pnfcauseoth","dgf","dgfduration","urine24","nephrectomy","blood_pf","pregnant_before")

# head(Cases)
Cases <- left_join(Cases, Cases2, by = c("patid", "soascaseid"))


#### 4. PatientAllCasesOrganStatus #####

PatientAllCasesOrganStatus <- read.csv (paste0(data_path,"Data_20231214/FUP121_PatientAllCasesOrganStatus_14DEC23.csv"), header=TRUE, sep = ",", stringsAsFactors=FALSE) 
dim(PatientAllCasesOrganStatus) # 3251 8, including other organ transplantations
length(unique(PatientAllCasesOrganStatus$patid)) # 2992
length(unique(PatientAllCasesOrganStatus$soascaseid)) # 3137, including simultaneous transplantation, K-P or K-liver, but K-K is in 1 row.

# 4.1 time format ---------------------------------------------------------
PatientAllCasesOrganStatus$tpxdate <-as.Date(PatientAllCasesOrganStatus$tpxdate)
PatientAllCasesOrganStatus = PatientAllCasesOrganStatus %>% 
  mutate(across(contains(c('organtype_statusdate'), ignore.case = TRUE), lubridate::dmy))

table(PatientAllCasesOrganStatus$organtype_status)
# All "Not applicable" and "PNF" are 9999-01-01, will use the end date of observation instead

#
PatientAllCasesOrganStatus[PatientAllCasesOrganStatus$organtype_statusdate == as.Date("7777-01-01"),]$organtype_status = "Not applicable"

PatientAllCasesOrganStatus[PatientAllCasesOrganStatus$organtype_status == "Not applicable",]$organtype_statusdate = as.Date("2022-12-31")

PatientAllCasesOrganStatus[grepl("PNF", PatientAllCasesOrganStatus$organtype_status),]$organtype_status = "PNF"

PatientAllCasesOrganStatus$organtype_statusdate = as.Date(ifelse(PatientAllCasesOrganStatus$organtype_status== "PNF",
                                                         as.Date(PatientAllCasesOrganStatus$tpxdate),
                                                         as.Date(PatientAllCasesOrganStatus$organtype_statusdate)))


#### 5. PatientsDiseases (MACE) #####
PatientsDiseases <- read.csv (paste0(data_path,"Data_20231214/FUP121_PatientsDiseases_14DEC23_MACE.csv"), header=TRUE, sep = ",", stringsAsFactors=FALSE) 
dim(PatientsDiseases)# 63356     9

PatientsDiseases <- PatientsDiseases %>% mutate(across(contains('date_in', ignore.case = TRUE), lubridate::dmy))
head(PatientsDiseases)

PatientsDiseases<-PatientsDiseases[PatientsDiseases$type != "None" & PatientsDiseases$type != "",] 
dim(PatientsDiseases)# 35213     9

year(PatientsDiseases[which(year(PatientsDiseases$date_in)>2040 & !is.na(PatientsDiseases$date_in)),]$date_in) <- year(PatientsDiseases[which(year(PatientsDiseases$date_in)>2040 & !is.na(PatientsDiseases$date_in)),]$date_in)-100

table(year(PatientsDiseases$date_in))

######### 6. count transplantations ############
#### 6.1. Count number of transplantation #####

#### 6.1.1 before 2008, Count previous number of transplantation (kidney, heart and other) ####
df_pretx_organ<-PatientBaseline[, c(1, grep("pretx_",names(PatientBaseline)))]
head(df_pretx_organ)

getCount <- function(data,keyword){
  data$wcount0 <- str_count(data$pretx_0, keyword)
  data$wcount1 <- str_count(data$pretx_1, keyword)
  data$wcount2 <- str_count(data$pretx_2, keyword)
  count<-rowSums(data[7:9], na.rm=TRUE)
  return(data.frame(data[1],count))
}

n_Kidney<-as.data.frame(getCount(df_pretx_organ,"Kidney"))
n_heart<-as.data.frame(getCount(df_pretx_organ,"Heart"))
n_Liver<-as.data.frame(getCount(df_pretx_organ,c("Liver")))
n_Lung<-as.data.frame(getCount(df_pretx_organ,"Lung"))
n_Pancreas<-as.data.frame(getCount(df_pretx_organ,"Pancreas"))
n_HSCT_auto<-as.data.frame(getCount(df_pretx_organ,"HSCT auto"))
n_HSCT_allo<-as.data.frame(getCount(df_pretx_organ,"HSCT allo"))


df_pretx_organ_n <- Reduce(function(x,y) merge(x,y,by="patid",all=TRUE) ,
                           list(n_Kidney,n_heart,n_Liver,n_Lung,n_Pancreas,n_HSCT_auto,n_HSCT_allo))
colnames(df_pretx_organ_n)<-c("patid","n_kidney_pre","n_heart_pre","n_Liver_pre","n_Lung_pre","n_Pancreas_pre","n_HSCT_auto_pre","n_HSCT_allo_pre")

df_pretx_organ_n$n_preTx<-rowSums(df_pretx_organ_n[,c(2:8)])
df_pretx_organ_n$n_other_pre<-rowSums(df_pretx_organ_n[,c(4:8)])
head(df_pretx_organ_n)

Baseline_Basic <- data.frame(patid = PatientBaseline$patid,
                             centreid = PatientBaseline$centreid,
                             sex = PatientBaseline$sex,
                             ethnicity = ifelse(PatientBaseline$ethnicity == "", "Other", PatientBaseline$ethnicity),
                             outtype = PatientBaseline$out,
                             outtype2 = PatientBaseline$out2,
                             outtype3 = PatientBaseline$out32,
                             outdate = lubridate::dmy(ifelse(PatientBaseline$out=="Death",PatientBaseline$deathdate, ifelse(PatientBaseline$out=="Dropout",PatientBaseline$dropoutdate,"")))
)
dim(Baseline_Basic)
head(Baseline_Basic)




PatientBaseline2 <- Reduce(function(x,y) merge(x,y,by="patid",all=TRUE) ,
                           list(Baseline_Basic,df_pretx_organ_n[c(1,9,2,3,10)]))

head(PatientBaseline2)


PatientBaseline3 <- Reduce(function(x,y) merge(x,y,by="patid",all=TRUE) ,
                           list(PatientBaseline2,cause_death_simple))

head(PatientBaseline3)



rm( list = Filter( exists, c("df_pretx_organ","df_pretx_organ_n",
                            "n_Kidney","n_heart","n_Liver","n_Lung","n_Pancreas","n_HSCT_auto","n_HSCT_allo") ) )


#### 6.1.2 after 2008, Count number of kidney, heart and other ####
# 6.1.2.1 First, filter redudant records for simultaneous Tpx
# filter Pancreas,liver record in PatientAllCasesOrganStatus, if Kidney - Pancreas or Kidney - Liver simultaneously
# purpose: one row means one tpx case id
a<-PatientAllCasesOrganStatus[!(
  PatientAllCasesOrganStatus$organ %in% c("Kidney - Pancreas", "Kidney - Liver") &
    (PatientAllCasesOrganStatus$organtype == "Pancreas" | PatientAllCasesOrganStatus$organtype == "Liver")
),]

# 6.1.2.2 Then, count number of kidney, heart and other since 2008, until 2021.Dec
a$n_kidney_new=0
a$n_heart_new=0
a$n_other_new=0

head(a)

for (i in c(unique(a$patid))){
  
  soascaseid = a[a$patid==i,]$soascaseid
  for (j in seq(1,length(unique(soascaseid)))){
    J=unique(soascaseid)[j]
    organ=a[a$soascaseid==J,]$organtype[1]
    if(j==1){
      
      if(stringr::str_detect(organ,"Kidney")){
        a[a$soascaseid==J,]$n_kidney_new<-a[a$soascaseid==J,]$n_kidney_new+1
      } else if (stringr::str_detect(organ,"Heart")){
        a[a$soascaseid==J,]$n_heart_new<-a[a$soascaseid==J,]$n_heart_new+1
      } else {
        a[a$soascaseid==J,]$n_other_new<-a[a$soascaseid==J,]$n_other_new+1
      }
      
    } else {
      last=unique(soascaseid)[j-1]
      
      if(stringr::str_detect(organ,"Kidney")){
        a[a$soascaseid==J,]$n_kidney_new<-a[a$soascaseid==last,]$n_kidney_new[1]+1
        a[a$soascaseid==J,]$n_heart_new<-a[a$soascaseid==last,]$n_heart_new[1]
        a[a$soascaseid==J,]$n_other_new<-a[a$soascaseid==last,]$n_other_new[1]
      }else if(stringr::str_detect(organ,"Heart")){
        a[a$soascaseid==J,]$n_kidney_new<-a[a$soascaseid==last,]$n_kidney_new[1]
        a[a$soascaseid==J,]$n_heart_new<-a[a$soascaseid==last,]$n_heart_new[1]+1
        a[a$soascaseid==J,]$n_other_new<-a[a$soascaseid==last,]$n_other_new[1]
      }else{
        a[a$soascaseid==J,]$n_kidney_new<-a[a$soascaseid==last,]$n_kidney_new[1]
        a[a$soascaseid==J,]$n_heart_new<-a[a$soascaseid==last,]$n_heart_new[1]
        a[a$soascaseid==J,]$n_other_new<-a[a$soascaseid==last,]$n_other_new[1]+1
      }
    }
  }
}

PatientAllCasesOrganStatus2<-a

PatientAllCasesOrganStatus2 <- PatientAllCasesOrganStatus2 %>%
  group_by(patid) %>%
  mutate(max_n_kidney_new=max(n_kidney_new), max_n_heart_new=max(n_heart_new), max_n_other_new=max(n_other_new))


#### 6.2. merge PatientAllCasesOrganStatus, Cases, Baseline #####

# 6.2.1 merge PatientAllCasesOrganStatus and Cases by soascaseid ------------
dim(PatientAllCasesOrganStatus2) #3129, this is the number of all organ Tpx, during 2008.May-2021.Dec
dim(Cases) # 3051, this is the number of RTx during 2008.May-2020.Jan

a <- Reduce(function(x,y) merge(x,y,by="soascaseid",all.x=TRUE) ,
            list(Cases,
                 subset(PatientAllCasesOrganStatus2, select = -c(patid, centreid, tpxdate, organ))))
dim(a) # 3051
head(a)

# 6.2.2 then merge Baseline by patid ----------------------------------------
b <- Reduce(function(x,y) merge(x,y,by="patid",all=TRUE) ,
            list(a,
                 subset(PatientBaseline3, select = -c(centreid))))

dim(b) # 3051
head(b)

Patient<-b %>%
  group_by(patid,tpxdate) %>% 
  arrange(patid,tpxdate) %>%
  mutate(n_Kidney = n_kidney_pre+n_kidney_new) %>%
  mutate(n_heart = n_heart_pre+n_heart_new) %>%
  mutate(n_other = n_other_pre+n_other_new)

dim(Patient) # 3051
colnames(Patient)

Patient = Patient %>%
  select("patid","soascaseid","tpxdate",
         "out","outdate","Death cause","organtype_status","organtype_statusdate",
         "centreid","age_at_tpx","sex","ethnicity",
         "etio_0","etiodate_0","etioyy_0","etiomm_0","dialysistype","dialysisdate","dialysistime","listdate",
         "donsex","donage","dontype","sumhlamismatch","coldisch",
         "n_Kidney",         # total number of RTx during the whole life utill 2021.Dec
         "n_kidney_pre",     # total number of RTx before 2008.May
         "n_kidney_new",     # No.? RTx during 2008.May-2021.Dec
         "max_n_kidney_new", # total number of RTx during 2008.May-2021.Dec
         "max_n_heart_new", # total number of RTx during 2008.May-2021.Dec
         "max_n_other_new", # total number of RTx during 2008.May-2021.Dec
         "n_heart","n_other",# total number of heart and other organ Tpx during the whole life utill 2021.Dec
         "resec_organtype",  # "first" should be the same with n_kidney_new == 1
         "organ","organtype","pnfcause","pnfcauseoth","dgf","dgfduration","urine24","nephrectomy")


dim(Patient)#3051

# remove the 2nd or 3rd RT if one patient has many RT during observation (2008-2022)
# 80002249 has two, but the first RT was under 18 years old.
Patient <- Patient[Patient$patid ==80002249 | Patient$max_n_kidney_new==1 | (Patient$max_n_kidney_new>1 & Patient$n_kidney_new ==1),]

dim(Patient)#2992



# check, "first" should be the same with n_kidney_new == 1
table(Patient[Patient$resec_organtype == "First",]$n_kidney_new) # 2551
table(Patient$n_kidney_new)
#    1    2 
# 2991    1
table(Patient$n_kidney_pre)
#    0    1    2    3 
# 2603  335   49    5 


write.csv(Patient,"Patient.csv", row.names = FALSE, na="")
# Save an object to a file
saveRDS(Patient, file = "Patient.rds")

####### 7. outcomes ############

# Restore the object
Patient<-readRDS(file = "Patient.rds")

# 7.1 death as outcomes ----------------------------------------------
Patient$death<-NULL
Patient$death_date<-as.Date("2022-12-31")
Patient$death_type<-NULL

table(Patient$`Death cause`)
Patient$`Death cause` <- gsub(";", "", Patient$`Death cause`)

for (i in 1:nrow(Patient)){
  #print(Patient[i,]$soascaseid)
  if(is.na(Patient[i,"out"])){
    Patient[i,"death"]<-0
    Patient[i,"death_date"]<-as.Date("2022-12-31")
    Patient[i,"death_type"]<- "alive"
  }else if (Patient[i,"out"] == "Death"){
    Patient[i,"death"]<-1
    Patient[i,"death_date"]<-Patient[i,]$outdate
    Patient[i,"death_type"]<- Patient[i,]$`Death cause`
  }else if (Patient[i,"out"] == "Dropout"){
    Patient[i,"death"]<-0
    Patient[i,"death_date"]<-Patient[i,]$outdate
    Patient[i,"death_type"]<- "Dropout"
  }
}


# 7.2 graft loss as outcomes ----------------------------------------------


Patient$graft<-NULL
Patient$graft_date<-Patient$death_date # graft loss censor until death happened
Patient$graft_type<-NULL

for (i in 1:nrow(Patient)){
  #print(Patient[i,]$soascaseid)
  if (Patient[i,"organtype_status"] == "Graft loss"){
    Patient[i,"graft"]<-1
    Patient[i,"graft_date"]<-Patient[i,]$organtype_statusdate
    Patient[i,"graft_type"]<- Patient[i,]$organtype_status
  }else if (Patient[i,"organtype_status"] %in% c("PNF")){
    Patient[i,"graft"]<-1
    Patient[i,"graft_date"]<-Patient[i,]$tpxdate
    Patient[i,"graft_type"]<- "PNF"
  }else{
    Patient[i,"graft"]<-0
    #Patient[i,"graft_date"]<-as.Date("2022-12-31")
    Patient[i,"graft_type"]<- "function"
  }
}


# 7.3 MACE as outcomes (first MACE after RTx)  ----------------------------------------------

PatientsDiseases_final <- Patient[,c("patid","soascaseid","centreid","tpxdate","graft_date")]  %>%
  left_join(PatientsDiseases, by = c("patid","centreid"))


a<-PatientsDiseases_final[PatientsDiseases_final$mace_diseases !="" & PatientsDiseases_final$class  == "Other events and diseases" & PatientsDiseases_final$type == "Other",]
dim(a)
b<-PatientsDiseases_final[PatientsDiseases_final$mace_diseases !="",]
dim(b)

# only first pre-MACE

c_pre <-PatientsDiseases_final[PatientsDiseases_final$date_in <PatientsDiseases_final$tpxdate & PatientsDiseases_final$mace_diseases !="",] %>% 
  group_by(soascaseid) %>% #1061
  dplyr::slice(which.min(date_in))#614
dim(c_pre)#785

# only first post-MACE

c <-PatientsDiseases_final[PatientsDiseases_final$date_in >PatientsDiseases_final$tpxdate & PatientsDiseases_final$mace_diseases !="",] %>% 
  group_by(soascaseid) %>% #1061
  dplyr::slice(which.min(date_in))#614

dim(c)#684

c_re <-PatientsDiseases_final[PatientsDiseases_final$date_in >PatientsDiseases_final$tpxdate & PatientsDiseases_final$mace_diseases !="",] %>%
  group_by(soascaseid) %>%
  summarise(Count = n())

Patient$MACE_pre<-0
Patient$MACE<-0
Patient$MACE_date<-Patient$death_date # MACE censor until death happened
#Patient$MACE_date<-Patient$graft_date # MACE censor until the graft loss or death happened
Patient$MACE_type<-NULL



for ( i in 1:nrow(c_pre)){
  if(c_pre[i,]$mace_diseases=="Myocardial Infarction or Coronary Revascularization"){
    Patient[Patient$soascaseid==c_pre[i,]$soascaseid,"MACE_type_pre"] <-"Myocardial Infarction or Coronary Revascularization"
  } else {} 
  
  if(c_pre[i,]$mace_diseases=="Cerebrovascular Event or Revascularization"){
    Patient[Patient$soascaseid==c_pre[i,]$soascaseid,"MACE_type_pre"] <-"Cerebrovascular Event or Revascularization"
  } else {} 
  
  if(c_pre[i,]$mace_diseases=="Cardiac Arrest"){
    Patient[Patient$soascaseid==c_pre[i,]$soascaseid,"MACE_type_pre"] <-"Cardiac Arrest"
  } else {} 
  
  if(c_pre[i,]$mace_diseases=="Heart Failure"){
    Patient[Patient$soascaseid==c_pre[i,]$soascaseid,"MACE_type_pre"] <-"Heart Failure"
  } else {} 
  
  Patient[Patient$soascaseid==c_pre[i,]$soascaseid,]$MACE_pre<-1
  Patient[Patient$soascaseid==c_pre[i,]$soascaseid,"MACE_pre_date"] <-c_pre[i,]$date_in
}


for ( i in 1:nrow(c)){
  if(c[i,]$mace_diseases=="Myocardial Infarction or Coronary Revascularization"){
    Patient[Patient$soascaseid==c[i,]$soascaseid,"MACE_type"] <-"Myocardial Infarction or Coronary Revascularization"
  } else {} 
  
  if(c[i,]$mace_diseases=="Cerebrovascular Event or Revascularization"){
    Patient[Patient$soascaseid==c[i,]$soascaseid,"MACE_type"] <-"Cerebrovascular Event or Revascularization"
  } else {} 
  
  if(c[i,]$mace_diseases=="Cardiac Arrest"){
    Patient[Patient$soascaseid==c[i,]$soascaseid,"MACE_type"] <-"Cardiac Arrest"
  } else {} 
  
  if(c[i,]$mace_diseases=="Heart Failure"){
    Patient[Patient$soascaseid==c[i,]$soascaseid,"MACE_type"] <-"Heart Failure"
  } else {} 
  
  Patient[Patient$soascaseid==c[i,]$soascaseid,]$MACE<-1
  Patient[Patient$soascaseid==c[i,]$soascaseid,"MACE_date"] <-c[i,]$date_in
}

# if CV_death, then add to MACE
Patient[which(!is.na(Patient$`Death cause`) & Patient$`Death cause`=="CV_death"),]$MACE<-1

Patient$MACE_re<-0
for ( i in 1:(nrow(c_re)-1)){
    Patient[Patient$soascaseid==c_re[i,]$soascaseid,"MACE_re",] <-c_re[i,]$Count
  }

# de novo hypertension, hyperlipidemia, diabetes
c_htn <-PatientsDiseases_final[PatientsDiseases_final$date_in >PatientsDiseases_final$tpxdate & (PatientsDiseases_final$type=="HTN" |
                                                                                                   grepl("hyperten",PatientsDiseases_final$other)),] %>% 
  group_by(soascaseid) %>% #1061
  dplyr::slice(which.min(date_in))#614

Patient$Hypertension_post<-"No"
Patient$Hypertension_post_date<-NA

for ( i in 1:nrow(c_htn)){
  
  Patient[Patient$soascaseid==c_htn[i,]$soascaseid,]$Hypertension_post<-"Yes"
  Patient[Patient$soascaseid==c_htn[i,]$soascaseid,"Hypertension_post_date"] <-c_htn[i,]$date_in
}

c_hlp <-PatientsDiseases_final[PatientsDiseases_final$date_in >PatientsDiseases_final$tpxdate & (PatientsDiseases_final$type=="Hyperlipidemia" |  grepl("hyperlip",PatientsDiseases_final$other)),] %>% 
  group_by(soascaseid) %>% #1061
  dplyr::slice(which.min(date_in))#614

Patient$Hyperlipidemia_post<-"No"
Patient$Hyperlipidemia_post_date<-NA

for ( i in 1:nrow(c_hlp)){
  
  Patient[Patient$soascaseid==c_hlp[i,]$soascaseid,]$Hyperlipidemia_post<-"Yes"
  Patient[Patient$soascaseid==c_hlp[i,]$soascaseid,"Hyperlipidemia_post_date"] <-c_hlp[i,]$date_in
}


c_dm <-PatientsDiseases_final[PatientsDiseases_final$date_in >PatientsDiseases_final$tpxdate & (PatientsDiseases_final$type %in% c("Treated DM","DM type1","DM type2 treated")),] %>% 
  group_by(soascaseid) %>% #1061
  dplyr::slice(which.min(date_in))#614

Patient$Diabetes_post<-"No"
Patient$Diabetes_post_date<-NA

for ( i in 1:nrow(c_dm)){
  
  Patient[Patient$soascaseid==c_dm[i,]$soascaseid,]$Diabetes_post<-"Yes"
  Patient[Patient$soascaseid==c_dm[i,]$soascaseid,"Diabetes_post_date"] <-c_dm[i,]$date_in
}

# if received previous kidney/heart/other organ tpx.
Patient$kidney_tpx<-ifelse(Patient$n_Kidney==1,"No","Yes")
Patient$heart_tpx<-ifelse(Patient$n_heart==0,"No","Yes")
Patient$other_tpx<-ifelse(Patient$n_other==0,"No","Yes")


names(Patient)

Patient2<-Patient[,c("patid","soascaseid","centreid","tpxdate",
                    "death_date","death","death_type","Death cause",
                    "graft_date","graft","graft_type",
                    "MACE_date","MACE","MACE_type","MACE_re",
                    "MACE_pre_date","MACE_pre","MACE_type_pre",
                    "Hypertension_post","Hypertension_post_date",
                    "Hyperlipidemia_post","Hyperlipidemia_post_date",
                    "Diabetes_post","Diabetes_post_date",
                    "age_at_tpx","sex","ethnicity",
                    "etio_0","etiodate_0","etioyy_0","etiomm_0",
                    "listdate","dialysisdate","dialysistype",
                    "donsex","donage","dontype","sumhlamismatch",
                    "coldisch","dgf","dgfduration","urine24",
                    "kidney_tpx","heart_tpx","other_tpx","n_Kidney","max_n_kidney_new")]

dim(Patient2) #2992 47

Patient2[Patient2$MACE==1 & is.na(Patient2$MACE_type),"MACE_type"]<-"CV_death"


#### 8. follow_up for physics, blood, smoking and disease ######

source(paste0(code_path,"2.follow_up_phy.R")) # get Physical_followup_final.rds
source(paste0(code_path,"2.follow_up_lab.R")) # get LabChemistry_followup_final.rds
source(paste0(code_path,"2.follow_up_smk.R")) # get Smoking_followup_final.rds
source(paste0(code_path,"2.follow_up_disease.R")) # get PatientsDiseases_followup_final.rds
source(paste0(code_path,"2.follow_up_proteinuria.R")) # get Proteinuria_match_final.rds

#### 9. add baseline of physics, blood, smoking and disease ######

Patient3 <- Patient2 %>%
  left_join(Physical_baseline_final[,c("patid","soascaseid","centreid","tpxdate","graft_date",
                                       "assdate","BMI","sbp","dbp")] , by = c("patid","soascaseid","centreid","tpxdate","graft_date"))
dim(Patient3)

Patient3 <- Patient3 %>%
  left_join(LabChemistry_baseline_final[,c("patid","soascaseid","centreid","tpxdate","graft_date",
                                       "crea","chol","ldlchol","hdlchol","hba1c")] , by = c("patid","soascaseid","centreid","tpxdate","graft_date"))


Patient3 <- Patient3 %>%
  left_join(Smoking_baseline[,c("patid","soascaseid","centreid","tpxdate","graft_date",
                                           "smoking_0")] , by = c("patid","soascaseid","centreid","tpxdate","graft_date"))

dim(Patient3)

Patient4<-Patient3

######### 10. add disease history at baseline ###############

a<-PatientsDiseases_baseline

Patient4$Hypertension<-"No"
Patient4$Hyperlipidemia<-"No"
Patient4$Diabetes<-"No"

# Patient4$MACE_history<-"No"
Patient4$Myocardial_Infarction<-"No"
Patient4$Cerebrovascular_Event<-"No"
Patient4$Cardiac_Arrest<-"No"
Patient4$Heart_Failure<-"No"


# below is trying to search Hypertension, Hyperlipidemia, Diabetes, 4 types of MACE from PatientsDiseases_bl
for ( i in c(unique(a$soascaseid))){
  
  if(any(a[a$soascaseid==i,]$type=="HTN") | any(grepl("hyperten",a[a$soascaseid==i,]$other))){
    Patient4[Patient4$soascaseid==i,]$Hypertension<-"Yes"
    
  } else {} 
  
  if(any(a[a$soascaseid==i,]$type=="Hyperlipidemia")){
    Patient4[Patient4$soascaseid==i,]$Hyperlipidemia<-"Yes"
  } else {}
  
  if(any(a[a$soascaseid==i,]$type %in% c("Treated DM","DM type1","DM type2 treated"))){
    Patient4[Patient4$soascaseid==i,]$Diabetes<-"Yes"
  } else {} 
  
  if(any(a[a$soascaseid==i,]$mace_diseases=="Myocardial Infarction or Coronary Revascularization")){
    Patient4[Patient4$soascaseid==i,]$Myocardial_Infarction<-"Yes"
    # Patient4[Patient4$soascaseid==i,]$MACE_history<-"Yes"

  } else {} 
  
  if(any(a[a$soascaseid==i,]$mace_diseases=="Cerebrovascular Event or Revascularization")){
    Patient4[Patient4$soascaseid==i,]$Cerebrovascular_Event<-"Yes"
    # Patient4[Patient4$soascaseid==i,]$MACE_history<-"Yes"
  } else {} 
  
  if(any(a[a$soascaseid==i,]$mace_diseases=="Cardiac Arrest")){
    Patient4[Patient4$soascaseid==i,]$Cardiac_Arrest<-"Yes"
    # Patient4[Patient4$soascaseid==i,]$MACE_history<-"Yes"
  } else {} 
  
  if(any(a[a$soascaseid==i,]$mace_diseases=="Heart Failure")){
    Patient4[Patient4$soascaseid==i,]$Heart_Failure<-"Yes"
    # Patient4[Patient4$soascaseid==i,]$MACE_history<-"Yes"
  } else {} 
  
}


c_pre <-a[a$date_in <a$tpxdate & a$mace_diseases !="",] %>% 
  group_by(soascaseid) %>% #1061
  dplyr::slice(which.min(date_in))#614
dim(c_pre) #798

Patient4$MACE_pre<-NA
Patient4$MACE_pre_date<-NA
Patient4$MACE_pre_type<-NA

for ( i in 1:nrow(c_pre)){

  Patient4[Patient4$soascaseid==c_pre[i,]$soascaseid,]$MACE_pre<-"Yes"
  Patient4[Patient4$soascaseid==c_pre[i,]$soascaseid,]$MACE_pre_type<-c_pre[i,]$mace_diseases
  Patient4[Patient4$soascaseid==c_pre[i,]$soascaseid,"MACE_pre_date"] <-c_pre[i,]$date_in
}


c_pre_re <-PatientsDiseases_baseline[PatientsDiseases_baseline$date_in <PatientsDiseases_baseline$tpxdate & PatientsDiseases_baseline$mace_diseases !="",] %>%
  group_by(soascaseid) %>%
  summarise(Count = n())


Patient4$MACE_pre_re<-0
for ( i in 1:(nrow(c_pre_re)-1)){
  Patient4[Patient4$soascaseid==c_pre_re[i,]$soascaseid,"MACE_pre_re",] <-c_pre_re[i,]$Count
}

summary(Patient4)

######## 11. deal with missing and unknown ##############
Patient5<-Patient4


#Patient5[Patient5=="Global consent refused"]<-NA
Patient5[Patient5==""]<-NA

table(Patient5$ethnicity)
table(is.na(Patient5$ethnicity))
Patient5$ethnicity<-ifelse(Patient5$ethnicity %in% c("Global consent refused","Unknown"),NA,Patient5$ethnicity)

table(Patient5$etio_0)
table(is.na(Patient5$etio_0))
Patient5$etio_0<-ifelse(Patient5$etio_0 %in% c("Global consent refused","Cause unknown","Unknown")| is.na(Patient5$etio_0)| Patient5$etio_0=="" ,NA,Patient5$etio_0)

table(Patient5$dialysistype)
table(is.na(Patient5$dialysistype))
Patient5$dialysistype<-ifelse(Patient5$dialysistype %in% c("Global consent refused") | is.na(Patient5$dialysistype),NA,Patient5$dialysistype)

table(is.na(Patient5$smoking_0))

Patient5<-Patient5%>%
  rename(smoking = smoking_0)
table(is.na(Patient5$smoking))


Patient5$smoking<-ifelse(Patient5$smoking %in% c("Unknown") | is.na(Patient5$smoking),NA,Patient5$smoking)

table(Patient5$urine24)
table(is.na(Patient5$urine24))
Patient5$urine24<-ifelse(Patient5$urine24 %in% c("Not done","Unknown") | is.na(Patient5$urine24),NA,Patient5$urine24)

table(Patient5$donsex)
table(is.na(Patient5$donsex))

Patient5$donsex<-ifelse(Patient5$donsex %in% c("Global consent refused")| is.na(Patient5$donsex),NA,Patient5$donsex)

table(Patient5$donage)
table(is.na(Patient5$donage))

Patient5$donage<-ifelse( is.na(Patient5$donage),NA,Patient5$donage)

Patient5$sumhlamismatch<-ifelse(Patient5$sumhlamismatch<0,NA,Patient5$sumhlamismatch)

Patient5$dgfduration<-ifelse(Patient5$dgfduration<0,NA,Patient5$dgfduration)



table(is.na(Patient5$BMI))#95
table(is.na(Patient5$sbp))#175
table(is.na(Patient5$dbp))#176
table(is.na(Patient5$crea))#18
table(is.na(Patient5$chol))#450
table(is.na(Patient5$ldlchol))#574
table(is.na(Patient5$hdlchol))#490
table(is.na(Patient5$hba1c))#1603

table(Patient5$Hypertension)

Patient5$CRS_type<-ifelse(Patient5$MACE_pre_re==0,"ESRD",
                          ifelse(Patient5$etioyy_0<year(Patient5$MACE_pre_date),"renocardiac",
                                 ifelse(Patient5$etioyy_0>year(Patient5$MACE_pre_date),"cardiorenal","simu")))



Patient5$CRS_type<-ifelse(is.na(Patient5$CRS_type),NA,Patient5$CRS_type)

table(Patient5$CRS_type)
# cardiorenal        ESRD renocardiac        simu     unknown 
# 126        2254         498         102          71

# +-1 year
# cardiorenal        ESRD renocardiac        simu     unknown 
# 101        2254         454         171          71 
Patient5$MACE_history<-ifelse(Patient5$MACE_pre_re==0,"No","Yes")

Patient5$new_MACE<-ifelse(Patient5$MACE_pre_re==0 & Patient5$MACE==1,"Yes","No")
Patient5$new_Hypertension<-ifelse(Patient5$Hypertension=="No" & Patient5$Hypertension_post=="Yes","Yes","No")
Patient5$new_Hyperlipidemia<-ifelse(Patient5$Hyperlipidemia=="No" & Patient5$Hyperlipidemia_post=="Yes","Yes","No")
Patient5$new_Diabetes<-ifelse(Patient5$Diabetes=="No" & Patient5$Diabetes_post=="Yes","Yes","No")

Patient5$MACE_pre <- ifelse(!is.na(Patient5$MACE_pre),"Yes","No")

library(gtsummary)

as.data.frame(Patient5) %>%
  dplyr::select(c(death,death_type,
    `Death cause`,graft,graft_type,
    MACE,MACE_type,MACE_re,
    age_at_tpx,sex,ethnicity,etio_0,dialysistype,donsex,donage,
    dontype,sumhlamismatch,coldisch,urine24,dgf,dgfduration,
    kidney_tpx,heart_tpx,other_tpx,
    BMI,sbp,dbp,
    crea,chol,ldlchol,hdlchol,hba1c,
    smoking,Hypertension,Hyperlipidemia,Diabetes,
    Myocardial_Infarction,Cerebrovascular_Event,Cardiac_Arrest,Heart_Failure,
    CRS_type,MACE_pre,MACE_pre_type,MACE_pre_re,
    Hypertension_post,Hyperlipidemia_post,Diabetes_post,
    new_Hypertension,new_Hyperlipidemia,new_Diabetes,new_MACE
  )) %>%
  tbl_summary(
    by = MACE_pre,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),#all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 2#,
    missing = "NA"#,
  ) %>%
add_p(list(all_continuous() ~ "t.test", all_categorical() ~ "fisher.test"))#add_p()#add_p(test.args = all_tests("fisher.test") ~ list(workspace=5e9))#add_p()

dim(Patient5)
write.csv(Patient5,"Patient5.csv", row.names = FALSE, na="")
# Save an object to a file
saveRDS(Patient5, file = "Patient5.rds")

####### 12. structure outcomes in time & status format ############

# Restore the object
Patient5<-readRDS(file = "Patient5.rds")
Patient5$smoking<-ifelse(Patient5$smoking %in% c("Unknown") | is.na(Patient5$smoking),NA,Patient5$smoking)

table(Patient5$smoking)

Patient_bl <- Patient5%>%
  arrange("Patid","tpxdate") %>%
  rowwise() %>%
  mutate(Year = year(tpxdate), .before = "tpxdate")  %>%
  mutate(list_time = (tpxdate - listdate)/365.25, .before = "listdate") %>%
  mutate(dialysis_time = ifelse(dialysisdate == as.Date("9999-01-01"),0,
                                ifelse(dialysisdate == as.Date("7777-01-01"),NA,
                                       ifelse(dialysisdate == as.Date("5555-01-01"),NA,
                                              as.numeric(tpxdate - dialysisdate)/365.25))), .before = "dialysisdate") %>%
  mutate(time_MACE = as.numeric(MACE_date - tpxdate )/365.25, .before = "MACE_date") %>%  
  mutate(time_graft = as.numeric(graft_date - tpxdate)/365.25, .before = "graft_date") %>%
  mutate(time_death = as.numeric(death_date - tpxdate)/365.25, .before = "death_date") %>%
  dplyr::rename(status_death = death)%>%
  dplyr::rename(status_MACE = MACE)%>%
  dplyr::rename(status_graft = graft)%>%
  mutate(time_MACE_pre = as.numeric(tpxdate - MACE_pre_date)/365.25, .before = "MACE_pre_date") %>%
  mutate(time_MARE_pre = as.numeric(tpxdate - etiodate_0)/365.25, .before = "etiodate_0") %>%
  mutate(time_Hypertension_post = as.numeric(Hypertension_post_date - tpxdate)/365.25, .before = "Hypertension_post_date") %>%
  mutate(time_Hyperlipidemia_post = as.numeric(Hyperlipidemia_post_date - tpxdate)/365.25, .before = "Hyperlipidemia_post_date") %>%
  mutate(time_Diabetes_post = as.numeric(Diabetes_post_date - tpxdate)/365.25, .before = "Diabetes_post_date")

Patient_bl <-Patient_bl %>%
  dplyr::select(-c(#MACE_pre_date,
            etioyy_0,etiomm_0,
            Hypertension_post_date,Hyperlipidemia_post_date,Diabetes_post_date))

########## 13. save Patient baseline characteristics ########

write.csv(Patient_bl,"Patient_bl.csv", row.names = FALSE, na="")
saveRDS(Patient_bl, file = "Patient_bl.rds")

######### 14. Patient baseline + Drug ########

# For DrugsInduction, only take those drugstartdate <= tpxdate+14
# For DrugsImmunoSuppression_match, only take those drugstartdate <= tpxdate+14
# For DrugsOther, no restriction

source(paste0(code_path,"2.follow_up_drug.R"))
# get baseline + drug,  saveRDS(Patient_bl2, file = "Patient_bl2.rds")

######### 15. Patient baseline + Drug + echo ########

source(paste0(code_path,"3.follow_up_echo.R"))
# get baseline + drug + echo (baseline), saveRDS(Patient, file = "Patient.rds")
# get whole echo dataset (baseline + follow-up), saveRDS(Patient, file = "echo_followup_final.rds")

