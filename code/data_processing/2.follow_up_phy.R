
#### 1. Physical (PatientFollowUp) #####

Physical_FollowUp <- read.csv (paste0(data_path,"Data_20231214/FUP121_PatientFollowUp_14DEC23.csv"), header=TRUE, sep = ",", stringsAsFactors=FALSE) 
dim(Physical_FollowUp) # 21575, including other organ transplantations
length(unique(Physical_FollowUp$patid)) # 2992

# 1.1 time format and remove those rows that are completely empty ---------------------------------------------------------

Physical_FollowUp = Physical_FollowUp %>% 
  mutate(across(contains(c('assdate'), ignore.case = TRUE), lubridate::dmy)) %>%
  select("patid","assdate","assperiod","weight","height","bmi","sbp","dbp") %>%
  filter(!is.na(weight) |!is.na(height) |!is.na(bmi) |!is.na(sbp) |!is.na(dbp))

dim(Physical_FollowUp)# 23473
head(Physical_FollowUp)


# 1.2 merge baseline and follow-up ----------------------------------------

Physical_Baseline = PatientBaseline[,c("patid","assdate","assperiod","weight","height","bmi","sbp","dbp")]
head(Physical_Baseline)

Physical = rbind(Physical_Baseline,Physical_FollowUp)
Physical = arrange(Physical, patid, assdate)
head(Physical)

ls()
rm( list = Filter( exists, c("Physical_Baseline","Physical_FollowUp") ) )
ls()

Physical[Physical$height<0 & !is.na(Physical$height),]$height<-NA
Physical[Physical$weight<0 & !is.na(Physical$weight),]$weight<-NA
Physical[Physical$bmi<0 & !is.na(Physical$bmi),]$bmi<-NA
Physical[Physical$sbp<0 & !is.na(Physical$sbp),]$sbp<-NA
Physical[Physical$dbp<0 & !is.na(Physical$dbp),]$dbp<-NA

# 1.3 manually correct height and weight ----------------------------------------

Physical[Physical$patid==80006954 & Physical$assperiod==36,]$weight<-80.3
Physical[Physical$patid==80006954 & Physical$assperiod==36,]$height<-179
Physical[Physical$patid==80006954 & Physical$assperiod==48,]$height<-179
Physical[Physical$patid==80006954 & Physical$assperiod==60,]$height<-179

Physical[Physical$patid==80008668 & Physical$assperiod==48,]$height<-165

Physical[Physical$patid==80007464 & Physical$assperiod==0,]$height<-180
Physical[Physical$patid==80007464 & Physical$assperiod==48,]$height<-180

Physical[Physical$patid==80009757 & Physical$assperiod==36,]$weight<-94
Physical[Physical$patid==80009757 & Physical$assperiod==36,]$height<-175

Physical[Physical$patid==80001464 & Physical$assperiod==108,]$height<-183

Physical[Physical$patid==80008896 & Physical$assperiod==12,]$height<-168


# 1.4 first add median height  -----------------------------------------------
# correct the height, because later on some height records will be filter out if it is after graft loss

Physical2 <- Physical %>%
  group_by(patid) %>% 
  mutate(median_height = median(height,na.rm = T), .after = "height") #%>%
  # mutate(median_height = ifelse(is.na(median_height), 
  #                               median(median_height, na.rm = T), 
  #                               median_height))


# 1.5 combine with Patient ------------------------------------------------------------

dim(Physical2[Physical2$patid %in% c(Patient$patid),])# 19459

Physical_match <- Patient[,c("patid","soascaseid","centreid","tpxdate","graft_date")]  %>%
  left_join(Physical2, by = c("patid"))

Physical_match$note<-""

dim(Physical) # 24266
dim(Physical_match)# 24845

# 1.6 filter assdate <= graft_date ------------------------------------------------------------
# assume assdate is correct, but period might be wrong
Physical_match2 <- Physical_match %>%
  filter(assdate <= graft_date) %>%
  filter(assdate >= tpxdate-14) 
dim(Physical_match2)# 26099

length(unique(Physical_match$soascaseid))
length(unique(Physical_match2$soascaseid))

# 1.6 manually fix assdate ------------------------------------------------------
# check dates based on assdate-tpxdate+assperoid*30.5

Physical_match3 <- Physical_match2

change_date_manually <- function(df,patid,colname_look,value,colname_change,old_date,new_assdate){
  df[df$patid == patid & df[,colname_look] == value, ]$note <- paste0(colname_change,":",old_date,"_manual",new_assdate,";")
  df[df$patid == patid & df$assperiod == value, colname_change] <- new_assdate
  return(df)
}

Physical_match3 <- change_date_manually(Physical_match3,80003560,"assperiod",60,"assdate",as.Date("2008-01-23"),as.Date("2018-01-23")) 
# there are more...
# not sure for the ones which gaps are aound one year
Physical_match3 <- change_date_manually(Physical_match3,80000191,"assperiod",108,"assdate",as.Date("2016-03-15"),as.Date("2017-03-15")) 
# the assperiod for 80003825 after 60 months is wrong, it shifted 12 months. But it is okay, we do not use assperiod anyway.
Physical_match3 <- change_date_manually(Physical_match3,80000223,"assperiod",96,"assdate",as.Date("2015-05-20"),as.Date("2016-06-06")) 
Physical_match3 <- change_date_manually(Physical_match3,80003552,"assperiod",96,"assdate",as.Date("2014-04-14"),as.Date("2015-04-14")) 
# the assperiod for 80001114 after 108, 120, 132 months is wrong, it shifted 12 months.
# the assperiod for 80002370 after 96, 108 months is wrong, it shifted 12 months.
Physical_match3 <- change_date_manually(Physical_match3,80001199,"assperiod",36,"assdate",as.Date("2012-02-14"),as.Date("2013-02-14")) 
Physical_match3 <- change_date_manually(Physical_match3,80000396,"assperiod",24,"assdate",as.Date("2009-11-11"),as.Date("2010-11-11")) 
Physical_match3 <- change_date_manually(Physical_match3,80007559,"assperiod",48,"assdate",as.Date("2020-02-25"),as.Date("2021-02-25")) 
Physical_match3 <- change_date_manually(Physical_match3,80000612,"assperiod",24,"assdate",as.Date("2010-04-14"),as.Date("2011-04-14")) 
Physical_match3 <- change_date_manually(Physical_match3,80001843,"assperiod",36,"assdate",as.Date("2013-03-14"),as.Date("2014-03-14")) 
Physical_match3 <- change_date_manually(Physical_match3,80000688,"assperiod",36,"assdate",as.Date("2011-06-24"),as.Date("2012-06-24")) 
# # 80002383 last assperiod is 48 months, which is a bit early, but it is okay because graft loss is before 48 months.
# # 80001741 death date 2012-12-21, assdate is 2012-01-25 at 24 months, which is supposed to be at 2012-12-08, is it correct? ps: assdate is 2012-01-20 at 12 months.
# # there are more ...
Physical_match3 <- change_date_manually(Physical_match3,80005295,"assperiod",0,"assdate",as.Date("2017-02-22"),as.Date("2018-02-22")) 

# can not because not data, 
# "RS-2009-0089" "RS-2009-0418" "RS-2010-0058" "RS-2010-0124" "RS-2011-0315" "RS-2010-0179" "RS-2010-0250" "RS-2013-0116"
# [10] "RS-2013-0075" "RS-2011-0265" "RS-2012-0303" "RS-2014-0029" "RS-2015-0182" "RS-2015-0403"


# 1.7 fix abnormal values -----------------------------------------------
# right now, only did for height and BMI
# 80006954
Physical_match4 <- Physical_match3 %>%
  mutate(BMI = round(weight/((median_height/100)**2),1), .after= bmi )


# 1.8 duplicated value ---------------------------
df_duplicated<-Physical_match4[(duplicated(Physical_match4[c("soascaseid","assdate")]) | duplicated(Physical_match4[c("soascaseid","assdate")], fromLast = TRUE)), ]

dim(df_duplicated)#75

duplicated_rows <- df_duplicated %>%
  select(-assperiod) %>% # Exclude the unique_column
  duplicated() | df_duplicated %>%
  select(-assperiod) %>% # Exclude the unique_column
  duplicated(fromLast = TRUE)

# Getting the rows that are duplicated (ignoring 'unique_column')
df_duplicated2 <- df_duplicated[-which(duplicated_rows), ]#
dim(df_duplicated2) #49

dim(Physical_match4)#26099
Physical_match5 =Physical_match4[!duplicated(Physical_match4[c("soascaseid","assdate")]),]
dim(Physical_match5)#26060

# 1.9 check missing value ---------------------------

# missing value in total
table(!is.na(Physical_match5$weight)) # 1587
table(!is.na(Physical_match5$median_height)) # 10

table(!is.na(Physical_match5$sbp)) # 1876 
table(!is.na(Physical_match5$dbp)) # 1890


# 1.10 save data ---------------------------

table(Physical_match5$assdate==Physical_match5$tpxdate) #3028 on tpxdate
a = Physical_match5[Physical_match5$assdate>=Physical_match5$tpxdate-14 & Physical_match5$assdate<=Physical_match5$tpxdate,]
a %>% arrange(tpxdate)

a[duplicated(a$soascaseid) | duplicated(a$soascaseid,fromLast = TRUE), ]
b =a[!duplicated(a$soascaseid),]

dim(Patient2)#3051
dim(a)#3034
dim(b)#3034

length(unique(Physical_match5$soascaseid))#3051
length(unique(b$soascaseid))#3034

setdiff(unique(Physical_match5$soascaseid),b$soascaseid)
# 17 patients do not have baseline


Physical_baseline_final<-a#3034
Physical_followup_final<-Physical_match5[Physical_match5$soascaseid %in% b$soascaseid,] #27092


names(Physical_baseline_final)

# library(plotly)
# plot_ly(Physical_followup_final, x = ~BMI, type = "histogram", name = "Histogram", nbinsx = 30) 
# plot_ly(Physical_followup_final, x = ~sbp, type = "histogram", name = "Histogram", nbinsx = 30) 
# plot_ly(Physical_followup_final, x = ~dbp, type = "histogram", name = "Histogram", nbinsx = 30) 


write.csv(Physical_followup_final,"Physical_followup_final.csv", row.names = FALSE, na="")
saveRDS(Physical_followup_final, file = "Physical_followup_final.rds")
