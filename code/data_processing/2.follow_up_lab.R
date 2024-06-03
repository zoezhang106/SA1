#### 2. LabChemistry #####

LabChemistry <- read.csv (paste0(data_path,"Data_20231214/FUP121_LabChemistry_14DEC23.csv"), header=TRUE, sep = ",", stringsAsFactors=FALSE) 
dim(LabChemistry) # 26838
length(unique(LabChemistry$patid)) # 2992


LabChemistry[LabChemistry$crea<0 & !is.na(LabChemistry$crea),]$crea<-NA
LabChemistry[LabChemistry$chol<0 & !is.na(LabChemistry$chol),]$chol<-NA
LabChemistry[LabChemistry$ldlchol<0 & !is.na(LabChemistry$ldlchol),]$ldlchol<-NA
LabChemistry[LabChemistry$hdlchol<0 & !is.na(LabChemistry$hdlchol),]$hdlchol<-NA
LabChemistry[LabChemistry$hba1c<0 & !is.na(LabChemistry$hba1c),]$hba1c<-NA



# 2.1 time format and remove those rows that are completely empty---------------------------------------------------------

LabChemistry = LabChemistry %>% 
  mutate(across(contains(c('assdate','creatinindate','lipiddate','hba1cdate'), ignore.case = TRUE), lubridate::dmy)) %>%
  select("patid","assdate","assperiod","crea","chol","ldlchol","hdlchol","hba1c","creatinindate","lipiddate","hba1cdate") %>%
  filter(!is.na(crea) |!is.na(chol) |!is.na(ldlchol) |!is.na(hdlchol) |!is.na(hba1c))

dim(LabChemistry) # 25904    11
head(LabChemistry)


# 2.2 combine with Patient ------------------------------------------------------------

dim(LabChemistry[LabChemistry$patid %in% c(Patient$patid),])# 19459

LabChemistry_match <- Patient[,c("patid","soascaseid","centreid","tpxdate","graft_date")]  %>%
  left_join(LabChemistry, by = c("patid")) 

dim(LabChemistry) # 25904
dim(LabChemistry_match)# 26513

LabChemistry_match$note<-""

# 2.2 filter assdate <= graft_date ------------------------------------------------------------
# assume assdate is correct, but period might be wrong
# calculate new assperiod and ass_days for 3 measure dates

LabChemistry_match2 <- LabChemistry_match %>%
  rowwise() %>%
  filter(assdate <= graft_date) %>%
  filter(assdate >= tpxdate-14) 

dim(LabChemistry_match2)# 25558


# 2.3 check duplicate 3 measure date ---------------------------------------------------

# 2.3.1 check completely duplicate records ---------------------------------------------------
name<-c("patid","soascaseid", "crea", "chol", "ldlchol", "hdlchol", "hba1c", "creatinindate",  "lipiddate",  "hba1cdate")
LabChemistry_dup <-LabChemistry_match2[duplicated(LabChemistry_match2[,name]) |duplicated(LabChemistry_match2[,name],fromLast=TRUE),]
LabChemistry_dup # new 36
# 2.3.2 remove completely duplicate records ---------------------------------------------------
# why it happened? it does not matter which records we keep or delete, since we do not use assdate and period anyway

dim(LabChemistry_match2) # 25558   # 23908    16
LabChemistry_match2 <- LabChemistry_match2[!duplicated(LabChemistry_match2[,name]),]
dim(LabChemistry_match2) # 25540   # 23889    16

# 2.3.3 check duplicate measure date but with different values ---------------------------------------------------
# need to decide individually, which is correct
# (1) creatinindate
LabChemistry_dup_creatinindate <-LabChemistry_match2[ !is.na(LabChemistry_match2$creatinindate) & 
                                                        (!duplicated(LabChemistry_match2[,c("patid","crea")])) & 
                                                        (duplicated(LabChemistry_match2[,c("patid","creatinindate")]) | 
                                                           duplicated(LabChemistry_match2[,c("patid","creatinindate")],fromLast=TRUE)),]
LabChemistry_dup_creatinindate <-LabChemistry_dup_creatinindate[ !is.na(LabChemistry_dup_creatinindate$creatinindate) & (duplicated(LabChemistry_dup_creatinindate[,c("patid","creatinindate")]) | duplicated(LabChemistry_dup_creatinindate[,c("patid","creatinindate")],fromLast=TRUE)),]
LabChemistry_dup_creatinindate #25


# (2) lipiddate
LabChemistry_dup_lipiddate <-LabChemistry_match2[ !is.na(LabChemistry_match2$lipiddate) & 
                                                    (!duplicated(LabChemistry_match2[,c("patid","chol")]) | 
                                                       !duplicated(LabChemistry_match2[,c("patid","ldlchol")]) | 
                                                       !duplicated(LabChemistry_match2[,c("patid","hdlchol")])) & 
                                                    (duplicated(LabChemistry_match2[,c("patid","lipiddate")]) | 
                                                       duplicated(LabChemistry_match2[,c("patid","lipiddate")],fromLast=TRUE)),]
LabChemistry_dup_lipiddate # new 427

LabChemistry_dup_lipiddate <-LabChemistry_dup_lipiddate[ !is.na(LabChemistry_dup_lipiddate$lipiddate) & (duplicated(LabChemistry_dup_lipiddate[,c("patid","lipiddate")]) | duplicated(LabChemistry_dup_lipiddate[,c("patid","lipiddate")],fromLast=TRUE)),]
LabChemistry_dup_lipiddate #27

# (3) hba1cdate
LabChemistry_dup_hba1cdate <-LabChemistry_match2[ !is.na(LabChemistry_match2$hba1cdate) & 
                                                    (!duplicated(LabChemistry_match2[,c("patid","hba1c")])) & 
                                                    (duplicated(LabChemistry_match2[,c("patid","hba1cdate")]) | 
                                                       duplicated(LabChemistry_match2[,c("patid","hba1cdate")],fromLast=TRUE)),]
LabChemistry_dup_hba1cdate # new 588

LabChemistry_dup_hba1cdate <-LabChemistry_dup_hba1cdate[ !is.na(LabChemistry_dup_hba1cdate$hba1cdate) & (duplicated(LabChemistry_dup_hba1cdate[,c("patid","hba1cdate")]) | duplicated(LabChemistry_dup_hba1cdate[,c("patid","hba1cdate")],fromLast=TRUE)),]
LabChemistry_dup_hba1cdate #6

# assdate is different, but the lab date is the same. It is ok, if you separate the table and match the date with each corresponding the value

# 2.4 abnormal 3 measure dates ----------------------------------------

# 2.4.1 check abnormal dates ----------------------------------------------

# compare the three measure date and then compare with the assdate(could be the assdate is wrong or the three date is wrong)
LabChemistry_match3 <- LabChemistry_match2  %>%
  mutate(date_crea_lipid = creatinindate - lipiddate, .after = "creatinindate") %>% 
  mutate(date_lipid_hba1c = lipiddate - hba1cdate, .after = "lipiddate") %>%   
  mutate(date_crea_hba1c = creatinindate - hba1cdate, .after = "hba1cdate")

# 2.4.2 fix manually ----------------------------------------------

# only correct the obvious one manually, there are more...

change_date_manually <- function(df,patid,colname_look,value,colname_change,old_date,new_assdate){
  df[df$patid == patid & df[,colname_look] == value, ]$note <- paste0(colname_change,":",old_date,"_manual",new_assdate,";")
  df[df$patid == patid & df$assperiod == value, colname_change] <- new_assdate
  return(df)
}
# change creatinindate
LabChemistry_match3 <- change_date_manually(LabChemistry_match3,80003652,"assperiod",84,"creatinindate",as.Date("2002-06-29"),as.Date("2020-06-30")) 
LabChemistry_match3 <- change_date_manually(LabChemistry_match3,80009042,"assperiod",24,"creatinindate",as.Date("2002-08-26"),as.Date("2020-08-26")) 
LabChemistry_match3 <- change_date_manually(LabChemistry_match3,80002071,"assperiod",60,"creatinindate",as.Date("2013-06-13"),as.Date("2016-06-13")) 
LabChemistry_match3 <- change_date_manually(LabChemistry_match3,80003802,"assperiod",6,"creatinindate",as.Date("2013-03-10"),as.Date("2014-03-10")) 
LabChemistry_match3 <- change_date_manually(LabChemistry_match3,80003601,"assperiod",48,"creatinindate",as.Date("2017-03-08"),as.Date("2014-03-08"))
LabChemistry_match3 <- change_date_manually(LabChemistry_match3,80001138,"assperiod",36,"creatinindate",as.Date("2012-01-18"),as.Date("2013-01-18"))
LabChemistry_match3 <- change_date_manually(LabChemistry_match3,80002385,"assperiod",108,"creatinindate",as.Date("2020-01-08"),as.Date("2021-01-08")) 
LabChemistry_match3 <- change_date_manually(LabChemistry_match3,80002385,"assperiod",108,"creatinindate",as.Date("2020-01-08"),as.Date("2021-01-08")) 
LabChemistry_match3 <- change_date_manually(LabChemistry_match3,80009817,"assperiod",0,"creatinindate",as.Date("2020-02-02"),as.Date("2019-02-02"))

#... there are more

# change lipiddate
LabChemistry_match3 <- change_date_manually(LabChemistry_match3,80000240,"assperiod",144,"lipiddate",as.Date("2000-05-13"),as.Date("2020-05-13")) 
LabChemistry_match3 <- change_date_manually(LabChemistry_match3,80002987,"assperiod",108,"lipiddate",as.Date("2002-08-09"),as.Date("2021-08-09")) 
LabChemistry_match3 <- change_date_manually(LabChemistry_match3,80008821,"assperiod",0,"lipiddate",as.Date("2007-10-08"),as.Date("2017-10-08"))   
LabChemistry_match3 <- change_date_manually(LabChemistry_match3,80004245,"assperiod",6,"lipiddate",as.Date("2004-08-12"),as.Date("2014-08-12"))  
LabChemistry_match3 <- change_date_manually(LabChemistry_match3,80009013,"assperiod",0,"lipiddate",as.Date("2009-10-14"),as.Date("2017-10-14"))   
LabChemistry_match3 <- change_date_manually(LabChemistry_match3,80000268,"assperiod",96,"lipiddate",as.Date("2013-10-04"),as.Date("2015-10-04")) 
#... there are more, for the abnormal date at baseline, is it actually true?

# change hba1cdate
LabChemistry_match3 <- change_date_manually(LabChemistry_match3,80004614,"assperiod",60,"hba1cdate",as.Date("2020-05-15"),as.Date("2019-05-15")) # 2020-05-15

# change assdate
LabChemistry_match3 <- change_date_manually(LabChemistry_match3,80003719,"assperiod",96,"assdate",as.Date("2013-08-05"),as.Date("2021-08-05")) 
LabChemistry_match3 <- change_date_manually(LabChemistry_match3,80009411,"assperiod",6,"assdate",as.Date("2019-02-17"),as.Date("2019-02-27")) 

#update
LabChemistry_match3 <- LabChemistry_match3  %>%
  mutate(date_crea_lipid = creatinindate - lipiddate, .after = "creatinindate") %>% 
  mutate(date_lipid_hba1c = lipiddate - hba1cdate, .after = "lipiddate") %>%   
  mutate(date_crea_hba1c = creatinindate - hba1cdate, .after = "hba1cdate")

LabChemistry_ab_date<- LabChemistry_match3[which(abs(LabChemistry_match3$date_crea_lipid)>300 | abs(LabChemistry_match3$date_lipid_hba1c)>300 | abs(LabChemistry_match3$date_crea_hba1c)>300 ), ]
dim(LabChemistry_ab_date) #5650
length(unique(LabChemistry_ab_date$patid)) #1907

LabChemistry_ab_date<-LabChemistry_match3[LabChemistry_match3$patid %in% LabChemistry_ab_date$patid,]
dim(LabChemistry_ab_date) #16961

# 2.5.3 fix in a batch way (not done yet) ----------------------------------------------

# need to decide which column is wrong and which column should be taken instead.

# 2.6 missing 3 measure date ------------------------------------------------------------
# if value is available, but the corresponding date is missing, we will use the date based on the order of creatinindate, lipiddate, hba1cdate, assdate
LabChemistry_match4 <- LabChemistry_match3

# if crea value is available, but no creatinindate, we will use the date, based on the order of creatinindate, lipiddate, hba1cdate, assdate
for (i in 1:nrow(LabChemistry_match4)){
  if( !is.na(LabChemistry_match4[i,]$crea) & is.na(LabChemistry_match4[i,]$creatinindate) ){
    if( !is.na(LabChemistry_match4[i,]$lipiddate) ){
      LabChemistry_match4[i,]$creatinindate <- LabChemistry_match4[i,]$lipiddate
      LabChemistry_match4[i,]$note <- paste0("creatinindate:NA_lipiddate",LabChemistry_match4[i,]$lipiddate,";")
    }else if( !is.na(LabChemistry_match4[i,]$hba1cdate) ){
      LabChemistry_match4[i,]$creatinindate <- LabChemistry_match4[i,]$hba1cdate
      LabChemistry_match4[i,]$note <- paste0("creatinindate:NA_hba1cdate",LabChemistry_match4[i,]$hba1cdate,";")
    }else if( !is.na(LabChemistry_match4[i,]$assdate) ){
      LabChemistry_match4[i,]$creatinindate <- LabChemistry_match4[i,]$assdate
      LabChemistry_match4[i,]$note <- paste0("creatinindate:NA_assdate",LabChemistry_match4[i,]$assdate,";")
    }
  }
  
  if( (!is.na(LabChemistry_match4[i,]$chol) | !is.na(LabChemistry_match4[i,]$ldlchol) | !is.na(LabChemistry_match4[i,]$hdlchol)) & is.na(LabChemistry_match4[i,]$lipiddate) ){
    if( !is.na(LabChemistry_match4[i,]$creatinindate) ){
      LabChemistry_match4[i,]$lipiddate <- LabChemistry_match4[i,]$creatinindate
      LabChemistry_match4[i,]$note <- paste0("lipiddate:NA_creatinindate",LabChemistry_match4[i,]$creatinindate,";")
    }else if( !is.na(LabChemistry_match4[i,]$hba1cdate) ){
      LabChemistry_match4[i,]$lipiddate <- LabChemistry_match4[i,]$hba1cdate
      LabChemistry_match4[i,]$note <- paste0("lipiddate:NA_hba1cdate",LabChemistry_match4[i,]$hba1cdate,";")
    }else if( !is.na(LabChemistry_match4[i,]$assdate) ){
      LabChemistry_match4[i,]$lipiddate <- LabChemistry_match4[i,]$assdate
      LabChemistry_match4[i,]$note <- paste0("lipiddate:NA_assdate",LabChemistry_match4[i,]$assdate,";")
    }
  }
  
  if( !is.na(LabChemistry_match4[i,]$hba1c) & is.na(LabChemistry_match4[i,]$hba1cdate) ){
    if( !is.na(LabChemistry_match4[i,]$creatinindate) ){
      LabChemistry_match4[i,]$hba1cdate <- LabChemistry_match4[i,]$creatinindate
      LabChemistry_match4[i,]$note <- paste0("hba1cdate:NA_creatinindate",LabChemistry_match4[i,]$creatinindate,";")
    }else if( !is.na(LabChemistry_match4[i,]$lipiddate) ){
      LabChemistry_match4[i,]$hba1cdate <- LabChemistry_match4[i,]$lipiddate
      LabChemistry_match4[i,]$note <- paste0("hba1cdate:NA_lipiddate",LabChemistry_match4[i,]$lipiddate,";")
    }else if( !is.na(LabChemistry_match4[i,]$assdate) ){
      LabChemistry_match4[i,]$hba1cdate <- LabChemistry_match4[i,]$assdate
      LabChemistry_match4[i,]$note <- paste0("hba1cdate:NA_assdate",LabChemistry_match4[i,]$assdate,";")
    }
  }
}



LabChemistry_match5 <- LabChemistry_match4 %>%
  rowwise() %>%
  mutate(crea_assperiod = ceiling(as.numeric(creatinindate - tpxdate)/30.5), .before = "hba1cdate") %>%
  mutate(crea_ass_days = creatinindate-tpxdate, .before = "hba1cdate") %>%
  mutate(lipid_assperiod = ceiling(as.numeric(lipiddate - tpxdate)/30.5), .before = "hba1cdate") %>%
  mutate(lipid_ass_days = lipiddate-tpxdate, .before = "hba1cdate") %>%
  mutate(hba1c_assperiod = ceiling(as.numeric(hba1cdate - tpxdate)/30.5), .before = "hba1cdate") %>%
  mutate(hba1c_ass_days = hba1cdate-tpxdate, .before = "hba1cdate")
dim(LabChemistry)        # 25904
dim(LabChemistry_match)  # 26513
dim(LabChemistry_match2) # 25540
dim(LabChemistry_match3) # 25540
dim(LabChemistry_match4) # 25540
dim(LabChemistry_match5) # 25540


# fix the new period
LabChemistry_match5[which(LabChemistry_match5$crea_assperiod < 0),]$crea_assperiod <-0
LabChemistry_match5[which(LabChemistry_match5$lipid_assperiod < 0),]$lipid_assperiod <-0
LabChemistry_match5[which(LabChemistry_match5$hba1c_assperiod < 0),]$hba1c_assperiod <-0


# 2.7 abnormal values  -----------------------------------------------
# 2.7.1 check abnormal values  -----------------------------------------------

# distribution plot
library(plotly)
# plot_ly(LabChemistry_match5, x = ~crea, type = "histogram", name = "Histogram", nbinsx = 30) 
# plot_ly(LabChemistry_match5, x = ~chol, type = "histogram", name = "Histogram", nbinsx = 30) 
# plot_ly(LabChemistry_match5, x = ~ldlchol, type = "histogram", name = "Histogram", nbinsx = 30) 
# plot_ly(LabChemistry_match5, x = ~hdlchol, type = "histogram", name = "Histogram", nbinsx = 30) 
# plot_ly(LabChemistry_match5, x = ~hba1c, type = "histogram", name = "Histogram", nbinsx = 30) 
# 
# plot_ly(LabChemistry_match5, x = ~log(crea), type = "histogram", name = "Histogram", nbinsx = 30) 
# plot_ly(LabChemistry_match5, x = ~log(chol), type = "histogram", name = "Histogram", nbinsx = 30) 
# plot_ly(LabChemistry_match5, x = ~log(ldlchol), type = "histogram", name = "Histogram", nbinsx = 30) 
# plot_ly(LabChemistry_match5, x = ~log(hdlchol), type = "histogram", name = "Histogram", nbinsx = 30) 
# plot_ly(LabChemistry_match5, x = ~log(hba1c), type = "histogram", name = "Histogram", nbinsx = 30) 


# global compare
# Confidence Interval (A 95% equal-tailed interval (ETI) has 2.5% of the distribution on either side of its limits.)
library(bayestestR)
crea_CI <- ci(LabChemistry_match5$crea, ci = 0.95) #[67.00, 825.00]      #95% ETI: [66.00, 808.00]
chol_CI <- ci(LabChemistry_match5$chol, ci = 0.95) #[2.60, 7.20]         #95% ETI: [2.60, 7.20]
ldlchol_CI <- ci(LabChemistry_match5$ldlchol, ci = 0.95) #[0.90, 4.60]   #95% ETI: [0.90, 4.60]
hdlchol_CI <- ci(LabChemistry_match5$hdlchol, ci = 0.95) #[0.70, 2.51]   #95% ETI: [0.69, 2.50]
hba1c_CI <- ci(LabChemistry_match5$hba1c, ci = 0.95) #[4.70, 8.70]       #95% ETI: [4.70, 8.70]


# local compare
# z-score
LabChemistry_match5 = LabChemistry_match5 %>%
  group_by(patid) %>% #ungroup() %>%
  mutate(z_score_crea = (crea - mean(crea))/sd(crea, na.rm =T), .after = "crea") %>%
  mutate(z_score_chol = (chol - mean(chol))/sd(chol, na.rm =T), .after = "chol") %>%
  mutate(z_score_ldlchol = (ldlchol - mean(ldlchol))/sd(ldlchol, na.rm =T), .after = "ldlchol") %>%
  mutate(z_score_hdlchol = (hdlchol - mean(hdlchol))/sd(hdlchol, na.rm =T), .after = "hdlchol") %>%
  mutate(z_score_hba1c = (hba1c - mean(hba1c, na.rm =T))/sd(hba1c, na.rm =T), .after = "hba1c") %>%
  mutate(gap_chol = chol - (ldlchol + hdlchol), .after = "chol") 

# 2.7.2 fix abnormal values  -----------------------------------------------

# ldlchol + hdlchol should not be larger than chol !!! Can we trust it?
# how could chol < (ldlchol + hdlchol) ??
table(LabChemistry_match5$gap_chol>0) # FALSE #401

# delete all three chol ?

LabChemistry_match5[LabChemistry_match5$gap_chol<0 & !is.na(LabChemistry_match5$gap_chol) &!is.na(LabChemistry_match5$chol),]$chol<-NA
LabChemistry_match5[LabChemistry_match5$gap_chol<0 & !is.na(LabChemistry_match5$gap_chol) &!is.na(LabChemistry_match5$ldlchol),]$ldlchol<-NA
LabChemistry_match5[LabChemistry_match5$gap_chol<0 & !is.na(LabChemistry_match5$gap_chol) &!is.na(LabChemistry_match5$hdlchol),]$hdlchol<-NA

# 2.8  save data  -----------------------------------------------

table(LabChemistry_match5$assdate==LabChemistry_match5$tpxdate) #3030 on tpxdate
a = LabChemistry_match5[LabChemistry_match5$assdate>=LabChemistry_match5$tpxdate-14 & LabChemistry_match5$assdate<=LabChemistry_match5$tpxdate,]
a %>% arrange(tpxdate)

a[duplicated(a$soascaseid) | duplicated(a$soascaseid,fromLast = TRUE), ]
b =a[!duplicated(a$soascaseid),]

dim(Patient2)#3051
dim(a)#3036
dim(b)#3035


LabChemistry_baseline_final<-b#3035
LabChemistry_followup_final<-LabChemistry_match5[LabChemistry_match5$soascaseid %in% b$soascaseid,] #25412



write.csv(LabChemistry_followup_final,"LabChemistry_followup_final.csv", row.names = FALSE, na="")
saveRDS(LabChemistry_followup_final, file = "LabChemistry_followup_final.rds")

