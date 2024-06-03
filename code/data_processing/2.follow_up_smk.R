#### 4. Smoking #####

Smoking <- read.csv (paste0(data_path,"Data_20231214/FUP121_Smoking_14DEC23.csv"), header=TRUE, sep = ",", stringsAsFactors=FALSE) 
dim(Smoking) # 26815
length(unique(Smoking$patid)) # 2992


# 4.1 time format ---------------------------------------------------------

Smoking = Smoking %>% 
  mutate(across(contains(c('psqdate'), ignore.case = TRUE), lubridate::dmy)) %>% 
  filter(smoking_0 !="")
dim(Smoking) #24915
# there is only psq date, not mismatching problem


# 4.2 combine with Patient ------------------------------------------------------------

Smoking_match <- Patient[,c("patid","soascaseid","centreid","tpxdate","graft_date","max_n_kidney_new")]  %>%
  left_join(Smoking, by = c("patid"))

Smoking_match$note<-""

dim(Smoking)      # 24915
dim(Smoking_match)# 25450



# 4.3 filter psqdate <= graft_date ------------------------------------------------------------

# 4.3.1 fix missing date -------------------------------------------------------

Smoking_match2 <- Smoking_match %>%
  rowwise() %>%
  mutate(Smoking_theo_assdate = tpxdate+assperiod*30.5, .before = "assperiod") %>%
  mutate(Smoking_date_gap = psqdate - Smoking_theo_assdate, .before = "assperiod")

# if psqdate is NA, use Smoking_theo_assdate, and add to the note
Smoking_match2[is.na(Smoking_match2$psqdate),]$note <- paste0("psqdate:",
                                                              Smoking_match2[is.na(Smoking_match2$psqdate),]$psqdate,"_Smoking_theo_assdate",
                                                              Smoking_match2[is.na(Smoking_match2$psqdate),]$Smoking_theo_assdate,";"
)

Smoking_match2[is.na(Smoking_match2$psqdate),]$psqdate <- Smoking_match2[is.na(Smoking_match2$psqdate),]$Smoking_theo_assdate

# 4.3.2 fix typo date -------------------------------------------------------

change_date_manually <- function(df,patid,colname_look,value,colname_change,old_date,new_assdate){
  df[df$patid == patid & df[,colname_look] == value, ]$note <- paste0(colname_change,":",old_date,"_manual",new_assdate,";")
  df[df$patid == patid & df$assperiod == value, colname_change] <- new_assdate
  return(df)
}

Smoking_match2 <- change_date_manually(Smoking_match2,80001503,"assperiod",132,"psqdate",as.Date("1961-02-23"),as.Date("2021-02-23")) 
Smoking_match2 <- change_date_manually(Smoking_match2,80005264,"assperiod",60,"psqdate",as.Date("2000-06-08"),as.Date("2020-06-08")) 
Smoking_match2 <- change_date_manually(Smoking_match2,80008618,"assperiod",6,"psqdate",as.Date("2008-08-27"),as.Date("2018-08-27")) 
Smoking_match2 <- change_date_manually(Smoking_match2,80004624,"assperiod",12,"psqdate",as.Date("2005-08-11"),as.Date("2015-08-11")) 
Smoking_match2 <- change_date_manually(Smoking_match2,80000509,"assperiod",96,"psqdate",as.Date("2008-12-23"),as.Date("2016-12-23")) 

# 4.3.3 filter by psqdate -------------------------------------------------------
# assume psqdate is correct, but period might be wrong
dim(Smoking_match2)# 25450
Smoking_match3 <- Smoking_match2 %>%
  filter(psqdate <= graft_date) %>% # did not pick assdate = graft_date, because not sure if it is before or after graft loss.
  filter(ifelse(max_n_kidney_new==1, psqdate >= tpxdate-365.25*5, psqdate >= tpxdate-365.25*2))

dim(Smoking_match3)# 19333

# 4.4 check duplicate assperiod --------------------------------------------------------------------

Smoking_dup <-Smoking_match3[duplicated(Smoking_match3[,c("patid","assperiod")]) |duplicated(Smoking_match3[,c("patid","assperiod")],fromLast=TRUE),]
Smoking_dup #156

# Smoking_match3<-Smoking_match3[-which(Smoking_match3$patid == 80000412 & Smoking_match3$psqdate == as.Date("2008-10-28")),]
# Smoking_match3 <- Smoking_match3[-which(Smoking_match3$patid == 80000494 & Smoking_match3$assperiod == 0 & Smoking_match3$psqdate == as.Date("2016-09-09")),]
# Smoking_match3 <- Smoking_match3[-which(Smoking_match3$patid == 80000815 & Smoking_match3$assperiod == 12 & Smoking_match3$smoking_0 == "Never"),]


# for 80000494 80000815, one of the period is wrong, it is okay as we will use psqdate and get new period

# 4.4 check duplicate psqdate ---------------------------------------------------
Smoking_dup <-Smoking_match3[duplicated(Smoking_match3[,c("patid","psqdate")]) |duplicated(Smoking_match3[,c("patid","psqdate")],fromLast=TRUE),]
Smoking_dup # 59

# remove duplicate psqdate
Smoking_match4<-Smoking_match3[!duplicated(Smoking_match3[,c("patid","psqdate")]),]

dim(Smoking_match2) # 25450
dim(Smoking_match3) # 19333
dim(Smoking_match4) # 19298

# update
Smoking_match5 <- Smoking_match4 %>%
  rowwise() %>%
  mutate(Smoking_date_gap = psqdate - Smoking_theo_assdate, .before = "assperiod")

# 4.5 abnormal psqdate ---------------------------------------------------

Smoking_match5 %>% arrange(Smoking_date_gap)
Smoking_match5 %>% arrange(desc(Smoking_date_gap))


# 4.5.2 fix manually ------------------------------------------------------


# Smoking_match5 %>%
#   filter(psqdate > as.Date("2023-01-01"))
# Smoking_match6 <- Smoking_match5 %>%
#   mutate(psqdate = if_else(psqdate > as.Date("2023-01-01"), Smoking_theo_assdate, psqdate))

# below are also period != 0, Smoking_theo_assdate > 300, there are more
# 80008624 80002147 80000854 80006134 80005150 80000635 80002535 80001138 80001227 80001114 80002602 80001919 80001812 80001093 80002709 80002362 80010037 80001194 80007640 80000635 80009319 80009755 80008402 80005011 80000455 80000548 80001644 ! 80000494 80001410 80003344 80001725 80001003 80001209 80000707 80000210 80008837
# 
# after correction psqdate, always update
# Smoking_match5 <- Smoking_match5 %>%
#   rowwise() %>%
#   mutate(Smoking_date_gap = psqdate - Smoking_theo_assdate, .before = "assperiod")


# 4.5.3 fix psqdate in a batch way, using Phy_theo_assdate ----------------------------------------
# 4.5.3.1 1st round -------------------------------------------------------
# if |Smoking_date_gap| > 300, then use Smoking_theo_assdate (maybe less than 300 are also wrong)
# not good, because the period can be very wrong
# Smoking_match5[which(Smoking_match5$assperiod !=0 & abs(Smoking_match5$Smoking_date_gap)>300),]$note<-paste0("psqdate:",
#                                                                                                              Smoking_match5[which(Smoking_match5$assperiod !=0 & abs(Smoking_match5$Smoking_date_gap)>300),]$psqdate,"_Smoking_theo_assdate",
#                                                                                                              Smoking_match5[which(Smoking_match5$assperiod !=0 & abs(Smoking_match5$Smoking_date_gap)>300),]$Smoking_theo_assdate,";"
# )
# Smoking_match5[which(Smoking_match5$assperiod !=0 & abs(Smoking_match5$Smoking_date_gap)>300),]$psqdate<-Smoking_match5[which(Smoking_match5$assperiod !=0 & abs(Smoking_match5$Smoking_date_gap)>300),]$Smoking_theo_assdate
# 
# 
# # after correction psqdate, always update
# Smoking_match6 <- Smoking_match5 %>%
#   rowwise() %>%
#   mutate(Smoking_date_gap = psqdate - Smoking_theo_assdate, .before = "assperiod")

# 4.5.3.3 2nd round -------------------------------------------------------

Smoking_match6 <- Smoking_match5 %>%
  arrange(patid, psqdate) %>%
  group_by(patid)%>%
  mutate(counter=row_number())

# if current psqdate > next , or psqdate < previous, then use Smoking_theo_assdate
# not good, because the two period ==0 will be together, and for sure the psqdate for second baseline > first follow up
for (patid in c(unique(Smoking_match6$patid))){
  # print(patid)
  counter = Smoking_match6[Smoking_match6$patid==patid,]$counter
  if (length(counter) > 2){
    for (i in seq(2,length(counter)-1)){
      psqdate_previous=Smoking_match6[Smoking_match6$patid==patid & Smoking_match6$counter==counter[i-1],]$psqdate[1]
      psqdate_current=Smoking_match6[Smoking_match6$patid==patid & Smoking_match6$counter==counter[i],]$psqdate[1]
      Smoking_theo_assdate=Smoking_match6[Smoking_match6$patid==patid & Smoking_match6$counter==counter[i],]$Smoking_theo_assdate[1]
      psqdate_next=Smoking_match6[Smoking_match6$patid==patid & Smoking_match6$counter==counter[i+1],]$psqdate[1]

      if (psqdate_previous > psqdate_current){
        Smoking_match6[Smoking_match6$patid==patid & Smoking_match6$counter==counter[i],]$note<-paste0("psqdate:",psqdate_current,"_Smoking_theo_assdate",Smoking_theo_assdate,";"
        )
        Smoking_match6[Smoking_match6$patid==patid & Smoking_match6$counter==counter[i],]$psqdate = Smoking_theo_assdate
      }

      # internal update psqdate
      psqdate_current=Smoking_match6[Smoking_match6$patid==patid & Smoking_match6$counter==counter[i],]$psqdate[1]

      if (psqdate_current > psqdate_next){
        Smoking_match6[Smoking_match6$patid==patid & Smoking_match6$counter==counter[i],]$note<-paste0("psqdate:",psqdate_current,"_Smoking_theo_assdate",Smoking_theo_assdate,";"
        )
        Smoking_match6[Smoking_match6$patid==patid & Smoking_match6$counter==counter[i],]$psqdate = Smoking_theo_assdate
      }

    }
  }
}

Smoking_match7 <- Smoking_match6

# 4.6 abnormal values -----------------------------------------------------
Smoking_match7$smoking = Smoking_match7$smoking_0
#based on the previous one
for (patid in c(unique(Smoking_match7$patid))){
  # print(patid)
  counter = Smoking_match7[Smoking_match7$patid==patid,]$counter
  if (length(counter) > 1){
    for (i in seq(2,length(counter))){
      smoking_previous=Smoking_match7[Smoking_match7$patid==patid & Smoking_match7$counter==counter[i-1],]$smoking_0[1]
      smoking_current=Smoking_match7[Smoking_match7$patid==patid & Smoking_match7$counter==counter[i],]$smoking_0[1]
      
      if ( !is.na(smoking_previous) & smoking_previous != "" & !is.na(smoking_current) & smoking_current == "No"){
        if (smoking_previous == "Never"){
          Smoking_match7[Smoking_match7$patid==patid & Smoking_match7$counter==counter[i],]$note<-paste0("psqdate:",smoking_current,"_previous:",smoking_previous,";")
          Smoking_match7[Smoking_match7$patid==patid & Smoking_match7$counter==counter[i],]$smoking_0 = "Never"
        } else if (smoking_previous == "Yes"){
          Smoking_match7[Smoking_match7$patid==patid & Smoking_match7$counter==counter[i],]$note<-paste0("psqdate:",smoking_current,"_previous:",smoking_previous,";")
          Smoking_match7[Smoking_match7$patid==patid & Smoking_match7$counter==counter[i],]$smoking_0 = "Stopped < one year ago"
        } else {
          Smoking_match7[Smoking_match7$patid==patid & Smoking_match7$counter==counter[i],]$note<-paste0("psqdate:",smoking_current,"_previous:","Stopped > one year ago",";")
          Smoking_match7[Smoking_match7$patid==patid & Smoking_match7$counter==counter[i],]$smoking_0 = "Stopped > one year ago"
        }
      }
    }
  }
}
# based on the next one
Smoking_match7$smoking_first_change = Smoking_match7$smoking_0

for (patid in c(unique(Smoking_match7$patid))){
  # print(patid)
  counter = Smoking_match7[Smoking_match7$patid==patid,]$counter
  if (length(counter) > 1){
    for (i in seq(1,length(counter)-1)){
      smoking_next=Smoking_match7[Smoking_match7$patid==patid & Smoking_match7$counter==counter[i+1],]$smoking_0[1]
      smoking_current=Smoking_match7[Smoking_match7$patid==patid & Smoking_match7$counter==counter[i],]$smoking_0[1]
      
      if ( !is.na(smoking_next) & smoking_next != "" & !is.na(smoking_current) & smoking_current == "No"){
        if (smoking_next == "Never"){
          Smoking_match7[Smoking_match7$patid==patid & Smoking_match7$counter==counter[i],]$note<-paste0("psqdate:",smoking_current,"_next:",smoking_next,";")
          Smoking_match7[Smoking_match7$patid==patid & Smoking_match7$counter==counter[i],]$smoking_0 = "Never"
        } else if (smoking_next == "Yes"){
          Smoking_match7[Smoking_match7$patid==patid & Smoking_match7$counter==counter[i],]$note<-paste0("psqdate:",smoking_current,"_next:",smoking_next,";")
          Smoking_match7[Smoking_match7$patid==patid & Smoking_match7$counter==counter[i],]$smoking_0 = "Yes"
        } else if (smoking_next == "Stopped < one year ag"){
          Smoking_match7[Smoking_match7$patid==patid & Smoking_match7$counter==counter[i],]$note<-paste0("psqdate:",smoking_current,"_next:",smoking_next,";")
          Smoking_match7[Smoking_match7$patid==patid & Smoking_match7$counter==counter[i],]$smoking_0 = "Yes"
        } else {
          Smoking_match7[Smoking_match7$patid==patid & Smoking_match7$counter==counter[i],]$note<-paste0("psqdate:",smoking_current,"_next:","Stopped > one year ago",";")
          Smoking_match7[Smoking_match7$patid==patid & Smoking_match7$counter==counter[i],]$smoking_0 = "Stopped > one year ago"
        }
      }
    }
  }
}

Smoking_match7$smoking_0 = ifelse(Smoking_match7$smoking_0=="No","Never",Smoking_match7$smoking_0)

# filtered_patients <- Smoking_match7 %>%
#   group_by(patid) %>%
#   filter(any(smoking_0 == "No")) %>%
#   ungroup()



# Smoking_match7 <- Smoking_match7 %>%
#   subset(select = -c(counter))

# Smoking_match7$smoking<-ifelse(Smoking_match7$smoking_0=="Yes","Yes",
#                                ifelse(Smoking_match7$smoking_0 %in% c("Stopped < one year ago","Stopped > one year ago","No","Never"),"No","Unknown"))

# 4.7 missing values  -------------------------------------------------------------
table(Smoking_match7$smoking)
table(Smoking_match7$smoking_0)
# Never                Refused Stopped < one year ago Stopped > one year ago                Unknown 
# 9477                    256                    570                   6282                    140 
# Yes 
# 2305 
Smoking_match7$smoking_0 = ifelse(Smoking_match7$smoking_0=="Refused","Unknown",Smoking_match7$smoking_0)


#    No Unknown     Yes 
# 16549     410    2339 

Smoking_followup_final <- Smoking_match7

Smoking_baseline<-Smoking_followup_final[Smoking_followup_final$psqdate <= Smoking_followup_final$tpxdate,]

dim(Smoking_baseline) #2411

Smoking_baseline <-Smoking_baseline%>% 
  group_by(soascaseid) %>%
  dplyr::slice(which.max(psqdate))

dim(Smoking_baseline) #2362


write.csv(Smoking_followup_final,"Smoking_followup_final.csv", row.names = FALSE, na="")
saveRDS(Smoking_followup_final, file = "Smoking_followup_final.rds")

