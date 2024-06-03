
#### 6. Proteinuria #####

Proteinuria <- read.csv (paste0(data_path,"Data_20231214/FUP121_Proteinuria_14DEC23.csv"), header=TRUE, sep = ",", stringsAsFactors=FALSE) 
dim(Proteinuria) # 21290
length(unique(Proteinuria$patid)) # 2992
length(unique(Proteinuria$soascaseid)) #3051

Proteinuria <- Reduce(function(x,y) merge(x,y,by="patid",all=TRUE) ,
                      list(PatientBaseline[c("patid","centreid")],Proteinuria))

#write.csv(Proteinuria,"Proteinuria_outlier.csv", row.names = FALSE, na="")

# 6.1 time format and remove those rows that are completely empty---------------------------------------------------------

Proteinuria = Proteinuria %>% 
  mutate(across(contains(c('assdate','proteinuriadate'), ignore.case = TRUE), lubridate::dmy)) %>% 
  filter(!is.na(proteinuria))
dim(Proteinuria) # 18619
# head(Proteinuria)


# 3.1 combine with Patient ------------------------------------------------------------

Proteinuria_match <- Patient[,c("patid","soascaseid","centreid","tpxdate","graft_date")]  %>%
  left_join(Proteinuria, by = c("patid","soascaseid","centreid"))

Proteinuria_match$note<-""

dim(Proteinuria) # 18619      #18619
dim(Proteinuria_match)# 14717 #18681

# 3.2 filter assdate <= graft_date ------------------------------------------------------------
# assume assdate is correct, but period might be wrong
Proteinuria_match2 <- Proteinuria_match %>%
  filter(assdate <= graft_date) # did not pick assdate = graft_date, because not sure if it is before or after graft loss.
dim(Physical_match2)# 18460 #23950

# 3.3 check duplicate assperiod --------------------------------------------------------------------

Proteinuria_dup <-Proteinuria_match2[duplicated(Proteinuria_match2[,c("patid","assperiod")]) |duplicated(Proteinuria_match2[,c("patid","assperiod")],fromLast=TRUE),]
Proteinuria_dup #129


# 3.4 check duplicate proteinuriadate ---------------------------------------------------

Proteinuria_dup <-Proteinuria_match2[duplicated(Proteinuria_match2[,c("patid","proteinuriadate")]) |duplicated(Proteinuria_match2[,c("patid","proteinuriadate")],fromLast=TRUE),]
Proteinuria_dup

# remove the records with the same value, same date
Proteinuria_dup <-Proteinuria_match2[ !is.na(Proteinuria_match2$proteinuriadate) & 
                                        (!duplicated(Proteinuria_match2[,c("patid","proteinuria")])) & 
                                        (duplicated(Proteinuria_match2[,c("patid","proteinuriadate")]) | 
                                           duplicated(Proteinuria_match2[,c("patid","proteinuriadate")],fromLast=TRUE)),]
Proteinuria_dup
# the left are different value, but the same date. however different period and assdate !!!
Proteinuria_dup <-Proteinuria_dup[ !is.na(Proteinuria_dup$proteinuriadate) & (duplicated(Proteinuria_dup[,c("patid","proteinuriadate")]) | duplicated(Proteinuria_dup[,c("patid","proteinuriadate")],fromLast=TRUE)),]

# why? one is wrong, could it be a typo?
Proteinuria_dup #22 careful, more digitals #28

unique(Proteinuria_dup$patid)
# [1] 80000527 80000779 80001093 80001126 80002294 80002339 80002998 80003495 80003533 80004576 80004740 80006377 80007515
# [14] 80008016

# 3.5 abnormal date  --------

# 3.5.1 check abnormal proteinuriadate --------------------------------------------

# calculate new assperiod and Uria_date_gap based on proteinuriadate

Proteinuria_match3 <- Proteinuria_match2 %>% 
  rowwise() %>%
  mutate(Uria_theo_assdate = tpxdate+assperiod*30.5, .before = "assperiod") %>%
  mutate(Uria_date_gap = proteinuriadate - Uria_theo_assdate, .before = "assperiod") %>%
  mutate(Uria_assperiod = ceiling(as.numeric(proteinuriadate - tpxdate)/30.5), .before = "assperiod")


dim(Proteinuria) # 18619
dim(Proteinuria_match)# 14902
dim(Proteinuria_match3)# 17095

Proteinuria_match3 %>% arrange(Uria_date_gap)
Proteinuria_match3 %>% arrange(desc(Uria_date_gap))

# need to decide whether assdate or proteinuriadate is wrong

# 3.5.2 fix manually ------------------------------------------------------

# 3.5.3 fix assdate in a batch way, using Phy_theo_assdate ----------------------------------------
# 3.5.3.1 1st round -------------------------------------------------------

# if |Uria_date_gap| > 300, then use Uria_theo_assdate (maybe less than 300 are also wrong)
Proteinuria_match3[which(abs(Proteinuria_match3$Uria_date_gap)>300 & Proteinuria_match3$Uria_theo_assdate<Proteinuria_match3$assdate),]$note<-paste0("proteinuriadate:",
                                                                                                                                                     Proteinuria_match3[which(abs(Proteinuria_match3$Uria_date_gap)>300 & Proteinuria_match3$Uria_theo_assdate<Proteinuria_match3$assdate),]$proteinuriadate,"_Uria_theo_assdate",
                                                                                                                                                     Proteinuria_match3[which(abs(Proteinuria_match3$Uria_date_gap)>300 & Proteinuria_match3$Uria_theo_assdate<Proteinuria_match3$assdate),]$Uria_theo_assdate,";"
)
Proteinuria_match3[which(abs(Proteinuria_match3$Uria_date_gap)>300 & Proteinuria_match3$Uria_theo_assdate<Proteinuria_match3$assdate),]$proteinuriadate<-Proteinuria_match3[which(abs(Proteinuria_match3$Uria_date_gap)>300 & Proteinuria_match3$Uria_theo_assdate<Proteinuria_match3$assdate),]$Uria_theo_assdate


# after correction proteinuriadate, always update
Proteinuria_match3 <- Proteinuria_match3 %>%
  rowwise()  %>%
  mutate(Uria_theo_assdate = tpxdate+assperiod*30.5, .before = "assperiod") %>%
  mutate(Uria_date_gap = proteinuriadate-Uria_theo_assdate, .before = "assperiod") %>%
  mutate(Phy_period = ceiling(as.numeric(proteinuriadate - tpxdate)/30.5), .before = "assperiod") 

# 2.5.3.2 fix missing date -------------------------------------------------------
Proteinuria_match3[is.na(Proteinuria_match3$proteinuriadate),]$note <- paste0("proteinuriadate:",
                                                                              Proteinuria_match3[is.na(Proteinuria_match3$proteinuriadate),]$proteinuriadate,"_Uria_theo_assdate",
                                                                              Proteinuria_match3[is.na(Proteinuria_match3$proteinuriadate),]$Uria_theo_assdate,";"
)
Proteinuria_match3[is.na(Proteinuria_match3$proteinuriadate),]$proteinuriadate <- Proteinuria_match3[is.na(Proteinuria_match3$proteinuriadate),]$Uria_theo_assdate

# 2.5.3.3 2nd round -------------------------------------------------------

Proteinuria_match3 <- Proteinuria_match3 %>% 
  arrange(patid, assperiod) %>% 
  group_by(patid)%>%
  mutate(counter=row_number())

# if current proteinuriadate > next , or proteinuriadate < previous, then use assdate

for (patid in c(unique(Proteinuria_match3$patid))){
  # print(patid)
  counter = Proteinuria_match3[Proteinuria_match3$patid==patid,]$counter
  if (length(counter) > 2){
    for (i in seq(2,length(counter)-1)){
      proteinuriadate_previous=Proteinuria_match3[Proteinuria_match3$patid==patid & Proteinuria_match3$counter==counter[i-1],]$proteinuriadate[1]
      proteinuriadate_current=Proteinuria_match3[Proteinuria_match3$patid==patid & Proteinuria_match3$counter==counter[i],]$proteinuriadate[1]
      assdate=Proteinuria_match3[Proteinuria_match3$patid==patid & Proteinuria_match3$counter==counter[i],]$assdate[1]
      proteinuriadate_next=Proteinuria_match3[Proteinuria_match3$patid==patid & Proteinuria_match3$counter==counter[i+1],]$proteinuriadate[1]
      
      if (proteinuriadate_previous > proteinuriadate_current){
        Proteinuria_match3[Proteinuria_match3$patid==patid & Proteinuria_match3$counter==counter[i],]$note<-paste0("proteinuriadate:",proteinuriadate_current,"_assdate",assdate,";"
        )
        Proteinuria_match3[Proteinuria_match3$patid==patid & Proteinuria_match3$counter==counter[i],]$proteinuriadate = assdate
      } 
      
      # internal update proteinuriadate
      proteinuriadate_current=Proteinuria_match3[Proteinuria_match3$patid==patid & Proteinuria_match3$counter==counter[i],]$proteinuriadate[1]
      
      if (proteinuriadate_current > proteinuriadate_next){
        Proteinuria_match3[Proteinuria_match3$patid==patid & Proteinuria_match3$counter==counter[i],]$note<-paste0("proteinuriadate:",proteinuriadate_current,"_assdate",assdate,";"
        )
        Proteinuria_match3[Proteinuria_match3$patid==patid & Proteinuria_match3$counter==counter[i],]$proteinuriadate = assdate
      } 
      
    }
  }
}
# the assdate still looks weird
# if current proteinuriadate > next , or proteinuriadate < previous, then use Uria_theo_assdate
# Uria_theo_assdate = tpxdate+assperiod*30.5

for (patid in c(unique(Proteinuria_match3$patid))){
  # print(patid)
  counter = Proteinuria_match3[Proteinuria_match3$patid==patid,]$counter
  if (length(counter) > 2){
    for (i in seq(2,length(counter)-1)){
      proteinuriadate_previous=Proteinuria_match3[Proteinuria_match3$patid==patid & Proteinuria_match3$counter==counter[i-1],]$proteinuriadate[1]
      proteinuriadate_current=Proteinuria_match3[Proteinuria_match3$patid==patid & Proteinuria_match3$counter==counter[i],]$proteinuriadate[1]
      Uria_theo_assdate=Proteinuria_match3[Proteinuria_match3$patid==patid & Proteinuria_match3$counter==counter[i],]$Uria_theo_assdate[1]
      proteinuriadate_next=Proteinuria_match3[Proteinuria_match3$patid==patid & Proteinuria_match3$counter==counter[i+1],]$proteinuriadate[1]
      
      if (proteinuriadate_previous > proteinuriadate_current){
        Proteinuria_match3[Proteinuria_match3$patid==patid & Proteinuria_match3$counter==counter[i],]$note<-paste0("proteinuriadate:",proteinuriadate_current,"_Uria_theo_assdate",Uria_theo_assdate,";"
        )
        Proteinuria_match3[Proteinuria_match3$patid==patid & Proteinuria_match3$counter==counter[i],]$proteinuriadate = Uria_theo_assdate
      } 
      
      # internal update proteinuriadate
      proteinuriadate_current=Proteinuria_match3[Proteinuria_match3$patid==patid & Proteinuria_match3$counter==counter[i],]$proteinuriadate[1]
      
      if (proteinuriadate_current > proteinuriadate_next){
        Proteinuria_match3[Proteinuria_match3$patid==patid & Proteinuria_match3$counter==counter[i],]$note<-paste0("proteinuriadate:",proteinuriadate_current,"_Uria_theo_assdate",Uria_theo_assdate,";"
        )
        Proteinuria_match3[Proteinuria_match3$patid==patid & Proteinuria_match3$counter==counter[i],]$proteinuriadate = Uria_theo_assdate
      } 
      
    }
  }
}


# after correction proteinuriadate, always update
Proteinuria_match4 <- Proteinuria_match3 %>%
  subset(select = -c(counter)) %>% 
  rowwise() %>%
  mutate(Uria_theo_assdate = tpxdate+assperiod*30.5, .before = "assperiod") %>%
  mutate(Uria_date_gap = proteinuriadate - Uria_theo_assdate, .before = "assperiod") %>%
  mutate(Uria_assperiod = ceiling(as.numeric(proteinuriadate - tpxdate)/30.5), .before = "assperiod")


# 3.6 abnormal values -------------------------------------------------------------
# Proteinuria: calculation not possible (protein too low) enter: Below detection limit and enter 0
# not measurable (highly concentrated or too little urin) enter: Not applicable and -999 is coming automatic

table(Proteinuria_match4$proteinuriadesc)
# Oct 2012,  proteinuriadesc should be empty

#           Above      Below  Measurable 
# 1880         37       1576       10845

#new
#           Above      Below Measurable 
# 2482         63       2019      14015 
Proteinuria_match5 <- Proteinuria_match4[Proteinuria_match4$proteinuriadesc == "Measurable",]

# 3.7 missing values  -------------------------------------------------------------
table(!is.na(Proteinuria_match5$proteinuria))
# TRUE 
# 10845 #14015 all true

Proteinuria_match_final <- Proteinuria_match5


write.csv(Proteinuria_match_final,"Proteinuria_match_final.csv", row.names = FALSE, na="")
saveRDS(Proteinuria_match_final, file = "Proteinuria_match_final.rds")
