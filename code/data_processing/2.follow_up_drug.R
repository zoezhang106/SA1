#### 9 Drugs #####
       
Drugs <- read.csv (paste0(data_path,"Data_20231214/FUP121_Drugs_14DEC23.csv"), header=TRUE, sep = ",", stringsAsFactors=FALSE) 
dim(Drugs)# [1] 46362    13
length(unique(Drugs$patid))# 2992
setdiff(Cases$patid,unique(Drugs$patid)) # no missing
Drugs <- Drugs %>% mutate(across(contains('date', ignore.case = TRUE), lubridate::dmy))
head(Drugs)

DrugsInduction <- read.csv (paste0(data_path,"Data_20231214/FUP121_DrugsInduction_14DEC23.csv"), header=TRUE, sep = ",", stringsAsFactors=FALSE) 
dim(DrugsInduction)# [1] 46362    13
length(unique(DrugsInduction$patid))# 2992
setdiff(Cases$patid,unique(DrugsInduction$patid)) # no missing
DrugsInduction <- DrugsInduction %>% mutate(across(contains('date', ignore.case = TRUE), lubridate::dmy))
head(DrugsInduction)



DrugsImmunoSuppression <- read.csv (paste0(data_path,"Data_20231214/FUP121_DrugsImmunoSuppression_14DEC23.csv"), header=TRUE, sep = ",", stringsAsFactors=FALSE) 
dim(DrugsImmunoSuppression)# [1] 46362    13
length(unique(DrugsImmunoSuppression$patid))# 2992
setdiff(Cases$patid,unique(DrugsImmunoSuppression$patid)) # no missing
DrugsImmunoSuppression <- DrugsImmunoSuppression %>% mutate(across(contains('date', ignore.case = TRUE), lubridate::dmy))
head(DrugsImmunoSuppression)


DrugsOther <- read.csv (paste0(data_path,"Data_20231214/FUP121_DrugsOther_14DEC23.csv"), header=TRUE, sep = ",", stringsAsFactors=FALSE) 
dim(DrugsOther)# [1] 46362    13
length(unique(DrugsOther$patid))# 2992
setdiff(Cases$patid,unique(DrugsOther$patid)) # no missing
DrugsOther <- DrugsOther %>% mutate(across(contains('date', ignore.case = TRUE), lubridate::dmy))
head(DrugsOther)



# The date of starting a drug intake period which is either imputed by the CDM (day and/or month) if missing or originally (and completely) given by the LDM. In the latter case, it holds ‘start.date.impute’ = ‘start.date’.
# from 02.05.2008
# fix the date 

Drugs[is.na(Drugs$start_date),]$start_date <- Drugs[is.na(Drugs$start_date),]$start_date_impute
Drugs[is.na(Drugs$stop_date),]$stop_date <- Drugs[is.na(Drugs$stop_date),]$stop_date_impute

dim(Drugs) # 46362
dim(Drugs[-which(is.na(Drugs$start_date)),]) # 43684
dim(Drugs[-which(is.na(Drugs$stop_date)),]) # 15280

a<- Drugs[which(is.na(Drugs$start_date)),]
a<- Drugs[which(is.na(Drugs$start_date) & is.na(Drugs$stop_date)),]
# a<- Drugs[which(is.na(Drugs$start_date) & is.na(Drugs$stop_date) & is.na(Drugs$class == "Immunosuppression")),]

dim(a) #1241

table(a$class)
# Immunosuppression     Induction InfectiousDiseaseProphylaxis                   OtherDrugs 
# 22                            2                           10                         1207 

library(stringr)

Patient_bl<-readRDS(file = "Patient_bl.rds")


DrugsInduction_match <- Reduce(function(x,y) merge(x,y,by="patid",all.x=TRUE) ,
                               list(DrugsInduction,
                                    Patient_bl[,c("patid","soascaseid","tpxdate","graft_date","max_n_kidney_new")]))
DrugsInduction_match<-DrugsInduction_match[DrugsInduction_match$patid %in% Patient_bl$patid,]
dim(Patient_bl) #3544
dim(DrugsInduction_match) # 4782



DrugsInduction_match$drugstartdate<-as.Date(ifelse(is.na(DrugsInduction_match$drugstartdate), as.character(DrugsInduction_match$tpxdate), as.character(DrugsInduction_match$drugstartdate)))

DrugsInduction_match[DrugsInduction_match$patid == 80008712,]$drugstartdate<-as.Date("2018-04-16") #2015-04-16
DrugsInduction_match[DrugsInduction_match$patid == 80007635,]$drugstartdate<-as.Date("2017-02-08") #2016-02-08
DrugsInduction_match[DrugsInduction_match$patid == 80006954,]$drugstartdate<-as.Date("2016-07-04") #2010-07-04

DrugsInduction_match[DrugsInduction_match$patid == 80004206,]$drugstartdate<-as.Date("2014-02-27") #2012-02-27
DrugsInduction_match[DrugsInduction_match$patid == 80004206,]$drugstopdate<-as.Date("2014-03-03") #2012-03-03


DrugsInduction_match2 <-DrugsInduction_match %>%
  filter(ifelse(max_n_kidney_new==1, drugstartdate >= tpxdate-365, drugstartdate >= tpxdate-30))%>%
  filter(drugstartdate <= tpxdate+14)
dim(DrugsInduction_match2) # 4606

# setdiff(unique(DrugsInduction_match$patid),unique(DrugsInduction_match2$patid))
# 80000364 80000815 80001112 80003296 80008766


DrugsImmunoSuppression_match <- Reduce(function(x,y) merge(x,y,by="patid",all.x=TRUE) ,
                                       list(DrugsImmunoSuppression,
                                            Patient_bl[,c("patid","soascaseid","tpxdate","graft_date","max_n_kidney_new")]))
DrugsImmunoSuppression_match<-DrugsImmunoSuppression_match[DrugsImmunoSuppression_match$patid %in% Patient_bl$patid,]
dim(Patient_bl) #3544
dim(DrugsImmunoSuppression_match) # 18334



DrugsImmunoSuppression_match$drugstartdate<-as.Date(ifelse(is.na(DrugsImmunoSuppression_match$drugstartdate), as.character(DrugsImmunoSuppression_match$tpxdate), as.character(DrugsImmunoSuppression_match$drugstartdate)))

DrugsImmunoSuppression_match2 <-DrugsImmunoSuppression_match%>%
  filter(ifelse(max_n_kidney_new==1, drugstartdate >= tpxdate-365, drugstartdate >= tpxdate-30))%>%
  filter(drugstartdate <= tpxdate+14)
dim(DrugsImmunoSuppression_match2) # 13080



DrugsOther_match <- Reduce(function(x,y) merge(x,y,by="patid",all.x=TRUE) ,
                           list(DrugsOther,
                                Patient_bl[,c("patid","soascaseid","tpxdate","graft_date","max_n_kidney_new")]))
DrugsOther_match<-DrugsOther_match[DrugsOther_match$patid %in% Patient_bl$patid,]
dim(Patient_bl) #3544
dim(DrugsOther_match) # 27082



DrugsOther_match$drugstartdate<-as.Date(ifelse(is.na(DrugsOther_match$drugstartdate), as.character(DrugsOther_match$tpxdate), as.character(DrugsOther_match$drugstartdate)))

DrugsOther_match2 <-DrugsOther_match%>%
  filter(ifelse(max_n_kidney_new==1, drugstartdate >= tpxdate-365, drugstartdate >= tpxdate-30))#%>%
#filter(drugstartdate <= tpxdate+14)


getCount2 <- function(data,keyword_list){
  for (keyword in keyword_list){
    data[,as.character(keyword)] <- str_count(data$drug, keyword)
  }
  
  return(data.frame(data))
}

DrugsInduction2<-as.data.frame(getCount2(DrugsInduction_match2,unique(DrugsInduction_match2$drug[DrugsInduction_match2$drug != ""])))
DrugsImmunoSuppression2<-as.data.frame(getCount2(DrugsImmunoSuppression_match2,unique(DrugsImmunoSuppression_match2$drug[DrugsImmunoSuppression_match2$drug != ""])))
DrugsOther2<-as.data.frame(getCount2(DrugsOther_match2,unique(DrugsOther_match2$drug[DrugsOther_match2$drug != ""])))

names(DrugsImmunoSuppression2) <- gsub("[.]", "-", names(DrugsImmunoSuppression2))
names(DrugsOther2) <- gsub("[.]", "-", names(DrugsOther2))



table(Drugs$type)
table(DrugsInduction$drug)
# ATG          ATGAM    Basiliximab           IVIG          Other Plasmapheresis      Rituximab  Thymoglobulin 
# 1            183              1           2242            188             80             79            207            735 
table(DrugsImmunoSuppression$drug)
# Aza            CsA         EC-MPA     Everolimus             FK Glucocorticoid            MMF          Other      Sirolimus 
# 594            784           1323            176           3752           3702           2886            273            128 
table(DrugsOther$drug)
# ACE-Inhibitor                 ARB        Beta-blocker                 CCB             Insulin                 OAK  Oral antidiabetics 
# 1822                1472                2570                3402                1139                1517                1023 
# Other Platelet inhibitior              Statin 
# 4278                1849                2419 

DrugsInduction_sets = unique(DrugsInduction2$drug[DrugsInduction2$drug != ""])
DrugsImmunoSuppression_sets = unique(DrugsImmunoSuppression2$drug[DrugsImmunoSuppression2$drug != ""])
DrugsOther_sets = unique(DrugsOther2$drug[DrugsOther2$drug != ""])
DrugsOther_sets <- gsub("[ ]", "-", DrugsOther_sets)


library(dplyr)

DrugsInduction3 <- DrugsInduction2 %>% 
  group_by(patid) %>% 
  summarise(across(DrugsInduction_sets, list(sum = sum)))

DrugsImmunoSuppression3 <- DrugsImmunoSuppression2 %>% 
  group_by(patid) %>% 
  summarise(across(DrugsImmunoSuppression_sets, list(sum = sum)))

DrugsOther3 <- DrugsOther2 %>% 
  group_by(patid) %>% 
  summarise(across(DrugsOther_sets, list(sum = sum)))


names(DrugsInduction3)<-c("patid",DrugsInduction_sets)
names(DrugsImmunoSuppression3)<-c("patid",DrugsImmunoSuppression_sets)
names(DrugsOther3)<-c("patid",DrugsOther_sets)

DrugsInduction3[,c(2:8)] <- ifelse(DrugsInduction3[,c(2:8)]==0,0,1)
DrugsImmunoSuppression3[,c(2:10)] <- ifelse(DrugsImmunoSuppression3[,c(2:10)]==0,0,1)
DrugsOther3[,c(2:11)] <- ifelse(DrugsOther3[,c(2:11)]==0,0,1)

library(UpSetR)



upset(as.data.frame(DrugsInduction3), sets = DrugsInduction_sets, mb.ratio = c(0.55, 0.45))
upset(as.data.frame(DrugsInduction3), sets = DrugsInduction_sets, mb.ratio = c(0.55, 0.45), order.by = "freq", keep.order = TRUE)
upset(as.data.frame(DrugsImmunoSuppression3), sets = DrugsImmunoSuppression_sets, mb.ratio = c(0.55, 0.45), order.by = "freq", keep.order = TRUE)
upset(as.data.frame(DrugsOther3), sets = c("ARB","Beta-blocker","CCB","Insulin","Oral-antidiabetics","Platelet-inhibitior","Statin","ACE-Inhibitor"), nintersects = 70, mb.ratio = c(0.55, 0.45), order.by = "freq", keep.order = TRUE)

upset(as.data.frame(DrugsOther3), sets = DrugsOther_sets, nintersects = 70, mb.ratio = c(0.55, 0.45), order.by = "freq", keep.order = TRUE)



write.csv(DrugsInduction_match2,"DrugsInduction_match.csv", row.names = FALSE, na="")
saveRDS(DrugsInduction_match2, file = "DrugsInduction_match.rds")

write.csv(DrugsImmunoSuppression_match2,"DrugsImmunoSuppression_match.csv", row.names = FALSE, na="")
saveRDS(DrugsImmunoSuppression_match2, file = "DrugsImmunoSuppression_match.rds")

write.csv(DrugsOther_match2,"DrugsOther_match.csv", row.names = FALSE, na="")
saveRDS(DrugsOther_match2, file = "DrugsOther_match.rds")


####################

DrugsInduction_match3 <- DrugsInduction_match2 %>%
  # filter(ifelse(max_n_kidney_new==1, drugstopdate <= graft_date, drugstopdate <= graft_date)) %>%
  # filter(ifelse(max_n_kidney_new==1, drugstartdate >= tpxdate-365, drugstartdate >= tpxdate-30))%>%
  mutate(Basiliximab = str_count(drug, 'Basiliximab')) %>% #1 Monoclonal antibodies for t-cell-inhibition
  mutate(Rituximab = str_count(drug, 'Rituximab')) %>% #1 Monoclonal antibodies for t-cell-inhibition
  mutate(Plasmapheresis = str_count(drug, 'Plasmapheresis')) %>% #2 Plasmapheresis
  mutate(Thymoglobulin = str_count(drug, 'Thymoglobulin')) %>% #3 Polyclonal antibodies for t-cell-inhibition
  mutate(ATG = str_count(drug, 'ATG')) %>% #3 Polyclonal antibodies for t-cell-inhibition
  mutate(IVIG = str_count(drug, 'IVIG')) %>% #4 Polyclonal and polyvalent
  mutate(Other_Induction = str_count(drug, 'Other')) 

DrugsImmunoSuppression_match3 <- DrugsImmunoSuppression_match2 %>%
  # filter(ifelse(max_n_kidney_new==1, drugstopdate <= graft_date, drugstopdate <= graft_date)) %>%
  # filter(ifelse(max_n_kidney_new==1, drugstartdate >= tpxdate-365, drugstartdate >= tpxdate-30))%>%
  mutate(CsA = str_count(drug, 'CsA')) %>% #1 Calcineurin inhibitors (CNI): Cyclosporine A (CsA) and Tacrolimus (Tac)
  mutate(FK = str_count(drug, 'FK')) %>% #1 Calcineurin inhibitors (CNI): Cyclosporine A (CsA) and Tacrolimus (Tac)
  mutate(MMF = str_count(drug, 'MMF')) %>% #2 Mycophenolate mofetil (MMF) and Mycophenolic Acid (MPA)
  mutate(EC_MPA = str_count(drug, 'EC-MPA')) %>% #2 Mycophenolate mofetil (MMF) and Mycophenolic Acid (MPA)
  mutate(Sirolimus = str_count(drug, 'Sirolimus')) %>% #3 mTOR inhibitors: sirolimus (Rapamune) and everolimus (Certican)
  mutate(Everolimus = str_count(drug, 'Everolimus')) %>% #3 mTOR inhibitors: sirolimus (Rapamune) and everolimus (Certican)
  mutate(Glucocorticoid = str_count(drug, 'Glucocorticoid')) %>% #4 Corticosteroids (Prednison and Methylprednisolon, Glucocorticoid)
  mutate(Aza = str_count(drug, 'Aza')) %>% #5 Azathioprine
  mutate(Other_ImmunoSuppression = str_count(drug, 'Other')) 

DrugsOther_match3 <- DrugsOther_match2 %>%
  # filter(ifelse(max_n_kidney_new==1, drugstopdate <= graft_date, drugstopdate <= graft_date)) %>%
  # filter(ifelse(max_n_kidney_new==1, drugstartdate >= tpxdate-365, drugstartdate >= tpxdate-30))%>%
  mutate(ACE_Inhibitor = str_count(drug, 'ACE-Inhibitor')) %>% #1 hypertension
  mutate(ARB = str_count(drug, 'ARB')) %>% #1
  mutate(Beta_blocker = str_count(drug, 'Beta-blocker')) %>% #1
  mutate(CCB = str_count(drug, 'CCB')) %>% #1
  mutate(Insulin = str_count(drug, 'Insulin')) %>% #2 diabetes
  mutate(Oral_antidiabetics  = str_count(drug, 'Oral antidiabetics')) %>% #2 diabetes
  mutate(Platelet_inhibitior = str_count(drug, 'Platelet inhibitior')) %>% #3 platelet adhesion
  mutate(Statin = str_count(drug, 'Statin')) %>% #4 help lower total cholesterol
  mutate(OAK = str_count(drug, 'OAK')) %>% #5
  mutate(Other = str_count(drug, 'Other')) 

length(unique(DrugsInduction_match$patid))#2872
length(unique(DrugsInduction_match2$patid))
length(unique(DrugsInduction_match3$patid))#2867



length(unique(DrugsImmunoSuppression_match$patid))#2992
length(unique(DrugsImmunoSuppression_match2$patid))#2981
length(unique(DrugsImmunoSuppression_match3$patid))


length(unique(DrugsOther_match$patid))#2957
length(unique(DrugsOther_match2$patid))#2932
length(unique(DrugsOther_match3$patid))#2932


DrugsInduction_match3 <-DrugsInduction_match3 %>%
  group_by(patid) %>%
  dplyr::slice(which.min(drugstartdate))

a<-cbind(aggregate(DrugsInduction_match3$Basiliximab, list(DrugsInduction_match3$soascaseid), FUN=sum),
         aggregate(DrugsInduction_match3$Rituximab, list(DrugsInduction_match3$soascaseid), FUN=sum)[2],
         aggregate(DrugsInduction_match3$Plasmapheresis, list(DrugsInduction_match3$soascaseid), FUN=sum)[2],
         aggregate(DrugsInduction_match3$Thymoglobulin, list(DrugsInduction_match3$soascaseid), FUN=sum)[2],
         aggregate(DrugsInduction_match3$ATG, list(DrugsInduction_match3$soascaseid), FUN=sum)[2],
         aggregate(DrugsInduction_match3$IVIG, list(DrugsInduction_match3$soascaseid), FUN=sum)[2],
         aggregate(DrugsInduction_match3$Other_Induction, list(DrugsInduction_match3$soascaseid), FUN=sum)[2])

DrugsImmunoSuppression_match3 <-DrugsImmunoSuppression_match3[DrugsImmunoSuppression_match3$drug %in% c("CsA","FK","Sirolimus","Everolimus"),] %>%
  group_by(patid) %>%
  dplyr::slice(which.min(drugstartdate))

b<-cbind(aggregate(DrugsImmunoSuppression_match3$CsA, list(DrugsImmunoSuppression_match3$soascaseid), FUN=sum),
         aggregate(DrugsImmunoSuppression_match3$FK, list(DrugsImmunoSuppression_match3$soascaseid), FUN=sum)[2],
         # aggregate(DrugsImmunoSuppression_match3$MMF, list(DrugsImmunoSuppression_match3$soascaseid), FUN=sum)[2],
         # aggregate(DrugsImmunoSuppression_match3$EC_MPA, list(DrugsImmunoSuppression_match3$soascaseid), FUN=sum)[2],
         aggregate(DrugsImmunoSuppression_match3$Sirolimus, list(DrugsImmunoSuppression_match3$soascaseid), FUN=sum)[2],
         aggregate(DrugsImmunoSuppression_match3$Everolimus, list(DrugsImmunoSuppression_match3$soascaseid), FUN=sum)[2])#,
# aggregate(DrugsImmunoSuppression_match3$Glucocorticoid, list(DrugsImmunoSuppression_match3$soascaseid), FUN=sum)[2],
# aggregate(DrugsImmunoSuppression_match3$Aza, list(DrugsImmunoSuppression_match3$soascaseid), FUN=sum)[2],
# aggregate(DrugsImmunoSuppression_match3$Other_ImmunoSuppression, list(DrugsImmunoSuppression_match3$soascaseid), FUN=sum)[2])

c<-cbind(aggregate(DrugsOther_match3$ACE_Inhibitor, list(DrugsOther_match3$soascaseid), FUN=sum),
         aggregate(DrugsOther_match3$ARB, list(DrugsOther_match3$soascaseid), FUN=sum)[2],
         aggregate(DrugsOther_match3$Beta_blocker, list(DrugsOther_match3$soascaseid), FUN=sum)[2],
         aggregate(DrugsOther_match3$CCB, list(DrugsOther_match3$soascaseid), FUN=sum)[2],
         aggregate(DrugsOther_match3$Insulin, list(DrugsOther_match3$soascaseid), FUN=sum)[2],
         aggregate(DrugsOther_match3$Oral_antidiabetics, list(DrugsOther_match3$soascaseid), FUN=sum)[2],
         aggregate(DrugsOther_match3$Platelet_inhibitior, list(DrugsOther_match3$soascaseid), FUN=sum)[2],
         aggregate(DrugsOther_match3$OAK, list(DrugsOther_match3$soascaseid), FUN=sum)[2],
         aggregate(DrugsOther_match3$Other, list(DrugsOther_match3$soascaseid), FUN=sum)[2])

names(a)<-c("soascaseid","Basiliximab","Rituximab","Plasmapheresis","Thymoglobulin","ATG","IVIG","Other_Induction")
names(b)<-c("soascaseid","CsA","FK","Sirolimus","Everolimus")
# names(b)<-c("soascaseid","CsA","FK","MMF","EC_MPA","Sirolimus","Everolimus","Glucocorticoid","Aza","Other_ImmunoSuppression")
names(c)<-c("soascaseid","ACE_Inhibitor","ARB","Beta_blocker","CCB","Insulin","Oral_antidiabetics","Platelet_inhibitior","OAK","Other")

head(a)

upset(as.data.frame(DrugsInduction_match3), sets = c("Basiliximab","Rituximab","Plasmapheresis","Thymoglobulin","ATG","IVIG","Other_Induction"), mb.ratio = c(0.55, 0.45))
upset(as.data.frame(DrugsImmunoSuppression_match3), sets = c("CsA","FK","Sirolimus","Everolimus"), mb.ratio = c(0.55, 0.45))



Patient_bl2 <- Reduce(function(x,y) merge(x,y,by=c("soascaseid"),all=TRUE) ,
                      list(Patient_bl,
                           a,b,c))

Patient_bl2 <-Patient_bl2%>%
  # mutate(Other_Induction = ifelse(Other_Induction >0, 1,0))%>%
  mutate(DrugsInduction = ifelse(Basiliximab >0, "Basiliximab",
                                 ifelse(Rituximab >0, "Rituximab",
                                        ifelse(Plasmapheresis >0, "Plasmapheresis", 
                                               ifelse(Thymoglobulin>0,"Thymoglobulin",
                                                      ifelse(ATG>0,"ATG",ifelse(IVIG>0,"IVIG","other")))))))%>%
  mutate(DrugsImmunoSuppression = ifelse(CsA >0, "CsA",ifelse(FK >0, "Tac",ifelse(Sirolimus >0 | Everolimus >0, "mTOR", "other"))))%>%
  # mutate(CsA = ifelse(CsA >0, 1,0))%>%
  # mutate(FK = ifelse(FK >0, 1,0))%>%
  # mutate(MMF = ifelse(MMF >0, 1,0))%>%
  # mutate(EC_MPA = ifelse(EC_MPA >0, 1,0))%>%
  # mutate(Sirolimus = ifelse(Sirolimus >0, 1,0))%>%
  # mutate(Everolimus = ifelse(Everolimus >0, 1,0))%>%
  # mutate(Glucocorticoid = ifelse(Glucocorticoid >0, 1,0))%>%
  # mutate(Aza = ifelse(Aza >0, 1,0))%>%
  # mutate(Other_ImmunoSuppression = ifelse(Other_ImmunoSuppression >0, 1,0))%>%
  mutate(ACE_Inhibitor = ifelse(ACE_Inhibitor >0, 1,0))%>%
  mutate(ARB = ifelse(ARB >0, 1,0))%>%
  mutate(Beta_blocker = ifelse(Beta_blocker >0, 1,0))%>%
  mutate(CCB = ifelse(CCB >0, 1,0))%>%
  mutate(Insulin = ifelse(Insulin >0, 1,0))%>%
  mutate(Oral_antidiabetics = ifelse(Oral_antidiabetics >0, 1,0))%>%
  mutate(Platelet_inhibitior = ifelse(Platelet_inhibitior >0, 1,0))%>%
  mutate(OAK = ifelse(OAK >0, 1,0))%>%
  mutate(Other = ifelse(Other >0, 1,0))

Patient_bl2$DrugsInduction<-ifelse(Patient_bl2$DrugsInduction %in% c("IVIG","other","Plasmapheresis"),"other",
                               ifelse(Patient_bl2$DrugsInduction=="Basiliximab","Basiliximab",
                                      ifelse(Patient_bl2$DrugsInduction=="Thymoglobulin","Thymoglobulin",
                                             ifelse(Patient_bl2$DrugsInduction=="Rituximab","Rituximab","ATG"))) )

write.csv(Patient_bl2,"Patient_bl2.csv", row.names = FALSE, na="")
# Save an object to a file
saveRDS(Patient_bl2, file = "Patient_bl2.rds")
