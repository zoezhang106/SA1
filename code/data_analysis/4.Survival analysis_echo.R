library(dplyr)

# Patient<-readRDS("Patient_bl2.rds")
Patient<-readRDS("Patient.rds")
#Patient <- Patient[!duplicated(Patient$soascaseid), ]

# Patient$status_graft <- ifelse(Patient$time_graft>5000,0,Patient$status_graft)
# 
# Patient[Patient$time_graft>5000,]$time_graft <-Patient[Patient$time_graft>5000,]$time_death

Patient$centreid

table(Patient$status_graft)
table(Patient$smoking)
# Patient$smoking = ifelse(Patient$smoking=="No",NA,Patient$smoking)
# Patient$smoking = ifelse(Patient$smoking=="Refused",NA,Patient$smoking)


length(unique(Patient$soascaseid)) # 676

table(Patient$death_type)
table(Patient$outtype3)

table(Patient$smoking)
Patient[which(Patient$dialysistype=='None'),]$dialysis_time = 0

Patient_all1 <- Patient
Patient_all1$cohort<-"Overall cohort"

Patient<-readRDS("Patient.rds")
Patient_all2 <- Patient_all1[Patient_all1$patid %in% Patient$patid,]
Patient_all2$cohort<-"Sub cohort with valid pre"

Patient_all3 <- Patient_all1[Patient_all1$patid %in% echo_match5$patid,]
Patient_all3$cohort<-"Sub cohort with valid pre and post"

Patient <- rbind(Patient_all3,Patient_all2,Patient_all1)
table(Patient$cohort)


library(gtsummary)
# Patient<-as.data.frame(rbind(a1,a2))
Patient$MACE_1 <- ifelse(Patient$status_MACE =="1"& Patient$time_MACE<1,"MACE","MACE free")
Patient$MACE_3 <- ifelse(Patient$status_MACE =="1"& Patient$time_MACE<3,"MACE","MACE free")
Patient$MACE_5 <- ifelse(Patient$status_MACE =="1"& Patient$time_MACE<5,"MACE","MACE free")
Patient$age_at_tpx<-as.numeric(Patient$age_at_tpx)
Patient<-as_tibble(Patient)
Patient %>%
  dplyr::select(c(MACE_1,MACE_3,MACE_5,
                  age_at_tpx, sex, ethnicity,etio_0,
                  dialysistype,dialysis_time,kidney_tpx,heart_tpx,
                  dontype,donage,donsex,sumhlamismatch,
                  BMI,sbp,dbp,
                  crea,chol,ldlchol,hdlchol,hba1c,smoking,
                  CRS_type,
                  MACE_history,
                  Myocardial_Infarction,Cerebrovascular_Event,Cardiac_Arrest,Heart_Failure,
                  Hypertension,Hyperlipidemia,Diabetes,
                  DrugsInduction,DrugsImmunoSuppression,
                  LV.ESD,LV.EDD,#LV_EDD_cal,
                  LV.EDDI,#LV.EF,
                  LV_EF,LV.posterior.wall.thickness,
                  LV_RWT_cal,LV.septum.thickness,LVMI,
                  LVMI_cal,LVMI_cal2,LV.E,LV.A,LV.E.A,
                  LAESD,LA.LAV,LA.LAVI,RV.TAPSE,LV.MAPSE,RV.EDD.long.axis,RV.EDDI.long.axis.cal
  )) %>%
  tbl_summary(
    by = MACE_history,
    statistic = list(all_continuous() ~ "{mean}",#({sd})
                     all_categorical() ~ "{n} ({p}%)"),#all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 2,
    #missing = "no",
    # label = list(
    #   age_at_tpx ~"Age",
    #   sex~"Sex",
    #   ethnicity ~"Ethnicity",
    #   etio_0 ~"Etiology",
    #   dialysistype ~"Dialysis type",
    #   dialysis_time ~"Dialysis time",
    #   kidney_tpx ~"Number of kidney tpx",
    #   heart_tpx ~"Number of heart tpx",
    #   dontype~"Donor type",
    #   donage ~"Donor age",
    #   donsex ~"Donor sex",
    #   sumhlamismatch ~"sum HLA mismatch",
    #   sbp~"Systolic blood pressure",
    #   dbp ~"Diastolic blood pressure",
    #   ldlchol ~"LDL Cholesterol",
    #   hdlchol ~"HDL Cholesterol",
    #   #MACE_history~"MACE history",
    #   Myocardial_Infarction ~"Myocardial Infarction",
    #   Cerebrovascular_Event ~"Cerebrovascular Event",
    #   Cardiac_Arrest~"Cardiac Arrest",
    #   Heart_Failure ~"Heart Failure"
    # )
  ) %>%
  add_p() %>% # comparing values by "both" column
  add_overall() %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**MACE history**")# adding spanning header


Patient[Patient$cohort !="Sub cohort with valid pre",] %>%
  dplyr::select(c(cohort,MACE_1,MACE_3,MACE_5,
    age_at_tpx, sex, ethnicity,etio_0,
    dialysistype,dialysis_time,kidney_tpx,heart_tpx,
    dontype,donage,donsex,sumhlamismatch,
    BMI,sbp,dbp,
    crea,chol,ldlchol,hdlchol,hba1c,smoking,
    CRS_type,
    MACE_history,
    Myocardial_Infarction,Cerebrovascular_Event,Cardiac_Arrest,Heart_Failure,
    Hypertension,Hyperlipidemia,Diabetes#,
    #DrugsInduction,DrugsImmunoSuppression#,
    # LV.ESD,LV.EDD,#LV_EDD_cal,
    # LV.EDDI,#LV.EF,
    # LV_EF,LV.posterior.wall.thickness,
    # LV_RWT_cal,LV.septum.thickness,LVMI,
    # LVMI_cal,LVMI_cal2,LV.E,LV.A,LV.E.A,
    # LAESD,LA.LAV,LA.LAVI,RV.TAPSE,LV.MAPSE,RV.EDD.long.axis,RV.EDDI.long.axis.cal
  )) %>%
  tbl_summary(
    by = cohort, #MACE_history,
    statistic = list(all_continuous() ~ "{mean}",#({sd})
                     all_categorical() ~ "{n} ({p}%)"),#all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 2,
    #missing = "no",
    # label = list(
    #   age_at_tpx ~"Age",
    #   sex~"Sex",
    #   ethnicity ~"Ethnicity",
    #   etio_0 ~"Etiology",
    #   dialysistype ~"Dialysis type",
    #   dialysis_time ~"Dialysis time",
    #   kidney_tpx ~"Number of kidney tpx",
    #   heart_tpx ~"Number of heart tpx",
    #   dontype~"Donor type",
    #   donage ~"Donor age",
    #   donsex ~"Donor sex",
    #   sumhlamismatch ~"sum HLA mismatch",
    #   sbp~"Systolic blood pressure",
    #   dbp ~"Diastolic blood pressure",
    #   ldlchol ~"LDL Cholesterol",
    #   hdlchol ~"HDL Cholesterol",
    #   #MACE_history~"MACE history",
    #   Myocardial_Infarction ~"Myocardial Infarction",
    #   Cerebrovascular_Event ~"Cerebrovascular Event",
    #   Cardiac_Arrest~"Cardiac Arrest",
    #   Heart_Failure ~"Heart Failure"
    # )
  ) %>%
  add_p() %>% # comparing values by "both" column
  #add_overall() %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Cohort**")# adding spanning header
  0.048# modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Cohort**")# adding spanning header
  # mod0.012ify_spanning_header(c("stat_1", "stat_2") ~ "**MACE history**")# adding spanning header

####### 1. Kaplan-Meier plots ############

library(ggsurvfit)
survfit2(Surv(time_death, status_death) ~ 1, data = Patient) %>% 
  ggsurvfit(color = "#2E9FDF") +
  #add_confidence_interval(fill = "#2E9FDF") +
  labs(
    x = "Years",
    y = "Overall survival probability"
  ) + 
  scale_x_continuous(n.breaks = 15) +
  add_confidence_interval()

survfit2(Surv(time_MACE, status_MACE) ~ 1, data = Patient) %>% 
  ggsurvfit(color = "#E7B800") +
  #add_confidence_interval(fill = "#E7B800") +
  labs(
    x = "Years",
    y = "MACE_free survival probability"
  ) + 
  scale_x_continuous(n.breaks = 15) +
  add_confidence_interval()


####### 1.2 Recurrent events ########

library(reReg)

tmp <- Patient[,c("soascaseid","time_death","outtype3","MACE_history")]  %>%
  left_join(PatientsDiseases_followup_final, by = c("soascaseid"))
tmp$status_CV_Death <- ifelse(tmp$outtype3 == "CV_death" & !is.na(tmp$outtype3),1,0)
tmp$time = as.numeric(tmp$date_in -tmp$tpxdate)/365.25

tmp2 <- tmp[,c("soascaseid","time_death","status_CV_Death","MACE_history","mace_diseases","time")]
tmp2 <- tmp2[which(tmp2$mace_diseases !="" & tmp2$time>=0),]
tmp2$mace_diseases<-1
tmp2$status_CV_Death<-0

tmp3<-Patient[,c("soascaseid","time_death","outtype3","MACE_history")]
tmp3$status_CV_Death <- ifelse(tmp3$outtype3 == "CV_death" & !is.na(tmp3$outtype3),1,0)
tmp3<-tmp3[,c("soascaseid","time_death","status_CV_Death","MACE_history")]

tmp3$mace_diseases <- 0
tmp3$time <- tmp3$time_death

df<-rbind(tmp2,tmp3)

df <- df %>%
  arrange(soascaseid, time) %>%
  group_by(soascaseid)
df<-as.data.frame(df)



reObj <- with(df, Recur(time, soascaseid, mace_diseases, status_CV_Death))

plot(reObj, cex = 1.5, xlab = "Time in years", ylab = "Patients", 
     terminal.name = "Death", 
     recurrent.name = "MACE")

plotEvents(Recur(time, soascaseid, mace_diseases, status_CV_Death) ~ MACE_history, data = df, base_size = 10)

####### 2. donut plot ############


library(ggplot2)
library(webr)
Patient$outtype2 <- ifelse(Patient$outtype3 == "CV_death", "CV_death","Non-CV_death")
PD = Patient[Patient$death_type == "Death",] %>% 
  group_by(outtype2, outtype3) %>% 
  dplyr::summarise(n = n())

names(PD)[1]<-"Causes"
names(PD)[2]<-"status_type"
#PD = as.data.frame(PD[c(2,3,4,5,7,8,9),])
# PD[PD$outtype3=="Infection disease",]$n = 81
# PD[PD$outtype3=="Other",]$n = 67
PD = as.data.frame(PD)
PD


PD$status_type<-c("Cardiovascular disease","Accident","Cancer","Graft failure","Infection disease","Suicide","Unknown","Other")
PieDonut(PD, aes(Causes, status_type, count=n), 
         start=3*pi/2,
         #explode=1,
         r0=0.2,r1=0.8,
         ratioByGroup=FALSE,
         #selected=c(2,3,4),
         explodePie = TRUE,
         explodeDonut=TRUE,showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.0001),
         title = "Causes of death",titlesize = 5,
         pieLabelSize = 3,donutLabelSize = 3,
         labelposition = getOption("PieDonut.labelposition", 1)
)




####### 3. missing ############

# 3.1 imputation NA --------------

library(mice)
library(VIM)

# 3.1 selection columns --------------
Patient2 <-Patient %>%
  dplyr::select(c(patid,soascaseid,time_MACE,status_MACE,MACE_type,time_graft,status_graft,
                  time_death,status_death,
                  Year,center,
                  age_at_tpx, sex, ethnicity,etio_0,coldisch,
                  dialysistype,dialysis_time,kidney_tpx,heart_tpx,
                  dontype,donage,donsex,sumhlamismatch,
                  BMI,sbp,dbp,
                  crea,chol,ldlchol,hdlchol,
                  hba1c,smoking,CRS_type,
                  Myocardial_Infarction,Cerebrovascular_Event,Cardiac_Arrest,Heart_Failure,
                  Hypertension,Hyperlipidemia,Diabetes,
                  DrugsInduction,DrugsImmunoSuppression,
                  LV.ESD,LV.EDD,#LV_EDD_cal,
                  LV.EDDI,#LV.EF,
                  LV_EF,LV.posterior.wall.thickness,
                  LV_RWT_cal,LV.septum.thickness,LVMI,
                  #LVMI_cal,LVMI_cal2,
                  LV.E,LV.A,LV.E.A,
                  LAESD,RV.TAPSE#,LV.MAPSE,RV.EDD.long.axis,RV.EDDI.long.axis.cal
  ))

Patient2 <- Patient2 %>% dplyr::rename("Age" = "age_at_tpx",
                                       "Sex" = "sex",
                                       "Ethnicity" = "ethnicity",
                                       "Etiology" = "etio_0",
                                       "Donor_type" = "dontype",
                                       "Donor_age" = "donage",
                                       "Donor_sex" = "donsex",
                                       "Cold_ischemia" = "coldisch",
                                       "Systolic_blood_pressure" = "sbp",
                                       "Diastolic_blood_pressure" = "dbp",
                                       "Creatinine" = "crea",
                                       "Cholesterol" = "chol",
                                       "LDL_Cholesterol" = "ldlchol",
                                       "HDL_Cholesterol" = "hdlchol",
                                       "HbA1c" = "hba1c",
                                       "Smoking" = "smoking",
                                       "Dialysis_Type" = "dialysistype",
                                       "Dialysis_Time" = "dialysis_time",
                                       "sum_HLA_mismatch" = "sumhlamismatch")

Patient2$DrugsInduction<-ifelse(Patient2$DrugsInduction %in% c("IVIG","other","Plasmapheresis"),"other",ifelse(Patient2$DrugsInduction=="Basiliximab","Basiliximab",ifelse(Patient2$DrugsInduction=="Thymoglobulin","Thymoglobulin",ifelse(Patient2$DrugsInduction=="Rituximab","Rituximab","ATG"))) )

categorical_vars<-c("status_MACE","status_graft","Sex","Ethnicity","Etiology",
                    "Dialysis_Type","Donor_type","Donor_sex","sum_HLA_mismatch","Smoking","Myocardial_Infarction","Cerebrovascular_Event","Cardiac_Arrest","Heart_Failure","Hypertension","Hyperlipidemia","Diabetes","DrugsInduction","DrugsImmunoSuppression","CRS_type")

Patient2 <- as.data.frame(Patient2)

for (i in categorical_vars) {
  print(i)
  Patient2[,i] <- as.factor(Patient2[,i])
}

Patient2$DrugsInduction = relevel(Patient2$DrugsInduction, ref = "Basiliximab")
Patient2$DrugsInduction <- factor(Patient2$DrugsInduction, 
                                  levels=c('Basiliximab', 'Rituximab', 'Thymoglobulin', 'ATG','other'))

Patient2$DrugsImmunoSuppression = relevel(Patient2$DrugsImmunoSuppression, ref = "Tac")
Patient2$DrugsImmunoSuppression <- factor(Patient2$DrugsImmunoSuppression, 
                                          levels=c('Tac', 'CsA', 'mTOR','other'))
Patient2$CRS_type = relevel(Patient2$CRS_type, ref = "ESRD")

Patient2$Sex = relevel(Patient2$Sex, ref = "Female")
Patient2$Ethnicity = relevel(Patient2$Ethnicity, ref = "Caucasian")
Patient2$Etiology = relevel(Patient2$Etiology, ref = "GN")
Patient2$Dialysis_Type = relevel(Patient2$Dialysis_Type, ref = "None")
Patient2$Donor_type = relevel(Patient2$Donor_type, ref = "Living related")
Patient2$Donor_sex = relevel(Patient2$Donor_sex, ref = "Female")
Patient2$sum_HLA_mismatch = relevel(Patient2$sum_HLA_mismatch, ref = "0")

Patient2$Myocardial_Infarction = relevel(Patient2$Myocardial_Infarction, ref = "No")
Patient2$Cerebrovascular_Event = relevel(Patient2$Cerebrovascular_Event, ref = "No")
Patient2$Cardiac_Arrest = relevel(Patient2$Cardiac_Arrest, ref = "No")
Patient2$Heart_Failure = relevel(Patient2$Heart_Failure, ref = "No")
Patient2$Hypertension = relevel(Patient2$Hypertension, ref = "No")
Patient2$Hyperlipidemia = relevel(Patient2$Hyperlipidemia, ref = "No")
Patient2$Diabetes = relevel(Patient2$Diabetes, ref = "No")
Patient2$Smoking = relevel(Patient2$Smoking, ref = "Never")

matrixplot(Patient2, cex.axis = 0.5)
#mice:::find.collinear(Patient2)

imp<-mice(Patient2,m=5, method='cart',seed = 1234)
Patient3 <- complete(imp,1)
matrixplot(Patient3, cex.axis = 0.6)
dim(Patient3)
summary(Patient3)


# imp<-mice(Patient2[,c(1:43)],m=5, method='cart',seed = 1234)
# Patient3 <- complete(imp,1)
# Patient3 <- cbind(Patient3,Patient2[,c(44:59)])
# matrixplot(Patient3, cex.axis = 0.6)
# dim(Patient3)
# summary(Patient3)

# 3.2 (Alternative) delete patients if the data is incomplete  --------------
# Patient4 <-na.omit(Patient2)
# dim(Patient2)
# dim(Patient4)


percentage_missing <- function(x) {
  round(sum(is.na(x)) / length(x),4 )*100
}

# Apply the function to each column
missing_percentages <- sapply(Patient2, percentage_missing)

# Printing the list of percentages
missing_table <- data.frame(Column = names(missing_percentages), 
                            PercentageMissing = missing_percentages)#[c(3:41),]
missing_table

####### 4. original vs log  ############

my_list = c("BMI","sbp","dbp",
            "crea","chol","ldlchol","hdlchol","hba1c")

library(ggplot2)
library(ggpubr)
library(cowplot)
Patient_back = Patient
max(Patient$hba1c, na.rm = T)

for (Value in my_list){
  
  p1 <- gghistogram(
    Patient_back, x = Value, y = "..density..",
    add = "mean", rug = TRUE,
    fill = "MACE_history", palette = c("#00AFBB", "#E7B800"),
    add_density = TRUE
  )+
    ggtitle("Original Distribution")
  
  # Adding log-transformed column
  Patient_back[,Value] <- log(Patient_back[,Value])
  
  # Plotting the log-transformed distribution
  p2 <- gghistogram(
    Patient_back, x = Value, y = "..density..",
    add = "mean", rug = TRUE,
    fill = "MACE_history", palette = c("#00AFBB", "#E7B800"),
    add_density = TRUE
  )+
    ggtitle("Log-Transformed Distribution")
  
  # Display plots
  gridExtra::grid.arrange(p1, p2, ncol = 2)
}



####### 5. Cox Proportional-Hazards Regression for Survival Data  ############


library(survival)
library(survminer)

Patient4 <- Patient3 %>% dplyr::rename(#"Age" = "age",
  #"Sex" = "sex",
  #"Ethnicity" = "ethnicity",
  "Donor type" = "Donor_type",
  "Donor age" = "Donor_age",
  "Donor sex" = "Donor_sex",
  "Cold ischemia" = "Cold_ischemia",
  "Systolic blood pressure" = "Systolic_blood_pressure",
  "Diastolic blood pressure" = "Diastolic_blood_pressure",
  #"Creatinine" = "crea",
  #"Cholesterol" = "chol",
  "LDL Cholesterol" = "LDL_Cholesterol",
  "HDL Cholesterol" = "HDL_Cholesterol",
  #"HbA1c" = "hba1c",
  #"Smoking" = "smoking",
  #"Etiology" = "etiology",
  "Dialysis Type" = "Dialysis_Type",
  "Dialysis Time" = "Dialysis_Time",
  "Kidney tpx" = "kidney_tpx" ,
  "Pre heart tpx" = "heart_tpx" ,
  "Myocardial Infarction" = "Myocardial_Infarction",
  "Cerebrovascular Event" = "Cerebrovascular_Event",
  "Cardiac Arrest" = "Cardiac_Arrest",
  "Heart Failure" = "Heart_Failure",
  # "Hypertension" ="Hypertension",
  # "Hyperlipidemia" ="Hyperlipidemia",
  # "Diabetes" ="Diabetes",
  "sum HLA mismatch" = "sum_HLA_mismatch"
)
############
Patient4$center2<-as.factor(ifelse(Patient4$center=="Bern_new2" | Patient4$center=="Bern_old",
                         "Bern",
                         ifelse(Patient4$center=="KSSG_new" | Patient4$center=="KSSG_old",
                                "KSSG",Patient4$center)))

Patient4$LV_EF2<-ifelse(Patient4$LV_EF>55,"normal","abnormal")
Patient4$LVMI2<-ifelse(Patient4$Sex=="Male",
                      ifelse(Patient4$LVMI<115,"normal","abnormal"),
                      ifelse(Patient4$LVMI<95,"normal","abnormal"))

Patient4$LV_RWT_cal2<-ifelse(Patient4$LV_RWT_cal<0.42,"normal","abnormal")
Patient4$LV.E.A2<-ifelse(Patient4$LV.E.A<1.5 & Patient4$LV.E.A>0.8,"normal","abnormal")
Patient4$RV.TAPSE2<-ifelse(Patient4$RV.TAPSE>17,"normal","abnormal")

Patient4$LV.ESD2<-ifelse(Patient4$Sex=="Male",
                       ifelse(Patient4$LV.ESD<4.0,"normal","abnormal"),
                       ifelse(Patient4$LV.ESD<3.5,"normal","abnormal"))


Patient4$LV.EDD2<-ifelse(Patient4$Sex=="Male",
                       ifelse(Patient4$LV.EDD<5.8,"normal","abnormal"),
                       ifelse(Patient4$LV.EDD<5.2,"normal","abnormal"))

Patient4$LV.EDDI2<-ifelse(Patient4$Sex=="Male",
                       ifelse(Patient4$LV.EDDI<3.0,"normal","abnormal"),
                       ifelse(Patient4$LV.EDDI<3.1,"normal","abnormal"))

Patient4$LV.posterior.wall.thickness2<-ifelse(Patient4$Sex=="Male",
                                              ifelse(Patient4$LV.posterior.wall.thickness<10,"normal","abnormal"),
                                              ifelse(Patient4$LV.posterior.wall.thickness<9,"normal","abnormal"))

Patient4$LV.septum.thickness2<-ifelse(Patient4$Sex=="Male",
                       ifelse(Patient4$LV.septum.thickness<10,"normal","abnormal"),
                       ifelse(Patient4$LV.septum.thickness<9,"normal","abnormal"))


Patient4$LAESD2<-ifelse(Patient4$Sex=="Male",
                         ifelse(Patient4$LAESD<4.0,"normal","abnormal"),
                         ifelse(Patient4$LAESD<3.8,"normal","abnormal"))



ggplot(Patient, aes(x = LV.ESD, fill = sex, color = sex)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Values by Group",
       x = "Risk score",
       y = "Density"
  ) 
  
  
for (i in names(Patient4)[57:68]) {
  print(i)
  Patient4[,i] <- as.factor(Patient4[,i])
}
table(Patient4$center2)
Patient4$center2 = relevel(Patient4$center2, ref = "USZ")
Patient4$LAESD2 = relevel(Patient4$LAESD2, ref = "normal")
Patient4$LV.septum.thickness2 = relevel(Patient4$LV.septum.thickness2, ref = "normal")
Patient4$LV.posterior.wall.thickness2 = relevel(Patient4$LV.posterior.wall.thickness2, ref = "normal")

Patient4$LV.ESD2 = relevel(Patient4$LV.ESD2, ref = "normal")
Patient4$LV.EDD2 = relevel(Patient4$LV.EDD2, ref = "normal")
Patient4$LV.EDDI2 = relevel(Patient4$LV.EDDI2, ref = "normal")
Patient4$LV_EF2 = relevel(Patient4$LV_EF2, ref = "normal")
Patient4$LVMI2 = relevel(Patient4$LVMI2, ref = "normal")
Patient4$LV_RWT_cal2 = relevel(Patient4$LV_RWT_cal2, ref = "normal")
Patient4$LV.E.A2 = relevel(Patient4$LV.E.A2, ref = "normal")
Patient4$RV.TAPSE2 = relevel(Patient4$RV.TAPSE2, ref = "normal")

Patient4$status_MACE_1 <- ifelse(Patient4$status_MACE =="1"& Patient4$time_MACE<1,1,0)
Patient4$time_MACE_1 <- ifelse(Patient4$status_MACE =="1"& Patient4$time_MACE<1,Patient4$time_MACE,1)

for (i in names(Patient4)[58:62]){
# for (i in names(Patient4)[43:56]){
  print(i)
  df <- Patient4[!is.na(Patient4[,i]),]
  df[,i] <- as.factor(df[,i])
  df[,i] = relevel(df[,i], ref = "normal")
  
  print(nrow(df))
  
  fitformal<-Surv(time_MACE_90, status_MACE_90=="1") ~  
    #CRS_type+#Etiology+#`sum HLA mismatch`+center+
    Age+Sex+Ethnicity+`Donor age`+`Donor sex`+`Donor type`+`Cold ischemia`+BMI+`Systolic blood pressure`+`Diastolic blood pressure`+Creatinine+Cholesterol+`LDL Cholesterol`+`HDL Cholesterol`+HbA1c+
    Smoking+ `Dialysis Type`+`Dialysis Time`+`Kidney tpx`+`Pre heart tpx`+ `Myocardial Infarction`+`Cerebrovascular Event`+`Cardiac Arrest`+`Heart Failure`+Hypertension+Hyperlipidemia+Diabetes+DrugsInduction+DrugsImmunoSuppression
  # Append the additional variable to the formula
  fitformal2 <- as.formula(paste(paste(deparse(fitformal), collapse = ""), "+", i))
  
  # Print the final formula to verify
  # print(fitformal2)
  
  res.cox <- coxph(fitformal, data = df,x=TRUE,y=TRUE,na.action = na.exclude)
  print(summary(res.cox)$concordance)
  
  res.cox2 <- coxph(fitformal2, data = df,x=TRUE,y=TRUE,na.action = na.exclude)
  print(summary(res.cox2)$concordance)
  # ggforest3(res.cox2,cpositions=c(0.01, 0.20, 0.42),ref=NA, font.x.size = 12)
  # print(forest_model(res.cox2, limits=log( c(.5, 5) ) ))
 
}


############

Patient4$status_MACE_90 <- ifelse(Patient4$status_MACE =="1"& Patient4$time_MACE<90/365.25,1,0)
Patient4$time_MACE_90 <- ifelse(Patient4$status_MACE =="1"& Patient4$time_MACE<90/365.25,Patient4$time_MACE,90/365.25)

Patient4$status_MACE_5 <- ifelse(Patient4$status_MACE =="1"& Patient4$time_MACE<5,1,0)
Patient4$time_MACE_5 <- ifelse(Patient4$status_MACE =="1"& Patient4$time_MACE<5,Patient4$time_MACE,5)

fitformal<-Surv(time_MACE_90, status_MACE_90=="1") ~ 
  #CRS_type+
  Age+Sex+#Ethnicity+
  #Etiology+
  `Donor age`+`Donor sex`+`Donor type`+`sum HLA mismatch`+
  `Cold ischemia`+
  `Systolic blood pressure`+`Diastolic blood pressure`+
  Creatinine+Cholesterol+`LDL Cholesterol`+`HDL Cholesterol`+HbA1c+
  Smoking+
  `Dialysis Type`+
  `Dialysis Time`+
  `Kidney tpx`+`Pre heart tpx`+
  `Myocardial Infarction`+`Cerebrovascular Event`+`Cardiac Arrest`+`Heart Failure`+Hypertension+Hyperlipidemia+Diabetes+`sum HLA mismatch`+DrugsInduction+#DrugsImmunoSuppression+
  LV.ESD+LV.EDD+
  LV.EDDI+
  LV_EF+LV.posterior.wall.thickness+
  LV_RWT_cal+LV.septum.thickness+LVMI+
  LAESD+RV.TAPSE

fitformal<-Surv(time_MACE, status_MACE=="1") ~  
  #CRS_type+
  #center2+
  Age+Sex+Ethnicity+
  #Etiology+
  `Donor age`+`Donor sex`+`Donor type`+#`sum HLA mismatch`+
  `Cold ischemia`+
  BMI+`Systolic blood pressure`+`Diastolic blood pressure`+
  Creatinine+Cholesterol+`LDL Cholesterol`+`HDL Cholesterol`+HbA1c+
  Smoking+
  `Dialysis Type`+
  `Dialysis Time`+
  `Pre kidney tpx`+`Pre heart tpx`+
  `Myocardial Infarction`+`Cerebrovascular Event`+`Cardiac Arrest`+`Heart Failure`+
  # LV.ESD+LV.EDD+
  # LV.EDDI+
  # LV_EF+LV.posterior.wall.thickness+
  # LV_RWT_cal+LV.septum.thickness+LVMI+
  # LAESD+RV.TAPSE+
  Hypertension+Hyperlipidemia+Diabetes+
  DrugsInduction+DrugsImmunoSuppression#+

fitformal<-Surv(time_MACE, status_MACE=="1") ~  
  #CRS_type+
  #center+
  Age+#Sex+Ethnicity+
  #Etiology+
  #`Donor age`+#`Donor sex`+`Donor type`+
  #`sum HLA mismatch`+
  #`Cold ischemia`+
  `Systolic blood pressure`+
  #`Diastolic blood pressure`+
  #Creatinine+Cholesterol+`LDL Cholesterol`+`HDL Cholesterol`+
  #HbA1c+
  #Smoking+
  #`Dialysis Type`+
  #`Dialysis Time`+
  `Pre kidney tpx`+`Pre heart tpx`+
  `Myocardial Infarction`+`Cerebrovascular Event`+`Cardiac Arrest`+#`Heart Failure`+
  #Hypertension#+#Hyperlipidemia+
  Diabetes+#DrugsInduction+DrugsImmunoSuppression#+
  LV.ESD+LV.EDD+
  #LV.EDDI+
  LV.posterior.wall.thickness+LV.septum.thickness+
  #LV.RWT+
  LVMI+LV_EF+LAESD+RV.TAPSE


Patient4$`Pre kidney tpx` <-Patient4$`Kidney tpx`
Patient4$LV.RWT<-Patient4$LV_RWT_cal
Patient4$LV.RWT2<-Patient4$LV_RWT_cal2

fitformal<-Surv(time_MACE, status_MACE=="1") ~  
  #CRS_type+
  #center+
  Age+#Sex+Ethnicity+
  #Etiology+
  #`Donor age`+#`Donor sex`+`Donor type`+
  #`sum HLA mismatch`+
  #`Cold ischemia`+
  `Systolic blood pressure`+
  #`Diastolic blood pressure`+
  #Creatinine+Cholesterol+`LDL Cholesterol`+`HDL Cholesterol`+
  #HbA1c+
  #Smoking+
  #`Dialysis Type`+
  #`Dialysis Time`+
  `Pre kidney tpx`+`Pre heart tpx`+
  `Myocardial Infarction`+`Cerebrovascular Event`+`Cardiac Arrest`+#`Heart Failure`+
  #Hypertension#+#Hyperlipidemia+
  Diabetes+#DrugsInduction+DrugsImmunoSuppression#+
  LV.ESD2+LV.EDD2+
  # LV.EDDI2+
  LV.posterior.wall.thickness2+LV.septum.thickness2+
  #LV.RWT2+
  LVMI2+LV_EF2+LAESD2+RV.TAPSE2

fitformal<-Surv(time_MACE, status_MACE=="1") ~  
  #CRS_type+
  #center2+
  Age+Sex+Ethnicity+
  #Etiology+
  `Donor age`+`Donor sex`+`Donor type`+`sum HLA mismatch`+
  `Cold ischemia`+
  `Systolic blood pressure`+`Diastolic blood pressure`+
  Creatinine+Cholesterol+`LDL Cholesterol`+`HDL Cholesterol`+HbA1c+
  Smoking+
  `Dialysis Type`+
  `Dialysis Time`+
  `Pre kidney tpx`+`Pre heart tpx`+
  `Myocardial Infarction`+`Cerebrovascular Event`+`Cardiac Arrest`+`Heart Failure`+
  Hypertension+Hyperlipidemia+Diabetes+`sum HLA mismatch`+
  DrugsInduction+DrugsImmunoSuppression+
  LV.ESD+LV.EDD+
  LV.EDDI+
  LV.posterior.wall.thickness+LV.septum.thickness+
  LV.RWT+
  LVMI+LV_EF+LAESD+RV.TAPSE

#library(tramME)
fit <- CoxphME(sui | ages + mmrcs + fev1pps ~ 1 + (ages + mmrcs + fev1pps | cohort),
               data = data3CIA, log_first = TRUE, order = 4, support = c(1, 150),
               control = optim_control(iter.max = 1e5, eval.max = 1e5, rel.tol = 1e-8))

fitformal<-Surv(time_MACE, status_MACE=="1") ~ (Age+Sex+Ethnicity | center)


fit <- CoxphME(fitformal, data = Patient4,x=TRUE,y=TRUE,na.action = na.exclude)
stopifnot(fit$opt$convergence == 0)
re_str <- attr(fit$param, "re")
pr <- list(beta = coef(fit, with_baseline = TRUE), theta = varcov(fit, as.theta = TRUE))

Patient4$Center<-ifelse(Patient4$center %in% c("KSSG_new","KSSG_old"),"KSSG",Patient4$center)
Patient4$Center<-ifelse(Patient4$Center %in% c("Bern_new2","Bern_old"),"Bern",Patient4$Center)


res.cox <- coxph(fitformal, data = Patient4,x=TRUE,y=TRUE,na.action = na.exclude)
summary(res.cox)$concordance

# ggforest(res.cox,cpositions=c(0.01, 0.20, 0.4),ref=NA)
# ggforest3(res.cox,cpositions=c(0.01, 0.20, 0.42),ref=NA, font.x.size = 12)


library(forestmodel)
print(forest_model(res.cox, limits=log( c(.5, 5) ) ))

library(MASS)
step.res.cox<-stepAIC(res.cox, direction = "both", trace = FALSE)
summary(step.res.cox)$concordance

# ggforest3(step.res.cox,cpositions=c(0.01, 0.25, 0.4),ref=NA, fontsize = 1, font.x.size = 12)
# ggforest(step.res.cox,cpositions=c(0.01, 0.20, 0.4),ref=NA)

print(forest_model(step.res.cox, limits=log( c(.5, 5) ) ))


####### 6. Competing  ############
#Patient4 <- Patient3
# dim(Patient4)
# length(unique(Patient4$soascaseid))
# 
# # Assuming df is your data frame and column_name is the column you're checking for duplicates
# Patient4 <- Patient4[!duplicated(Patient4$soascaseid), ]
# dim(Patient4)

Patient4$status_MACE2 <-0
Patient4$time_MACE2 <-0

for (i in Patient4$soascaseid){
  if (Patient4[Patient4$soascaseid ==i,]$status_MACE == 1){
    Patient4[Patient4$soascaseid ==i,]$status_MACE2 <- 1
    Patient4[Patient4$soascaseid ==i,]$time_MACE2 <- Patient4[Patient4$soascaseid ==i,]$time_MACE
  }else{
    if (Patient4[Patient4$soascaseid ==i,]$status_graft == 1){
      Patient4[Patient4$soascaseid ==i,]$status_MACE2 <- 2
      Patient4[Patient4$soascaseid ==i,]$time_MACE2 <- Patient4[Patient4$soascaseid ==i,]$time_graft
    }else{
      if (Patient4[Patient4$soascaseid ==i,]$status_death == 1){
        Patient4[Patient4$soascaseid ==i,]$status_MACE2 <- 3
        Patient4[Patient4$soascaseid ==i,]$time_MACE2 <- Patient4[Patient4$soascaseid ==i,]$time_death
      }else{
        #print(Patient4[Patient4$soascaseid ==i,]$status_MACE)
        Patient4[Patient4$soascaseid ==i,]$status_MACE2 <- 0
        Patient4[Patient4$soascaseid ==i,]$time_MACE2 <- Patient4[Patient4$soascaseid ==i,]$time_MACE
      }
    }
  }
}

table(Patient4$status_MACE2)
#Patient4$status_MACE2 <-as.factor(Patient4$status_MACE2)
summary(Patient4$time_MACE2)

Patient5 <- Patient4

# Replace spaces with underscores in column names
colnames(Patient5) <- gsub(" ", "_", colnames(Patient5))

library(autoReg)
gaze(Patient5,show.n=TRUE)

fit=survfit(Surv(time_MACE2,status_MACE2!=0) ~ 1,data=Patient5)
#summary(fit,times=0:5)
ggsurvplot(fit)


# MACE-specific survival
fitformal<-Surv(time_MACE2, status_MACE2 ==1) ~  
  #CRS_type+
  Age+Sex+Ethnicity+
  #Etiology+
  `Donor_age`+`Donor_sex`+`Donor_type`+`sum_HLA_mismatch`+
  `Cold_ischemia`+
  `Systolic_blood_pressure`+`Diastolic_blood_pressure`+
  Creatinine+Cholesterol+`LDL_Cholesterol`+`HDL_Cholesterol`+HbA1c+
  Smoking+
  `Dialysis_Type`+
  `Dialysis_Time`+
  `Kidney_tpx`+`Pre_heart_tpx`+
  `Myocardial_Infarction`+`Cerebrovascular_Event`+`Cardiac_Arrest`+`Heart_Failure`+Hypertension+Hyperlipidemia+Diabetes+DrugsInduction+DrugsImmunoSuppression+
  LV.ESD+LV.EDD+
  LV.EDDI+
  LV_EF+LV.posterior.wall.thickness+
  LV_RWT_cal+LV.septum.thickness+LVMI+
  LAESD+RV.TAPSE


fit<- coxph(fitformal, data = Patient5,x=TRUE,y=TRUE,na.action = na.exclude)
summary(fit)$concordance

x=autoReg(fit,uni=TRUE,threshold=1)
x %>% myft()
#modelPlot(fit,uni=TRUE, threshold=1,show.ref=FALSE,widths=c(1,0,1,1))

# Testing for proportional hazards

result=cox.zph(fit)
result
coxzphplot(result)

# Testing influential observations
ggcoxdiagnostics(fit, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())

# Testing non linearity
ggcoxfunctional(Surv(time_MACE2, status_MACE2) ~ Age + log(Age) + sqrt(Age) + I(Age^2)+ I(Age^3), data = Patient4)
ggcoxfunctional(Surv(time_MACE2, status_MACE2) ~ LVMI + log(LVMI) + sqrt(LVMI)+ I(LVMI^2)+ I(LVMI^3), data = Patient4)
ggcoxfunctional(Surv(time_MACE2, status_MACE2) ~ LV_RWT_cal + log(LV_RWT_cal) + sqrt(LV_RWT_cal) + I(LV_RWT_cal^2)+ I(LV_RWT_cal^3), data = Patient4)

# Competing-risks regression is an alternative to cox proportional regression

fit1=crrFormula(time_MACE2+status_MACE2~
                  #CRS_type+
                  Age+Sex+Ethnicity+
                  #Etiology+
                  `Donor_age`+`Donor_sex`+`Donor_type`+`sum_HLA_mismatch`+
                  `Cold_ischemia`+
                  `Systolic_blood_pressure`+`Diastolic_blood_pressure`+
                  Creatinine+Cholesterol+`LDL_Cholesterol`+`HDL_Cholesterol`+HbA1c+
                  Smoking+
                  `Dialysis_Type`+
                  `Dialysis_Time`+
                  `Kidney_tpx`+`Pre_heart_tpx`+
                  `Myocardial_Infarction`+`Cerebrovascular_Event`+`Cardiac_Arrest`+`Heart_Failure`+Hypertension+Hyperlipidemia+Diabetes+DrugsInduction+DrugsImmunoSuppression+
                  LV.ESD+LV.EDD+
                  LV.EDDI+
                  LV_EF+LV.posterior.wall.thickness+
                  LV_RWT_cal+LV.septum.thickness+LVMI+
                  LAESD+RV.TAPSE,
                cencode = 0,
                data=Patient5)

autoReg(fit,uni=TRUE,threshold=1) %>% 
  addFitSummary(fit1,"HR (competing risks multivariable)") %>% 
  myft()


library(cmprsk) # for use of cuminc()
fit=cuminc(Patient4$time_MACE2,Patient5$status_MACE2)
fit


ggcmprsk(time_MACE2+status_MACE2~1,data=Patient5,
         id=c("alive","MACE","graft loss","death"),
         se=TRUE)

ggcmprsk(time_MACE2+status_MACE2~Sex,data=Patient5,
         id=c("alive","MACE","graft loss","death"),
         strata=c("female","male"))

ggcmprsk(time_MACE2+status_MACE2~CRS_type,data=Patient5,
         id=c("alive","MACE","graft loss","death"))


############

