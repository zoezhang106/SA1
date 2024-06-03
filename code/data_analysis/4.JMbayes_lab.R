library(JMbayes2)

data1 <- LabChemistry_followup_final %>%
  left_join(Patient_bl, by = c("patid","soascaseid","centreid","tpxdate","graft_date"))
dim(data1)#632

data1 <- data1%>%
  add_column("time_ass_y" = as.numeric(data1$assdate.x-data1$tpxdate)/365.25, .after="assdate.x")%>%
  relocate(soascaseid,tpxdate,graft_date, .before="assdate.x")

data1<-as.data.frame(data1)%>%
  dplyr::arrange(soascaseid,time_ass_y)

categorical_vars<-c("sex","ethnicity",
                    "dialysistype","dontype","donsex","sumhlamismatch","smoking","Myocardial_Infarction","Cerebrovascular_Event","Cardiac_Arrest","Heart_Failure","Hypertension","Hyperlipidemia","Diabetes")

for (i in categorical_vars) {
  print(i)
  data1[,i] <- as.factor(data1[,i])
}

data1$sex = relevel(data1$sex, ref = "Female")
data1$ethnicity = relevel(data1$ethnicity, ref = "Caucasian")
# data1$Etiology = relevel(data1$Etiology, ref = "GN")
data1$dialysistype = relevel(data1$dialysistype, ref = "None")
data1$dontype = relevel(data1$dontype, ref = "Living related")
data1$donsex = relevel(data1$donsex, ref = "Female")
data1$sumhlamismatch = relevel(data1$sumhlamismatch, ref = "0")

data1$Myocardial_Infarction = relevel(data1$Myocardial_Infarction, ref = "Yes")
data1$Cerebrovascular_Event = relevel(data1$Cerebrovascular_Event, ref = "Yes")
data1$Cardiac_Arrest = relevel(data1$Cardiac_Arrest, ref = "Yes")
data1$Heart_Failure = relevel(data1$Heart_Failure, ref = "Yes")
data1$Hypertension = relevel(data1$Hypertension, ref = "Yes")
data1$Hyperlipidemia = relevel(data1$Hyperlipidemia, ref = "Yes")
data1$Diabetes = relevel(data1$Diabetes, ref = "Yes")

#data1 <- data1[c(1:200),]

data1$time_ass_y<-ifelse(data1$time_ass_y<0,0,data1$time_ass_y)
data1.id <-data1%>% 
  group_by(soascaseid) %>%
  dplyr::slice(which.min(time_ass_y))



CoxFit <- coxph(Surv(time_MACE, status_MACE) ~ age_at_tpx+sex+dialysistype+dontype+Myocardial_Infarction+Cerebrovascular_Event+Cardiac_Arrest+Heart_Failure+Hypertension+Hyperlipidemia+Diabetes, data = data1.id, id = soascaseid)

fm1 <- lme(crea.x ~ time_ass_y, data = data1, random = ~ time_ass_y | soascaseid, na.action=na.exclude)
fm2 <- lme(chol.x ~ time_ass_y, data = data1, random = ~ time_ass_y | soascaseid, na.action=na.exclude)
fm3 <- lme(ldlchol.x ~ time_ass_y, data = data1, random = ~ time_ass_y | soascaseid, na.action=na.exclude)
fm4 <- lme(hdlchol.x ~ time_ass_y, data = data1, random = ~ time_ass_y | soascaseid, na.action=na.exclude)
fm5 <- lme(hba1c.x ~ time_ass_y, data = data1, random = ~ time_ass_y | soascaseid, na.action=na.exclude)

# "BSA","ldlchol.x","hdlchol.x","hba1c.x","LV_EF","LV.posterior.wall.thickness","LV.septum.thickness","crea.x","chol.x"

jointFit <- jm(CoxFit, list(fm1,fm2,fm3,fm4,fm5), time_var = "time_ass_y",
               n_iter = 2500L, n_burnin = 500L, n_thin = 5L)
summary(jointFit)
jointFit_mace1<-jointFit


fForms <- list(
  "crea.x" = ~ value(crea.x) + slope(crea.x),
  "chol.x" = ~ value(chol.x) + slope(chol.x),
  "ldlchol.x" = ~ value(ldlchol.x) + slope(ldlchol.x),
  "hdlchol.x" = ~ value(hdlchol.x) + slope(hdlchol.x),
  "hba1c.x" = ~ value(hba1c.x) + slope(hba1c.x)
)

jointFit2 <- update(jointFit, functional_forms = fForms)
summary(jointFit2)

jointFit_mace2<-jointFit2


fForms <- list(
  "crea.x" = ~ value(crea.x) + area(crea.x),
  "chol.x" = ~ value(chol.x) + area(chol.x),
  "ldlchol.x" = ~ value(ldlchol.x) + area(ldlchol.x),
  "hdlchol.x" = ~ value(hdlchol.x) + area(hdlchol.x),
  "hba1c.x" = ~ value(hba1c.x) + area(hba1c.x)
)

jointFit3 <- update(jointFit, functional_forms = fForms)
summary(jointFit3)

##############

auc1_1<-tvAUC(tvROC(jointFit, newdata = data1, Tstart = 1, Dt = 5, cores = 1L))$auc
auc1_2<-tvAUC(tvROC(jointFit, newdata = data1, Tstart = 2, Dt = 5, cores = 1L))$auc
auc1_3<-tvAUC(tvROC(jointFit, newdata = data1, Tstart = 3, Dt = 5, cores = 1L))$auc
auc1_4<-tvAUC(tvROC(jointFit, newdata = data1, Tstart = 4, Dt = 5, cores = 1L))$auc
auc1_5<-tvAUC(tvROC(jointFit, newdata = data1, Tstart = 5, Dt = 4.9, cores = 1L))$auc

auc2_1<-tvAUC(tvROC(jointFit2, newdata = data1, Tstart = 1, Dt = 5, cores = 1L))$auc
auc2_2<-tvAUC(tvROC(jointFit2, newdata = data1, Tstart = 2, Dt = 5, cores = 1L))$auc
auc2_3<-tvAUC(tvROC(jointFit2, newdata = data1, Tstart = 3, Dt = 5, cores = 1L))$auc
auc2_4<-tvAUC(tvROC(jointFit2, newdata = data1, Tstart = 4, Dt = 5, cores = 1L))$auc
auc2_5<-tvAUC(tvROC(jointFit2, newdata = data1, Tstart = 5, Dt = 4.9, cores = 1L))$auc

auc3_1<-tvAUC(tvROC(jointFit3, newdata = data1, Tstart = 1, Dt = 5, cores = 1L))$auc
auc3_2<-tvAUC(tvROC(jointFit3, newdata = data1, Tstart = 2, Dt = 5, cores = 1L))$auc
auc3_3<-tvAUC(tvROC(jointFit3, newdata = data1, Tstart = 3, Dt = 5, cores = 1L))$auc
auc3_4<-tvAUC(tvROC(jointFit3, newdata = data1, Tstart = 4, Dt = 5, cores = 1L))$auc
auc3_5<-tvAUC(tvROC(jointFit3, newdata = data1, Tstart = 5, Dt = 4.9, cores = 1L))$auc

a<-data.frame(year=c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5),
              auc =c(auc1_1,auc1_2,auc1_3,auc1_4,auc1_5,auc2_1,auc2_2,auc2_3,auc2_4,auc2_5,
                     auc3_1,auc3_2,auc3_3,auc3_4,auc3_5),
              model =c("model1:value","model1:value","model1:value","model1:value","model1:value",
                       "model2:value + slope","model2:value + slope","model2:value + slope","model2:value + slope","model2:value + slope",
                       "model3:value + area","model3:value + area","model3:value + area","model3:value + area","model3:value + area"))

library(ggrepel)
ggplot(data = a, aes(x = year, y = auc, group = model, color = model, shape=model))+ 
  geom_line(aes(color = model), size = 1.2)+
  labs(x ="Time of longitudial data (year)", y = "AUC of 5-year prediction") +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        axis.line = element_line(colour = "black"),
        # panel.grid.major = element_line(colour = "grey90"),
        # panel.grid.minor = element_line(colour = "grey90"),
        legend.background = element_rect(fill='transparent'),
        legend.position = c(0.4, 0.8),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        strip.text.y = element_text(
          size = 20, #color = "red", face = "bold.italic"
        ))+
  geom_label_repel(aes(label = round(auc,2)), nudge_x = 0, size = 4)


