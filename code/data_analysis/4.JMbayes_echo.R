library(JMbayes2)

data1 <- echo_match4 %>%
  left_join(Patient_bl, by = c("patid","soascaseid","centreid","tpxdate","graft_date"))
dim(data1)#632

data1<-as.data.frame(data1)%>%
  dplyr::arrange(soascaseid,time_echo_y)

categorical_vars<-c("sex","ethnicity",
                    "dialysistype","dontype","donsex","sumhlamismatch","smoking",
                    "sbp","dbp",
                      "crea","chol","ldlchol","hdlchol","hba1c",
                    #"Myocardial_Infarction","Cerebrovascular_Event","Cardiac_Arrest","Heart_Failure",
                    "Hypertension","Hyperlipidemia","Diabetes")

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
# data1$crea = relevel(data1$crea, ref = "0")
# data1$chol = relevel(data1$chol, ref = "0")
# data1$ldlchol = relevel(data1$ldlchol, ref = "0")
# data1$hdlchol = relevel(data1$hdlchol, ref = "0")
# data1$hba1c = relevel(data1$hba1c, ref = "0")

# data1$Myocardial_Infarction = relevel(data1$Myocardial_Infarction, ref = "Yes")
# data1$Cerebrovascular_Event = relevel(data1$Cerebrovascular_Event, ref = "Yes")
# data1$Cardiac_Arrest = relevel(data1$Cardiac_Arrest, ref = "Yes")
# data1$Heart_Failure = relevel(data1$Heart_Failure, ref = "Yes")
data1$Hypertension = relevel(data1$Hypertension, ref = "Yes")
data1$Hyperlipidemia = relevel(data1$Hyperlipidemia, ref = "Yes")
data1$Diabetes = relevel(data1$Diabetes, ref = "Yes")

#data1 <- data1[c(1:200),]
data1$time_echo_y<-ifelse(data1$time_echo_y<0,0,data1$time_echo_y)
data1.id <-data1%>% 
  group_by(soascaseid) %>%
  dplyr::slice(which.min(time_echo_y))



CoxFit <- coxph(Surv(time_MACE, status_MACE) ~ age_at_tpx+sex+dialysistype+dontype+
                  #crea+chol+ldlchol+hdlchol+hba1c+
                  #Myocardial_Infarction+Cerebrovascular_Event+Cardiac_Arrest+Heart_Failure+
                  Hypertension+Hyperlipidemia+Diabetes, data = data1.id, id = soascaseid)

fm1 <- lme(LAESD ~ time_echo_y, data = data1, random = ~ time_echo_y | soascaseid, na.action=na.exclude)
fm2 <- lme(RV.TAPSE ~ time_echo_y, data = data1, random = ~ time_echo_y | soascaseid, na.action=na.exclude)
fm3 <- lme(LV.EDD ~ time_echo_y, data = data1, random = ~ time_echo_y | soascaseid, na.action=na.exclude)
fm4 <- lme(LVMI ~ time_echo_y, data = data1, random = ~ time_echo_y | soascaseid, na.action=na.exclude)

fm5 <- lme(LV_RWT_cal ~ time_echo_y, data = data1, random = ~ time_echo_y | soascaseid, na.action=na.exclude)
fm6 <- lme(LV_EF ~ time_echo_y, data = data1, random = ~ time_echo_y | soascaseid, na.action=na.exclude)
fm7 <- lme(LV.posterior.wall.thickness ~ time_echo_y, data = data1, random = ~ time_echo_y | soascaseid, na.action=na.exclude)
fm8 <- lme(LV.septum.thickness ~ time_echo_y, data = data1, random = ~ time_echo_y | soascaseid, na.action=na.exclude)

# "BSA","LV_EDD","LVMI_cal","RWT","LV_EF","LV.posterior.wall.thickness","LV.septum.thickness","LAESD","RV.TAPSE"

jointFit <- jm(CoxFit, list(fm1,fm2,fm3,fm4,fm5,fm6,fm7,fm8), time_var = "time_echo_y",
               n_iter = 2500L, n_burnin = 500L, n_thin = 5L)
summary(jointFit)
jointFit_mace1<-jointFit


fForms <- list(
  "LAESD" = ~ value(LAESD) + slope(LAESD),
  "RV.TAPSE" = ~ value(RV.TAPSE) + slope(RV.TAPSE),
  "LV.EDD" = ~ value(LV.EDD) + slope(LV.EDD),
  "LVMI" = ~ value(LVMI) + slope(LVMI),
  "LV_RWT_cal" = ~ value(LV_RWT_cal) + slope(LV_RWT_cal),
  "LV_EF" = ~ value(LV_EF) + slope(LV_EF),
  "LV.posterior.wall.thickness" = ~ value(LV.posterior.wall.thickness) + slope(LV.posterior.wall.thickness),
  "LV.septum.thickness" = ~ value(LV.septum.thickness) + slope(LV.septum.thickness)
)

jointFit2 <- update(jointFit, functional_forms = fForms)
summary(jointFit2)

jointFit_mace2<-jointFit2


fForms <- list(
  "LAESD" = ~ value(LAESD) + area(LAESD),
  "RV.TAPSE" = ~ value(RV.TAPSE) + area(RV.TAPSE),
  "LV.EDD" = ~ value(LV.EDD) + area(LV.EDD),
  "LVMI" = ~ value(LVMI) + area(LVMI),
  "LV_RWT_cal" = ~ value(LV_RWT_cal) + area(LV_RWT_cal),
  "LV_EF" = ~ value(LV_EF) + area(LV_EF),
  "LV.posterior.wall.thickness" = ~ value(LV.posterior.wall.thickness) + area(LV.posterior.wall.thickness),
  "LV.septum.thickness" = ~ value(LV.septum.thickness) + area(LV.septum.thickness)
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

a<-data.frame(year=c(1,2,3,4,5,1,2,3,4,5#,1,2,3,4,5
                     ),
              auc =c(auc1_1,auc1_2,auc1_3,auc1_4,auc1_5,auc2_1,auc2_2,auc2_3,auc2_4,auc2_5#,
                     #auc3_1,auc3_2,auc3_3,auc3_4,auc3_5
                     ),
              model =c("model1:value","model1:value","model1:value","model1:value","model1:value",
                       "model2:value + slope","model2:value + slope","model2:value + slope","model2:value + slope","model2:value + slope"#,
                       #"model3:value + area","model3:value + area","model3:value + area","model3:value + area","model3:value + area"
                       ))

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
        axis.title = element_text(size = 15),
        strip.text.y = element_text(
          size = 15, #color = "red", face = "bold.italic"
        ))+
  geom_label_repel(aes(label = round(auc,2)), nudge_x = 0, size = 4)

####### 1.3 dynamic prediction and plot ########
# use the measurement during the first 5 time_creas to predict the MACE outcomes after 5 time_creas, until 12 time_creas
summary(data1)
data1$event <- data1$status_MACE

t0 <- 6
ND <- as.data.frame(data1[data1$soascaseid %in% c("RS-2003-0383","RS-2006-0308","	
RS-2006-0450"), ])
ND <- ND[ND$time_echo_y < t0, ]
ND$status_MACE <- 0
ND$time_MACE <- t0

# We can plot the data
# predLong1 <- predict(jointFit, newdata = ND, return_newdata = TRUE)
# 
# plot(predLong1)

predLong2 <- predict(jointFit, newdata = ND,
                     times = seq(t0, 12, length.out = 51),
                     return_newdata = TRUE)
# plot(predLong2, outcomes =1, subject = "RS-2003-0383")

predSurv <- predict(jointFit, newdata = ND, process = "event",
                    times = seq(t0, 12, length.out = 51),
                    return_newdata = TRUE)
# plot(predLong2, predSurv)


cols <- c('#F25C78', '#D973B5', '#F28322','#04ADBF')
plot(predLong2, predSurv, outcomes = c(4,6,8), subject = "RS-2006-0308",#1
     fun_long = list(identity, identity, identity),#
     fun_event = function (x) 1 - x,
     ylab_event = "Survival Probabilities",
     ylab_long = c("LV EDD", "RWT", "LVMI"),
     bg = '#132743', col_points = cols, col_line_long = cols,
     col_line_event = '#F7F7FF', col_axis = "white", 
     fill_CI_long = c("#F25C7880"),#, "#D973B580", "#F2832280"
     fill_CI_event = "#F7F7FF80",
     pos_ylab_long = c(1.9, 1.9, 0.08))

plot(predLong2, predSurv, outcomes = c(4,6,8), subject = "RS-2008-0007",#0
     fun_long = list(identity, identity, identity),#
     fun_event = function (x) 1 - x,
     ylab_event = "Survival Probabilities",
     ylab_long = c("LV EDD", "RWT", "LVMI"),
     bg = '#132743', col_points = cols, col_line_long = cols,
     col_line_event = '#F7F7FF', col_axis = "white", 
     fill_CI_long = c("#F25C7880"),#, "#D973B580", "#F2832280"
     fill_CI_event = "#F7F7FF80",
     pos_ylab_long = c(1.9, 1.9, 0.08))

ND <- as.data.frame(data1[data1$soascaseid %in% c("RS-2003-0383","RS-2004-0141","RS-2008-0007","RS-2007-0126","RS-2008-0444",#0
                                                  "RS-2006-0238","RS-2008-0151","RS-2008-0224","RS-2005-0529","RS-2010-0165"), ])

predLong1 <- predict(jointFit, newdata = data1,
                     times = seq(t0, 12, length.out = 51),
                     return_newdata = TRUE)
plot(predLong1, outcomes =1, subject = "RS-2006-0308")

predSurv1 <- predict(jointFit, newdata = data1, process = "event",
                     times = seq(t0, 12, length.out = 51),
                     return_newdata = TRUE)
plot(predLong1, predSur1)

library(animation)

id = "RS-2006-0308" # 5 yes 2.8
id = "RS-2006-0450"# 2 no
#id = "RS-2003-0383" # 1 no

saveGIF({
  df<-as.data.frame(data1[data1$soascaseid %in% c(id), ])
  len_id <- nrow(df)
  print(len_id)
  
  for(i in c(1:len_id)){
    t0 <-df[i,]$time_echo_y
    print(t0)
    ND <- df[which(df$time_echo_y < t0), ]
    ND$status_MACE <- 0
    ND$time_MACE <- t0
    print(i)
    predLong2 <- predict(jointFit, newdata = ND,
                         times = seq(t0, 12, length.out = 51),
                         return_newdata = TRUE)
    #plot(predLong2, outcomes =1, subject = id)
    predSurv <- predict(jointFit, newdata = ND, process = "event",
                        times = seq(t0, 12, length.out = 51),
                        return_newdata = TRUE)
    
    plot(predLong2, predSurv, outcomes = c(4,6,8), subject = id,
         fun_long = list(identity, identity, identity),#
         fun_event = function (x) 1 - x,
         ylab_event = "Survival Probabilities",
         ylab_long = c("LV_EDD", "RWT", "LVMI"),
         bg = '#132743', col_points = cols, col_line_long = cols,
         col_line_event = '#F7F7FF', col_axis = "white", 
         fill_CI_long = c("#F25C7880"),#, "#D973B580", "#F2832280"
         fill_CI_event = "#F7F7FF80",
         pos_ylab_long = c(1.9, 1.9, 0.08))
    # sfit <- survfitJM(jointFit.p1, newdata = df[1:i, ]) 
    # plot(sfit, estimator="mean", include.y = TRUE, conf.int=0.95, fill.area=TRUE, col.area="lightblue", main="Patient 1")
    
  }
},movie.name = "/Users/yuanshengzhang/Desktop/animation_no.gif",ani.width = 400, ani.height=400)

