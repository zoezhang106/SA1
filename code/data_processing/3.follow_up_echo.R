library(lubridate)
library(dplyr)
require(tidyverse)
library(survminer)
library(survival)
library(ggplot2)

##### 1. prepare data ########


echo <- read.csv (paste0(data_path,"echo_dataset/all_3_center_v3.csv"), header=TRUE, sep = ",", stringsAsFactors=FALSE) 
Patient_bl2<-readRDS(file = "Patient_bl2.rds")

dim(echo)#5092
echo <- unique(echo)
dim(echo)#5064

table(echo$center)

# remove unit row, not needed
echo <- echo %>%
  mutate(across(contains(c('date'), ignore.case = TRUE), lubridate::dmy)) %>%
  arrange(patid,echo_date)



# combine with based on baseline dataset to add tpxdate and graft date
echo_match <- Patient_bl2[,c("patid","soascaseid","centreid","tpxdate","graft_date")]  %>%
  left_join(echo, by = c("patid"))
dim(echo_match)#6825

# change to numeric variables
echo_match[,c(8:22)]<-echo_match[,c(8:22)]%>% mutate_all(as.numeric)

##### 2. calculate echo parameter ########
# calculate LV EDD, LVMI, RWT based on the follow formular
# LV Mass = 0.8 x (1.04 x (((LV.EDD + LV.septum.thickness +LV.posterior.wall.thickness)^3 - LV.EDD^3))) + 0.6
# LVMI (LV Mass Indexed to Body Surface Area) = LV Mass / BSA
# RWT (Relative Wall Thickness) = 2 x PWd / LVEDD
# For BSA, Mosteller’s formula is employed: BSA = (((Height in cm) x (Weight in kg))/ 3600)½

echo_match<-echo_match %>%
  dplyr::mutate(`LV_EDD_cal` = ifelse(!is.na(`LV.EDD`),`LV.EDD`, round(`LV.EDDI`*BSA,2)), .after="LV.EDD")%>%
  dplyr::mutate(`LVMI_cal2` = ifelse(!is.na(LVMI),`LVMI`,round((1.04 * ((LV_EDD_cal + LV.septum.thickness/10 +LV.posterior.wall.thickness/10)^3 - `LV_EDD_cal`^3) -14)/BSA, 2)), .after="LVMI")%>%
  dplyr::mutate(`LVMI_cal` = ifelse(!is.na(LVMI),`LVMI`,round((0.8*(1.04*(((LV_EDD_cal + LV.septum.thickness/10 +LV.posterior.wall.thickness/10)^3 - LV_EDD_cal^3))) + 0.6)/BSA, 2)), .after="LVMI") %>%
  dplyr::mutate(`LV_RWT_cal` = round(2*LV.posterior.wall.thickness/LV_EDD_cal*0.1,2), .after="LV.posterior.wall.thickness")


##### 3. filter data ########

# filter valid echo, echo that are earlier than 2 y before tpx are excluded
echo_match <- echo_match%>%
  add_column("time_echo_y" = as.numeric(echo_match$echo_date-echo_match$tpxdate)/365.25, .after="echo_date")%>%
  relocate(soascaseid,tpxdate,graft_date, .before="echo_date")%>%
  filter(echo_date <= graft_date)%>% #3968
  filter(round(time_echo_y) >= -2)

tmp <- echo_match%>% # remove the last column closest_to_transplant
  left_join(Patient_bl2, by = c("patid","soascaseid","centreid","tpxdate","graft_date"))


table(Patient_bl2$centreid)
table(echo$center)

round(table(echo_match$centreid)/table(Patient_bl2$centreid)[-2], 2)

# select the echo parameters used for later Cox regression analysis
echo_match2 <- echo_match%>% #[,c("patid","centreid","soascaseid","tpxdate","graft_date","echo_date","time_echo_y",
                             #"BSA","LV.ESD, "LV_EDD_cal", "LVMI_cal","RWT", "LV_EF","LV.posterior.wall.thickness", "LV.septum.thickness","LAESD","LV.LAV","LV.LAVI", "RV.TAPSE") ]%>%
  add_column("echo" = ifelse(echo_match$time_echo_y<0,"pre","post"), .after="echo_date")


# filter baseline echo
echo_match2 <- echo_match2%>%
  mutate(Non_NA_Count = rowSums(!is.na(echo_match2[,10:31])), .after = time_echo_y)
  

#manually choose good post
# 1.filer out report which only contain (very few) 6 parameter, likely to be acute phase.
# 2. calculate median

post_op <- echo_match2 %>%
  filter(echo == "post" )%>%
  filter(Non_NA_Count >6)
dim(post_op)#2400


post_op2 <- post_op %>%
  filter(echo == "post" & time_echo_y >= 1 & time_echo_y <= 3)
dim(post_op2)#575

d<-post_op2[!(duplicated(post_op2[,c("patid","echo_date")]) | duplicated(post_op2[,c("patid","echo_date")], fromLast = TRUE)),]
dim(d)#575
d = d[,-ncol(d)]

c<-post_op2[duplicated(post_op2[,c("patid","echo_date")]) | duplicated(post_op2[,c("patid","echo_date")], fromLast = TRUE),]
dim(c) #0
#write.csv(c,"c.csv",na ="")


# c <- read.csv(paste0(data_path,"echo_dataset/good_post.csv"), header=TRUE, sep = ",", stringsAsFactors=FALSE) 
# 
# c <- c%>%
#   mutate(across(contains(c('date'), ignore.case = TRUE), lubridate::dmy))%>%
#   filter(good_post == 1)
# c <- c[,-c(1,10,11)] 
# 
# post_op3 = rbind(c,d)%>%
#   group_by(patid)%>%
#   arrange(patid,echo_date)
# dim(post_op3)#600
post_op3 = d

# calculate median
post_op4 = post_op3[,c(1,9:32)]%>%
# post_op4 = post_op3%>%
  group_by(patid)%>%
  summarise_all(mean, na.rm=T)
dim(post_op4)#396

post_op4 <- post_op4%>%
  mutate(echo = "post", .after='patid')


##########################################
pre_op <- echo_match2 %>%
  filter(echo == "pre" & time_echo_y >= -2 & time_echo_y <= 0)%>%
  filter(Non_NA_Count >6)
dim(pre_op)#960

b<-pre_op[!(duplicated(pre_op$patid) | duplicated(pre_op$patid, fromLast = TRUE)),]
dim(b)#501

a<-pre_op[duplicated(pre_op$patid) | duplicated(pre_op$patid, fromLast = TRUE),]
dim(a) #459
write.csv(a,"a.csv",na ="")

#manually choose good pre.
# 1.filer out report which only contain very few parameter, likely to be acute phase.
# 2. choose the latest one

a <- read.csv(paste0(data_path,"echo_dataset/good_pre.csv"), header=TRUE, sep = ",", stringsAsFactors=FALSE) 

a <- a%>%
  mutate(across(contains(c('date'), ignore.case = TRUE), lubridate::dmy))%>%
  filter(good_pre == 1)
a <- a[,-c(1,11)]

# use the most closest to tpx echo if there are more than one reports available # or based on which report has more echo parameters available
# pre_op2 <- pre_op %>%
#   group_by(soascaseid) %>%
#   mutate(closest_to_transplant = min(abs(time_echo_y))) %>%
#   filter(abs(time_echo_y) == closest_to_transplant)
# pre_op2 <- pre_op2[,-ncol(pre_op2)]


pre_op2 = rbind(a,b)%>%
  group_by(patid)%>%
  arrange(patid,echo_date)
dim(pre_op2)#676

Patient <- pre_op2%>% # remove the last column closest_to_transplant
  left_join(Patient_bl2, by = c("patid","soascaseid","centreid","tpxdate","graft_date"))
dim(Patient)#676 ,126
length(unique((Patient$patid))) # 676

#Patient[duplicated(Patient$patid) | duplicated(Patient$patid, fromLast = TRUE),]

write.csv(Patient,"Patient.csv", row.names = FALSE, na="")

saveRDS(Patient, file = "Patient.rds")

dim(Patient)



# % pre, for each center
round(table(Patient$centreid)/table(Patient_bl2$centreid)[-2], 2)
#   BE  HUG   SG  USB  USZ 
# 0.10 0.53 0.61 0.15 0.26

# % post, for each center

# valid pre + valid post is 2y +- 1y
dim(pre_op2)
dim(post_op4)
pre_op3 = pre_op2[,c(1,9:32)]

pre_op3 <- pre_op3%>%
  mutate(echo = "pre", .after='patid')


valid_pre = pre_op3[pre_op3$patid %in% post_op4$patid,]
dim(valid_pre)
valid_post = post_op4[post_op4$patid %in% pre_op3$patid,]
dim(valid_post)

echo_match3 = rbind(valid_pre,valid_post)%>%
  group_by(patid)%>%
  arrange(patid)
dim(echo_match3) #492

ggplot(echo_match3, aes(echo,LV_EF, group = patid, color = patid)) +
  geom_point() +
  geom_line()
 

Patient2 <- valid_pre%>% # remove the last column closest_to_transplant
  left_join(Patient_bl2, by = c("patid"))

# % pre, for each center
round(table(Patient2$centreid)/table(Patient_bl2$centreid)[-2], 2)
#   BE  HUG   SG  USB  USZ 
# 0.05 0.17 0.23 0.03 0.12 


# ggplot(echo_match3, aes(x=centreid, y=..count..), colors = centreid)+
#   geom_bar(stat='count')

write.csv(echo_match3,"echo_followup_final.csv", row.names = FALSE, na="")
saveRDS(echo_match3, file = "echo_followup_final.rds")


####### valid pre + all post ####### 
echo_match4 = rbind(pre_op2,post_op[post_op$patid %in% pre_op2$patid,])%>%
  group_by(patid)%>%
  arrange(patid,echo_date)
dim(echo_match4) #2408

write.csv(echo_match4,"echo_followup_final.csv", row.names = FALSE, na="")
saveRDS(echo_match4, file = "echo_followup_final.rds")


####### valid pre + valid post ####### 
valid_pre2 = pre_op3[pre_op3$patid %in% post_op4$patid,]
dim(valid_pre2)#246
valid_post2 = post_op4[post_op4$patid %in% pre_op3$patid,]
dim(valid_post2)#246

echo_match5 = rbind(valid_pre2,valid_post2)%>%
  group_by(patid)%>%
  arrange(patid)
dim(echo_match5) #492

write.csv(echo_match5,"echo_pair_final.csv", row.names = FALSE, na="")
saveRDS(echo_match5, file = "echo_pair_final.rds")


Patient_paired <- echo_match5%>% # remove the last column closest_to_transplant
  left_join(Patient_bl2, by = c("patid"))
dim(Patient_paired)#492


####### statistics ####### 
ggplot(echo_match4, aes(echo,LV_EF, group = patid, color = patid)) +
  geom_point() +
  geom_line()


Patient3 <- valid_pre2%>% # remove the last column closest_to_transplant
  left_join(Patient_bl2, by = c("patid"))

# post
round(table(Patient3$centreid)/table(Patient_bl2$centreid)[-2], 2)
round(table(echo_match4$centreid)/table(Patient_bl2$centreid)[-2], 2)
#valid post
round(table(post_op2$centreid)/table(Patient_bl2$centreid)[-2], 2)
table(post_op2$centreid)

# all pre
table(echo_match2$centreid)
round(table(echo_match2$centreid)/table(Patient_bl2$centreid)[-2], 2)

echo_match2%>%
  group_by(centreid)%>%
  summarise(count=n_distinct(patid))

# all post-op echo with valid pre-op echo
valid_post <- valid_post%>% # remove the last column closest_to_transplant
  left_join(echo_match2[,c("patid","centreid")], by = c("patid"))
valid_post2 <- valid_post2%>% # remove the last column closest_to_transplant
  left_join(echo_match2[,c("patid","centreid")], by = c("patid"))

valid_post%>%
  group_by(centreid)%>%
  summarise(count=n())
valid_post2%>%
  group_by(centreid)%>%
  summarise(count=n_distinct(patid))

# Valid post-op echo with valid pre-op echo

valid_post2 = post_op[post_op$patid %in% pre_op3$patid,]
valid_post2%>%
  group_by(centreid)%>%
  summarise(count=n())
valid_post2%>%
  group_by(centreid)%>%
  summarise(count=n_distinct(patid))

# Valid post-op echo with valid pre-op echo
valid_post2 = post_op2[post_op2$patid %in% pre_op3$patid,]
valid_post2%>%
  group_by(centreid)%>%
  summarise(count=n())
valid_post2%>%
  group_by(centreid)%>%
  summarise(count=n_distinct(patid))
