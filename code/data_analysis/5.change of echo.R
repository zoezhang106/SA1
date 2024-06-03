echo_match5
names(echo_match5)
paired_data<-echo_match5

##### t-test difference ##########

# Names of echo parameters
param_names <- names(echo_match5)[6:ncol(echo_match5)]

library(dplyr)

# Function to perform paired t-test on a given parameter
perform_paired_t_test <- function(df, param) {
  
  param_pre <- paste0(param, "_Pre")
  param_post <- paste0(param, "_Post")
  df2<- data.frame(
    patid = df$patid,
    param_pre = df[df$echo == "pre",param],
    param_post = df[df$echo == "post",param]
  )
  names(df2)<-c("patid",param_pre,param_post)
  
  # Filter out rows where either pre or post is NA
  clean_data <- df2 %>% filter(!is.na(df2[[param_pre]]) & !is.na(df2[[param_post]]))
  
  # Perform the paired t-test
  
  test_result <- t.test(clean_data[[param_pre]], clean_data[[param_post]], paired = TRUE)
  
  # Return a data frame with the p-value and parameter name
  data.frame(
    Parameter = param,
    n = nrow(clean_data)/2,
    mean_pre = mean(clean_data[[param_pre]]),
    sd_pre = sd(clean_data[[param_pre]]),
    mean_post = mean(clean_data[[param_post]]),
    sd_post = sd(clean_data[[param_post]]),
    P_Value = test_result$p.value
  )
}

# Example dataset (assuming data and merged data frames are prepared as above)
param_names <- names(paired_data)[6:ncol(paired_data)]  # Assuming parameters start from the 6th column

# Apply the paired t-test function to each parameter
test_results <- lapply(param_names, function(x) perform_paired_t_test(paired_data, x))

# Convert list of data frames to a single data frame
test_results_df <- do.call(rbind, test_results)

# Print the results
print(test_results_df)

######## box plot LVMI ###########
echo_all <-echo_match5%>% # remove the last column closest_to_transplant
  left_join(Patient_bl2[,c("patid","sex")], by = c("patid"))


echo_all$color = "#F8766D"#"lightgreen"
list1<-echo_all[which(echo_all$sex == "Female"& echo_all$echo=="pre"&echo_all$LVMI>95),]$patid
list2<-echo_all[which(echo_all$sex == "Male"& echo_all$echo=="pre"&echo_all$LVMI>115),]$patid
list3<-echo_all[which(is.na(echo_all$LVMI)),]$patid

echo_all <- echo_all[echo_all$patid %in% c(list1,list2)& !(echo_all$patid %in% list3),]

for (i in echo_all$patid){
  #print(i)
  a = median(as.numeric(echo_all[which(echo_all$patid ==i & echo_all$echo=="post"),]$LVMI), na.rm = TRUE)
  b = median(as.numeric(echo_all[echo_all$patid ==i & echo_all$echo=="pre",]$LVMI), na.rm = TRUE)
  #print(a)
  if (!identical(a, numeric(0))  &!identical(b, numeric(0)) ){
    if(!is.na(a)&!is.na(b)){
      if(a-b>0){echo_all[which(echo_all$patid ==i),]$color = "#00BFC4"}}
  }
  #print("2")
}

p <- ggboxplot(echo_all, x = "echo", y = "LVMI",
               color = "echo", palette = "jco",
               add = "jitter",short.panel.labs = FALSE)

# Use only p.format as label. Remove method name.
p<-p + #stat_compare_means(aes(label = paste0("p = ", after_stat(p.format)),
                              #label = "p.format", 
                              #paired = TRUE,method = "t.test", size = 6))+ 
  geom_line(aes(group=patid), colour=echo_all$color, linetype="11")+
  theme(text = element_text(size = 20)) +
  theme(
    #panel.background = element_rect(fill='transparent'), #transparent panel bg
    #plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    #panel.border = element_blank(),
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank()#, #remove minor gridlines
    #legend.background = element_rect(fill='transparent'), #transparent legend bg
    #legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )+ ylab("LV mass index (g/m²)")#ylab("RV end-diast. diam. index (cm/m²)")#ylab("RV Tricuspid annular plane \n systolic excursion (cm)") #ylab("LV ejection fraction (%)")#ylab("LV mitral annular plane \n systolic excursion (cm)") #ylab("LV mass index (g/m²)") #ylab("LV relative wall thickness")
#ylab("LV end-diast. diam. index (cm/m²)")
p

dim(echo_all)



round(mean(echo_all[echo_all$echo =="pre",]$LVMI),2)
round(sd(echo_all[echo_all$echo =="pre",]$LVMI),2)
round(mean(echo_all[echo_all$echo =="post",]$LVMI),2)
round(sd(echo_all[echo_all$echo =="post",]$LVMI),2)


test_result <- t.test(echo_all[echo_all$echo =="pre",]$LVMI, 
                      echo_all[echo_all$echo =="post",]$LVMI, paired = TRUE)
test_result

######### box plot LV_RWT_cal ###########
echo_all <-echo_match5%>% # remove the last column closest_to_transplant
  left_join(Patient_bl2[,c("patid","sex")], by = c("patid"))


echo_all$color = "#F8766D"#"lightgreen"
list1<-echo_all[which(echo_all$echo=="pre"&echo_all$LV_RWT_cal>0.42),]$patid
list3<-echo_all[which(is.na(echo_all$LV_RWT_cal)),]$patid
echo_all <- echo_all[echo_all$patid %in% c(list1)& !(echo_all$patid %in% list3),]


for (i in echo_all$patid){
  #print(i)
  a = median(as.numeric(echo_all[which(echo_all$patid ==i & echo_all$echo=="post"),]$LV_RWT_cal), na.rm = TRUE)
  b = median(as.numeric(echo_all[echo_all$patid ==i & echo_all$echo=="pre",]$LV_RWT_cal), na.rm = TRUE)
  #print(a)
  if (!identical(a, numeric(0))  &!identical(b, numeric(0)) ){
    if(!is.na(a)&!is.na(b)){
      if(a-b>0){echo_all[which(echo_all$patid ==i),]$color = "#00BFC4"}}
  }
  #print("2")
}

p <- ggboxplot(echo_all, x = "echo", y = "LV_RWT_cal",
               color = "echo", palette = "jco",
               add = "jitter",short.panel.labs = FALSE)

# Use only p.format as label. Remove method name.
 p<-p + 
#stat_compare_means(aes(label = paste0("p = ", after_stat(p.format)),
#                               #label = "p.format", 
#                               paired = TRUE,method = "t.test", size = 6),
#                           label.x.npc = "centre",
#                           label.y.npc = "top")+ 
  geom_line(aes(group=patid), colour=echo_all$color, linetype="11")+
  theme(text = element_text(size = 20)) +
  theme(
    #panel.background = element_rect(fill='transparent'), #transparent panel bg
    #plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    #panel.border = element_blank(),
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank()#, #remove minor gridlines
    #legend.background = element_rect(fill='transparent'), #transparent legend bg
    #legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )+ ylab("LV relative wall thickness")#ylab("LV mass index (g/m²)")#ylab("RV end-diast. diam. index (cm/m²)")#ylab("RV Tricuspid annular plane \n systolic excursion (cm)") #ylab("LV ejection fraction (%)")#ylab("LV mitral annular plane \n systolic excursion (cm)") #ylab("LV mass index (g/m²)") #
#ylab("LV end-diast. diam. index (cm/m²)")
p

dim(echo_all)

round(mean(echo_all[echo_all$echo =="pre",]$LV_RWT_cal),2)
round(sd(echo_all[echo_all$echo =="pre",]$LV_RWT_cal),2)
round(mean(echo_all[echo_all$echo =="post",]$LV_RWT_cal),2)
round(sd(echo_all[echo_all$echo =="post",]$LV_RWT_cal),2)


test_result <- t.test(echo_all[echo_all$echo =="pre",]$LV_RWT_cal, 
                      echo_all[echo_all$echo =="post",]$LV_RWT_cal, paired = TRUE)

test_result


######### box plot LV_EF ###########
echo_all <-echo_match5%>% # remove the last column closest_to_transplant
  left_join(Patient_bl2[,c("patid","sex")], by = c("patid"))


echo_all$color = "#F8766D"#"lightgreen"
list1<-echo_all[which(echo_all$echo=="pre"&echo_all$LV_EF<55),]$patid
list3<-echo_all[which(is.na(echo_all$LV_EF)),]$patid
echo_all <- echo_all[echo_all$patid %in% c(list1)& !(echo_all$patid %in% list3),]


for (i in echo_all$patid){
  #print(i)
  a = median(as.numeric(echo_all[which(echo_all$patid ==i & echo_all$echo=="post"),]$LV_EF), na.rm = TRUE)
  b = median(as.numeric(echo_all[echo_all$patid ==i & echo_all$echo=="pre",]$LV_EF), na.rm = TRUE)
  #print(a)
  if (!identical(a, numeric(0))  &!identical(b, numeric(0)) ){
    if(!is.na(a)&!is.na(b)){
      if(a-b>0){echo_all[which(echo_all$patid ==i),]$color = "#00BFC4"}}
  }
  #print("2")
}

p <- ggboxplot(echo_all, x = "echo", y = "LV_EF",
               color = "echo", palette = "jco",
               add = "jitter",short.panel.labs = FALSE)

# Use only p.format as label. Remove method name.
p<-p + 
  #stat_compare_means(aes(label = paste0("p = ", after_stat(p.format)),
  #                               #label = "p.format", 
  #                               paired = TRUE,method = "t.test", size = 6),
  #                           label.x.npc = "centre",
  #                           label.y.npc = "top")+ 
  geom_line(aes(group=patid), colour=echo_all$color, linetype="11")+
  theme(text = element_text(size = 20)) +
  theme(
    #panel.background = element_rect(fill='transparent'), #transparent panel bg
    #plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    #panel.border = element_blank(),
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank()#, #remove minor gridlines
    #legend.background = element_rect(fill='transparent'), #transparent legend bg
    #legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )+ ylab("LV ejection fraction (%)")#ylab("LV relative wall thickness")#ylab("LV mass index (g/m²)")#ylab("RV end-diast. diam. index (cm/m²)")#ylab("RV Tricuspid annular plane \n systolic excursion (cm)") #ylab("LV mitral annular plane \n systolic excursion (cm)") #ylab("LV mass index (g/m²)") #
#ylab("LV end-diast. diam. index (cm/m²)")
p

dim(echo_all)

round(mean(echo_all[echo_all$echo =="pre",]$LV_EF),2)
round(sd(echo_all[echo_all$echo =="pre",]$LV_EF),2)
round(mean(echo_all[echo_all$echo =="post",]$LV_EF),2)
round(sd(echo_all[echo_all$echo =="post",]$LV_EF),2)


test_result <- t.test(echo_all[echo_all$echo =="pre",]$LV_EF, 
                      echo_all[echo_all$echo =="post",]$LV_EF, paired = TRUE)

test_result



######### box plot RV.TAPSE ###########
echo_all <-echo_match5%>% # remove the last column closest_to_transplant
  left_join(Patient_bl2[,c("patid","sex")], by = c("patid"))


echo_all$color = "#F8766D"#"lightgreen"
list1<-echo_all[which(echo_all$echo=="pre"&echo_all$RV.TAPSE<17),]$patid
list3<-echo_all[which(is.na(echo_all$RV.TAPSE)),]$patid
echo_all <- echo_all[echo_all$patid %in% c(list1)& !(echo_all$patid %in% list3),]


for (i in echo_all$patid){
  #print(i)
  a = median(as.numeric(echo_all[which(echo_all$patid ==i & echo_all$echo=="post"),]$RV.TAPSE), na.rm = TRUE)
  b = median(as.numeric(echo_all[echo_all$patid ==i & echo_all$echo=="pre",]$RV.TAPSE), na.rm = TRUE)
  #print(a)
  if (!identical(a, numeric(0))  &!identical(b, numeric(0)) ){
    if(!is.na(a)&!is.na(b)){
      if(a-b>0){echo_all[which(echo_all$patid ==i),]$color = "#00BFC4"}}
  }
  #print("2")
}

p <- ggboxplot(echo_all, x = "echo", y = "RV.TAPSE",
               color = "echo", palette = "jco",
               add = "jitter",short.panel.labs = FALSE)

# Use only p.format as label. Remove method name.
p<-p + 
  #stat_compare_means(aes(label = paste0("p = ", after_stat(p.format)),
  #                               #label = "p.format", 
  #                               paired = TRUE,method = "t.test", size = 6),
  #                           label.x.npc = "centre",
  #                           label.y.npc = "top")+ 
  geom_line(aes(group=patid), colour=echo_all$color, linetype="11")+
  theme(text = element_text(size = 20)) +
  theme(
    #panel.background = element_rect(fill='transparent'), #transparent panel bg
    #plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    #panel.border = element_blank(),
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank()#, #remove minor gridlines
    #legend.background = element_rect(fill='transparent'), #transparent legend bg
    #legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )+ ylab("RV Tricuspid annular plane \n systolic excursion (mm)") #ylab("LV ejection fraction (%)")#ylab("LV relative wall thickness")#ylab("LV mass index (g/m²)")#ylab("RV end-diast. diam. index (cm/m²)")#ylab("RV Tricuspid annular plane \n systolic excursion (cm)") #ylab("LV mitral annular plane \n systolic excursion (cm)") #ylab("LV mass index (g/m²)") #
#ylab("LV end-diast. diam. index (cm/m²)")
p

dim(echo_all)

round(mean(echo_all[echo_all$echo =="pre",]$RV.TAPSE),2)
round(sd(echo_all[echo_all$echo =="pre",]$RV.TAPSE),2)
round(mean(echo_all[echo_all$echo =="post",]$RV.TAPSE),2)
round(sd(echo_all[echo_all$echo =="post",]$RV.TAPSE),2)


test_result <- t.test(echo_all[echo_all$echo =="pre",]$RV.TAPSE, 
                      echo_all[echo_all$echo =="post",]$RV.TAPSE, paired = TRUE)

test_result


#######  4 four patterns of cardiac remodeling #########
a3_pair <-echo_match5%>% # remove the last column closest_to_transplant
  left_join(Patient_bl2[,c("patid","sex")], by = c("patid"))

#echo6<-echo_1[!(echo_1$soascaseid %in% echo_1[is.na(echo_1$LVMI) | is.na(echo_1$LV_RWT_cal),]$soascaseid),]
echo6<-a3_pair[!(a3_pair$patid %in% a3_pair[is.na(a3_pair$LVMI) | is.na(a3_pair$LV_RWT_cal),]$patid),]
dim(echo6)
echo6$LVH<-"normal"

echo6_pre <-echo6[echo6$echo == "pre",]
echo6_post <-echo6[echo6$echo == "post",]%>% 
  group_by(patid) %>%
  dplyr::slice(which.max(time_echo_y))
echo6<-rbind(echo6_pre,echo6_post)         

echo6[echo6$sex =="Male" & echo6$LVMI<=115 & echo6$LV_RWT_cal>0.42,]$LVH<-"concentric remodeling"
echo6[echo6$sex =="Female" & echo6$LVMI<=95 & echo6$LV_RWT_cal>0.42,]$LVH<-"concentric remodeling"

echo6[echo6$sex =="Male" & echo6$LVMI>115 & echo6$LV_RWT_cal>0.42,]$LVH<-"concentric hypertrophy"
echo6[echo6$sex =="Female" & echo6$LVMI>95 & echo6$LV_RWT_cal>0.42,]$LVH<-"concentric hypertrophy"

echo6[echo6$sex =="Male" & echo6$LVMI>115 & echo6$LV_RWT_cal<=0.42,]$LVH<-"eccentric hypertrophy"
echo6[echo6$sex =="Female" & echo6$LVMI>95 & echo6$LV_RWT_cal<=0.42,]$LVH<-"eccentric hypertrophy"

table(echo6[echo6$echo=="pre",]$LVH)/nrow(echo6[echo6$echo=="pre",])
table(echo6[echo6$echo=="post",]$LVH)/nrow(echo6[echo6$echo=="post",])

library(caret)
dim(echo6)

pre_value <- factor(echo6[echo6$echo=="pre",]$LVH)
post_value <- factor(echo6[echo6$echo=="post",]$LVH)
example <- confusionMatrix(data=post_value, reference = pre_value, dnn = c("Post", "Pre"))

as.table(example)

df <- data.frame(patid=echo6$patid,
                 echo=echo6$echo,
                 LVH=echo6$LVH)


df<-reshape(df, idvar = "patid", timevar = "echo", direction = "wide")

transition_counts <- df %>%
  group_by(LVH.pre, LVH.post) %>%
  summarise(freq = n(), .groups = 'drop')

print(transition_counts)

# install.packages("ggalluvial")
library(ggalluvial)


ggplot(data = transition_counts,
       aes(axis1 = LVH.pre, axis2 = LVH.post, y = freq, label = freq)) +
  geom_alluvium(aes(fill = LVH.pre)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  geom_text(stat = "flow", nudge_x = 0.2) +
  theme_void()+
  theme(legend.position = "bottom") 
  
  
ggplot(data = transition_counts,
       aes(axis1 = LVH.pre, axis2 = LVH.post, y = freq, label = freq)) +
  geom_alluvium(aes(fill = LVH.pre)) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Before", "After"),
                   expand = c(0.15, 0.05)) +
  theme_void()



#### 6. trajectory by CRS_type for #>5 ######

aa<-echo_match4%>% # remove the last column closest_to_transplant
  left_join(Patient_bl2[,c("patid","status_MACE","time_MACE","MACE_history")], by = c("patid"))

aa<-aa %>%
  add_count(patid, name = "count")
table(aa$count)
aa$time_echo_y<-ifelse(aa$time_echo_y<=0,0,aa$time_echo_y)

#### 6.1 LVMI --------
#aa2<-aa[aa$count>5& (aa$time_echo_y==0 | (aa$time_echo_y>0.5 & aa$time_echo_y<5)),]#
# aa2<-aa[aa$count>5& (aa$time_echo_y>=0),]#
#aa2<-aa[aa$count>5& (aa$time_echo_y>=0) & !is.na(aa$LVMI),]#
aa2<-aa[aa$count>5& (aa$time_echo_y<=0 | (aa$time_echo_y>0.5)),]#
#aa2<-aa[aa$count>5& aa$time_echo_y>=0 & (aa$time_echo_y< aa$time_MACE),]#
dim(aa2)
aa2$LVMI_pre<-NA

# for(i in unique(aa2$patid)){
#   aa2[(aa2$patid ==i),]$LVMI_pre = aa2[(aa2$patid ==i & aa2$echo == "pre"),]$LVMI
# }
# 
# aa2$change<-aa2$LVMI-aa2$LVMI_pre

aa2$status_MACE_5y <- ifelse(aa2$status_MACE =="1"& aa2$time_MACE<5,1,0)
aa2$time_MACE_5y <- ifelse(aa2$status_MACE =="1"& aa2$time_MACE<5,aa2$time_MACE,5)
aa2<-aa2[aa2$time_echo_y< aa2$time_MACE_5y,]#


# ggplot(aa2,                    # Specify colors manually
#        aes(x = time_echo_y, #round(time_echo_y),
#            y = LVMI,
#            col = soascaseid
#        )) +
#   #xlim(0,5)+
#   geom_line(data=aa2[!is.na(aa2$LVMI),], show.legend = FALSE) +#
#   geom_point(show.legend = FALSE) +theme_classic()+ facet_wrap(vars(status_MACE))

#library(ggpmisc)
ggplot(aa2,                    # Specify colors manually
       aes(x = time_echo_y, #round(time_echo_y),
           y = change,
           color = MACE_history#soascaseid
       )) +
  geom_point(alpha = 0.5, show.legend = FALSE) +
  geom_path(aes(group = soascaseid), alpha = 0.5) +
  geom_smooth(
              span = .2 #,color="black"#
              #method = lm, formula = y~x,
              )+
  #geom_line(data=aa2[!is.na(aa2$LVMI),], show.legend = FALSE) +#
  theme_classic()+ 
  facet_wrap(vars(status_MACE_5y), labeller = "label_both")+
  stat_poly_eq(
      aes(label = paste(stat(adj.rr.label),
                        stat(p.value.label), 
                        sep = "*\", \"*")),
      formula = y~x, 
      rr.digits = 2, 
      p.digits = 1, 
      parse = TRUE,size=3.5)
#library(ggpmisc)
ggplot(aa2,                    # Specify colors manually
       aes(x = time_echo_y, #round(time_echo_y),
           y = LVMI,
           color = MACE_history#soascaseid
       )) +
  geom_point(alpha = 0.5, show.legend = FALSE) +
  geom_path(aes(group = soascaseid), alpha = 0.5) +
  geom_smooth(
    #span = .2#, #,color="black"#
    # method = 'loess'
    #method = lm, formula = y~x
  )+
  #geom_line(data=aa2[!is.na(aa2$LVMI),], show.legend = FALSE) +#
  theme_classic()+ 
  facet_wrap(vars(status_MACE_5y), labeller = "label_both")+
  stat_poly_eq(
    aes(label = paste(stat(adj.rr.label),
                      stat(p.value.label), 
                      sep = "*\", \"*")),
    formula = y~x, 
    rr.digits = 2, 
    p.digits = 1, 
    parse = TRUE,size=3.5)+  
  scale_fill_manual(name = "MACE_history", values = c("#00BFC4","#F8766D"))+  
  scale_color_manual(name = "MACE_history", values = c("#00BFC4","#F8766D"))

# When span = 0.2, it means that 20% of the data points are used to fit each local regression. A smaller span will result in a more flexible (wiggly) fit, while a larger span will result in a smoother (less wiggly) fit.

#### 6.2 LV_EF --------
#aa2<-aa[aa$count>5& (aa$time_echo_y==0 | (aa$time_echo_y>0.5 & aa$time_echo_y<5)),]#
aa2<-aa[aa$count>5& (aa$time_echo_y<=0 | (aa$time_echo_y>0.5)),]#

#aa2<-aa[aa$count>5& (aa$time_echo_y==0 | (aa$time_echo_y>0.5 & aa$time_echo_y<5)),]#
# aa2<-aa[aa$count>5& (aa$time_echo_y>=0),]#
#aa2<-aa[aa$count>5& (aa$time_echo_y>=0) & !is.na(aa$LVMI),]#
#aa2<-aa[aa$count>5& (aa$time_echo_y<=0 | (aa$time_echo_y>0.5)),]#
#aa2<-aa[aa$count>5& aa$time_echo_y>=0 & (aa$time_echo_y< aa$time_MACE),]#
dim(aa2)



# ggplot(aa2,                    # Specify colors manually
#        aes(x = time_echo_y, #round(time_echo_y),
#            y = LV_EF,
#            col = soascaseid
#        )) +
#   #xlim(0,5)+
#   geom_line(data=aa2[!is.na(aa2$LV_EF),], show.legend = FALSE) +#
#   geom_point(show.legend = FALSE) +theme_classic()+ facet_wrap(vars(status_MACE))

aa2$LV_EF_pre<-NA

for(i in unique(aa2$patid)){
  aa2[(aa2$patid ==i),]$LV_EF_pre = aa2[(aa2$patid ==i & aa2$echo == "pre"),]$LV_EF
}

aa2$change<-aa2$LV_EF-aa2$LV_EF_pre

aa2$status_MACE_5y <- ifelse(aa2$status_MACE =="1"& aa2$time_MACE<5,1,0)
aa2$time_MACE_5y <- ifelse(aa2$status_MACE =="1"& aa2$time_MACE<5,aa2$time_MACE,5)
aa2<-aa2[aa2$time_echo_y< aa2$time_MACE_5y,]#


ggplot(aa2,                    # Specify colors manually
       aes(x = time_echo_y, #round(time_echo_y),
           y = change,
           color = MACE_history#soascaseid
       )) +
  geom_point(alpha = 0.5, show.legend = FALSE) +
  geom_path(aes(group = soascaseid), alpha = 0.5) +
  geom_smooth(
    span = .2 #,color="black"#
    #method = lm, formula = y~x,
  )+
  #geom_line(data=aa2[!is.na(aa2$LVMI),], show.legend = FALSE) +#
  theme_classic()+ 
  facet_wrap(vars(status_MACE_5y), labeller = "label_both")+
  stat_poly_eq(
    aes(label = paste(stat(adj.rr.label),
                      stat(p.value.label), 
                      sep = "*\", \"*")),
    formula = y~x, 
    rr.digits = 2, 
    p.digits = 1, 
    parse = TRUE,size=3.5)

ggplot(aa2,                    # Specify colors manually
       aes(x = time_echo_y, #round(time_echo_y),
           y = LV_EF,
           fill = MACE_history,
           color = MACE_history#soascaseid
       )) +
  geom_point(alpha = 0.5, show.legend = FALSE) +
  geom_path(aes(group = soascaseid), alpha = 0.5) +
  geom_smooth(
    #span = .2 #,color="black"#
    #method = lm, formula = y~x,
  )+
  #geom_line(data=aa2[!is.na(aa2$LVMI),], show.legend = FALSE) +#
  theme_classic()+ 
  facet_wrap(vars(status_MACE_5y), labeller = "label_both")+
  stat_poly_eq(
    aes(label = paste(stat(adj.rr.label),
                      stat(p.value.label), 
                      sep = "*\", \"*")),
    formula = y~x, 
    rr.digits = 2, 
    p.digits = 1, 
    parse = TRUE,size=3.5)+  
  scale_fill_manual(name = "MACE_history", values = c("#00BFC4","#F8766D"))+  
  scale_color_manual(name = "MACE_history", values = c("#00BFC4","#F8766D"))

###### pair-wise t-test between groups #######

Patient2$Center<-as.factor(ifelse(Patient2$center=="Bern_new2" | Patient2$center=="Bern_old",
                                   "Bern",
                                   ifelse(Patient2$center=="KSSG_new" | Patient2$center=="KSSG_old",
                                          "KSSG",Patient2$center)))

stat.test <- Patient2 %>%
  t_test(LAESD ~ Center) %>%
  add_significance()
stat.test

# Box plots with p-values
bxp <- ggboxplot(Patient2, x = "Center", y = "LAESD", fill = "#00AFBB")
stat.test <- stat.test %>%
  filter(p.adj.signif != "ns") %>%
  add_xy_position(x = "Center")
bxp + 
  #stat_pvalue_manual(stat.test, label = "p") +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  ylab("LV end-syst. diam. (cm)")
# scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
# 


stat.test <- Patient4 %>%
  t_test(LV.ESD ~ Center) %>%
  add_significance()
stat.test

# Box plots with p-values
bxp <- ggboxplot(Patient4, x = "Center", y = "LV.ESD", fill = "#00AFBB")
stat.test <- stat.test %>%
  filter(p.adj.signif != "ns") %>%
  add_xy_position(x = "Center")
bxp + 
  #stat_pvalue_manual(stat.test, label = "p") +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  ylab("LV end-syst. diam. (cm)")
# scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
# 

stat.test <- Patient4 %>%
  t_test(LV.EDD ~ Center) %>%
  add_significance()
stat.test

# Box plots with p-values
bxp <- ggboxplot(Patient4, x = "Center", y = "LV.EDD", fill = "#00AFBB")
stat.test <- stat.test %>%
  filter(p.adj.signif != "ns") %>%
  add_xy_position(x = "Center")
bxp + 
  #stat_pvalue_manual(stat.test, label = "p") +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  ylab("LV end-diast. diam. (cm)")
# scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
# 

stat.test <- Patient4 %>%
  t_test(LV.EDDI ~ Center) %>%
  add_significance()
stat.test

# Box plots with p-values
bxp <- ggboxplot(Patient4, x = "Center", y = "LV.EDDI", fill = "#00AFBB")
stat.test <- stat.test %>%
  filter(p.adj.signif != "ns") %>%
  add_xy_position(x = "Center")
bxp + 
  #stat_pvalue_manual(stat.test, label = "p") +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  ylab("LV end-diast. diam. index (cm/m²)")
# scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
#

# Statistical test
Patient4$Center<-Patient4$center2
stat.test <- Patient4 %>%
  t_test(LVMI ~ Center) %>%
  add_significance()
stat.test

# Box plots with p-values
bxp <- ggboxplot(Patient4, x = "Center", y = "LVMI", fill = "#00AFBB")
stat.test <- stat.test %>% 
  filter(p.adj.signif != "ns") %>%
  add_xy_position(x = "Center")
bxp + 
  #stat_pvalue_manual(stat.test, label = "p") +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+ 
  ylab("LV mass index (g/m²)")


stat.test <- Patient4 %>%
  t_test(LV.posterior.wall.thickness ~ Center) %>%
  add_significance()
stat.test

# Box plots with p-values
bxp <- ggboxplot(Patient4, x = "Center", y = "LV.posterior.wall.thickness", fill = "#00AFBB")
stat.test <- stat.test %>%
  filter(p.adj.signif != "ns") %>%
  add_xy_position(x = "Center")
bxp + 
  #stat_pvalue_manual(stat.test, label = "p") +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  ylab("LV posterior wall thickness (mm)")
# scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))

stat.test <- Patient4 %>%
  t_test(LV.septum.thickness ~ Center) %>%
  add_significance()
stat.test

# Box plots with p-values
bxp <- ggboxplot(Patient4, x = "Center", y = "LV.septum.thickness", fill = "#00AFBB")
stat.test <- stat.test %>%
  filter(p.adj.signif != "ns") %>%
  add_xy_position(x = "Center")
bxp + 
  #stat_pvalue_manual(stat.test, label = "p") +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  ylab("LV septum thickness (mm)")
# scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
# 

# Statistical test
stat.test <- Patient4 %>%
  t_test(LV_EF ~ Center) %>%
  add_significance()
stat.test

# Box plots with p-values
bxp <- ggboxplot(Patient4, x = "Center", y = "LV_EF", fill = "#00AFBB")
stat.test <- stat.test %>%
  filter(p.adj.signif != "ns") %>%
  add_xy_position(x = "Center")
bxp + 
  #stat_pvalue_manual(stat.test, label = "p") +
  # stat_pvalue_manual(stat.test, label = "p.adj")+
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  ylab("LV ejection fraction (%)")
# scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
# 
# # Customize p-value labels using glue expression 
# # https://github.com/tidyverse/glue
# bxp + stat_pvalue_manual(
#   stat.test, label = "T-test, p = {p}",
#   vjust = -1, bracket.nudge.y = 1
# ) +
#   scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))


############
Patient_tmp<-Patient[Patient$dialysis_time<10,]
Patient_tmp$dialysis_time2<-as.factor(floor(Patient_tmp$dialysis_time))

ggboxplot(Patient_tmp, x = "dialysis_time2", y = "LVMI",
             color = "dialysistype", palette = "jco",
             add = "jitter",short.panel.labs = FALSE)

Patient_tmp<-Patient[Patient$dialysis_time<6 & Patient$dialysistype =="HD",]
Patient_tmp<-Patient[Patient$dialysis_time<6,]
Patient_tmp$dialysis_time2<-as.factor(floor(Patient_tmp$dialysis_time))

library(rstatix)
Patient_tmp %>%
  group_by(dialysistype) %>%
  t_test(data =., LVMI ~ dialysis_time2) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")


stat.test <- Patient_tmp %>%
  t_test(LVMI ~ dialysis_time2) %>%
  add_significance()
stat.test

bxp <- ggboxplot(Patient_tmp, x = "dialysis_time2", y = "LV_EF", fill = "#00AFBB")
stat.test <- stat.test %>% add_xy_position(x = "dialysis_time2")
bxp + 
  #stat_pvalue_manual(stat.test, label = "p") +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)


ggboxplot(Patient_tmp, x = "dialysis_time2", y = "LVMI",
         color = "dialysis_time2", palette = "jco",
         add = "jitter",short.panel.labs = FALSE)+
  stat_compare_means(method = "t.test", label = "p.signif")





ggboxplot(Patient_tmp, x = "dialysis_time2", y = "LVMI",
         color = "dialysis_time2", palette = "jco",
         add = "jitter",short.panel.labs = FALSE)+
  stat_compare_means(method = "t.test", label = "p.signif", 
                     comparisons = list(
                       c("0", "1"),
                       c("0", "2"),
                       c("0", "3"),
                       c("0", "4"),
                       c("0", "5")
                     ))

ggboxplot(Patient_tmp, x = "dialysis_time2", y = "LV_RWT_cal",
         color = "dialysis_time2", palette = "jco",
         add = "jitter",short.panel.labs = FALSE)+
  stat_compare_means(method = "t.test", label = "p.signif")


