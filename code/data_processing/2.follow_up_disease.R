

PatientsDiseases

# 5.5 Disease -------------------------------------------------------------


# 5.1 combine with Patient ------------------------------------------------------------

PatientsDiseases_match <- Patient[,c("patid","soascaseid","centreid","tpxdate","graft_date")]  %>%
  left_join(PatientsDiseases, by = c("patid","centreid"))

PatientsDiseases_match$note<-""

dim(PatientsDiseases) # 35213
dim(PatientsDiseases_match)# 35946


# 5.2 filter date_in <= graft_date ------------------------------------------------------------
# 5.2.1 fix missing date -------------------------------------------------------
PatientsDiseases_match2 <- PatientsDiseases_match %>%
  rowwise() %>%
  mutate(Diseases_theo_assdate = tpxdate+assperiod*30.5, .before = "assperiod") %>%
  mutate(Diseases_date_gap = date_in - Diseases_theo_assdate, .before = "assperiod")

a<-PatientsDiseases_match2[PatientsDiseases_match2$assperiod !=0 & is.na(PatientsDiseases_match2$date_in),]
dim(a) #335


PatientsDiseases_match2[is.na(PatientsDiseases_match2$date_in),]$note <- paste0("date_in:",
                                                                                PatientsDiseases_match2[is.na(PatientsDiseases_match2$date_in),]$date_in,"_Diseases_theo_assdate",
                                                                                PatientsDiseases_match2[is.na(PatientsDiseases_match2$date_in),]$Diseases_theo_assdate,";"
)

PatientsDiseases_match2[is.na(PatientsDiseases_match2$date_in),]$date_in <- PatientsDiseases_match2[is.na(PatientsDiseases_match2$date_in),]$Diseases_theo_assdate

# 5.2.2 filter by date_in -------------------------------------------------------
# assume date_in is correct, but period might be wrong
dim(PatientsDiseases_match2) # 35946
PatientsDiseases_match3 <- PatientsDiseases_match2 %>%
  filter(date_in <= graft_date) 
dim(PatientsDiseases_match3) # 35314

# update
PatientsDiseases_match3 <- PatientsDiseases_match3 %>%
  rowwise() %>%
  mutate(Diseases_date_gap = date_in - Diseases_theo_assdate, .before = "assperiod") %>%
  mutate(Dis_assperiod = (date_in-tpxdate)/30.5, .before = "assperiod")  %>%
  mutate(Dis_ass_days = date_in-tpxdate, .before = "assperiod") 

PatientsDiseases_followup_final <- PatientsDiseases_match3
PatientsDiseases_baseline <- PatientsDiseases_followup_final[PatientsDiseases_followup_final$date_in <= PatientsDiseases_followup_final$tpxdate,]



write.csv(PatientsDiseases_followup_final,"PatientsDiseases_followup_final.csv", row.names = FALSE, na="")
saveRDS(PatientsDiseases_followup_final, file = "PatientsDiseases_followup_final.rds")



