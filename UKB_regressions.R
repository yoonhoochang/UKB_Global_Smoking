#then regression it is!

#time since smoking cessation is for former smokers only
#PRS is divided into (smokers,non-smokers,all sample->record n)

#for additional analysis: pack years for all samples (0 for never smokers)

data <- prs_smoking_idp_all_confounds_imputed_pc_py0
attach(data)

#check brain measure rows
grep("n_25000_2_0",colnames(data))
grep("n_25010_2_0",colnames(data))
#58-68

#daily smoking 
#daily smoking corrected for pack years
#pack years
#time since smoking cessation (past smokers only)
#prs (all, never, ever)

#daily smoking 
write.table(paste("Name","Estimate","Std. Error", "z value", "P-value", sep="\t"), "daily_smoking_1107.txt", sep="\n", row.names=F, col.names=F, quote=F)
for (i in 58:68) {
  result1 <- lm(data[,i] ~ ever_daily_smoked + week_drinks + waist_hip + age_completed_imputed + Income + bmi+ dbp + sbp + site1_sex + site1_date + site1_age + site1_head_size + site1_rfMRI_motion + site1_tfMRI_motion + site1_age_2 + site1_date_2 + site1_age_sex + site2_sex + site2_date + site2_age + site2_head_size + site2_rfMRI_motion + site2_tfMRI_motion + site2_age_2 + site2_date_2 + site2_age_sex + site3_sex + site3_date + site3_age + site3_head_size + site3_rfMRI_motion + site3_tfMRI_motion + site3_age_2 + site3_date_2 + site3_age_sex + n_22009_0_1 + n_22009_0_10 + n_22009_0_2 + n_22009_0_3 + n_22009_0_4+n_22009_0_5+n_22009_0_6+n_22009_0_7+n_22009_0_8+n_22009_0_9)
  result2 <- summary.lm(result1)$coefficients[2,]
  write.table(paste(colnames(data)[i],result2[1],result2[2],result2[3],result2[4],sep="\t"), "daily_smoking_1107.txt", sep="\n", row.names=F, col.names=F, quote=F, append=T)
}

#daily smoking corrected for pack years
write.table(paste("Name","Estimate","Std. Error", "z value", "P-value", sep="\t"), "daily_smoking_allpy_1107.txt", sep="\n", row.names=F, col.names=F, quote=F)
for (i in 58:68) {
  result1 <- lm(data[,i] ~ ever_daily_smoked + all_packyears + week_drinks + waist_hip + age_completed_imputed + Income + bmi+ dbp + sbp + site1_sex + site1_date + site1_age + site1_head_size + site1_rfMRI_motion + site1_tfMRI_motion + site1_age_2 + site1_date_2 + site1_age_sex + site2_sex + site2_date + site2_age + site2_head_size + site2_rfMRI_motion + site2_tfMRI_motion + site2_age_2 + site2_date_2 + site2_age_sex + site3_sex + site3_date + site3_age + site3_head_size + site3_rfMRI_motion + site3_tfMRI_motion + site3_age_2 + site3_date_2 + site3_age_sex + n_22009_0_1 + n_22009_0_10 + n_22009_0_2 + n_22009_0_3 + n_22009_0_4+n_22009_0_5+n_22009_0_6+n_22009_0_7+n_22009_0_8+n_22009_0_9)
  result2 <- summary.lm(result1)$coefficients[2,]
  write.table(paste(colnames(data)[i],result2[1],result2[2],result2[3],result2[4],sep="\t"), "daily_smoking_allpy_1107.txt", sep="\n", row.names=F, col.names=F, quote=F, append=T)
}

#pack years
write.table(paste("Name","Estimate","Std. Error", "z value", "P-value", sep="\t"), "packyears_1107.txt", sep="\n", row.names=F, col.names=F, quote=F)
for (i in 58:68) {
  result1 <- lm(data[,i] ~ packyears_imputed + week_drinks + waist_hip + age_completed_imputed + Income + bmi+ dbp + sbp + site1_sex + site1_date + site1_age + site1_head_size + site1_rfMRI_motion + site1_tfMRI_motion + site1_age_2 + site1_date_2 + site1_age_sex + site2_sex + site2_date + site2_age + site2_head_size + site2_rfMRI_motion + site2_tfMRI_motion + site2_age_2 + site2_date_2 + site2_age_sex + site3_sex + site3_date + site3_age + site3_head_size + site3_rfMRI_motion + site3_tfMRI_motion + site3_age_2 + site3_date_2 + site3_age_sex + n_22009_0_1 + n_22009_0_10 + n_22009_0_2 + n_22009_0_3 + n_22009_0_4+n_22009_0_5+n_22009_0_6+n_22009_0_7+n_22009_0_8+n_22009_0_9)
  result2 <- summary.lm(result1)$coefficients[2,]
  write.table(paste(colnames(data)[i],result2[1],result2[2],result2[3],result2[4],sep="\t"), "packyears_1107.txt", sep="\n", row.names=F, col.names=F, quote=F, append=T)
}

#time since smoking cessation (past smokers only)
#filtering for past daily smokers (so remove current daily, and NAs)

data_cessation <- prs_smoking_idp_all_confounds_imputed_pc_py0 %>% filter(time_since_cessation != 0) %>% filter(!is.na(time_since_cessation))

attach(data_cessation)

write.table(paste("Name","Estimate","Std. Error", "z value", "P-value", sep="\t"), "cessation_past_1107.txt", sep="\n", row.names=F, col.names=F, quote=F)
for (i in 58:68) {
  result1 <- lm(data_cessation[,i] ~ time_since_cessation  + packyears_imputed + week_drinks + waist_hip + age_completed_imputed + Income + bmi+ dbp + sbp + site1_sex + site1_date + site1_age + site1_head_size + site1_rfMRI_motion + site1_tfMRI_motion + site1_age_2 + site1_date_2 + site1_age_sex + site2_sex + site2_date + site2_age + site2_head_size + site2_rfMRI_motion + site2_tfMRI_motion + site2_age_2 + site2_date_2 + site2_age_sex + site3_sex + site3_date + site3_age + site3_head_size + site3_rfMRI_motion + site3_tfMRI_motion + site3_age_2 + site3_date_2 + site3_age_sex + n_22009_0_1 + n_22009_0_10 + n_22009_0_2 + n_22009_0_3 + n_22009_0_4+n_22009_0_5+n_22009_0_6+n_22009_0_7+n_22009_0_8+n_22009_0_9)
  result2 <- summary.lm(result1)$coefficients[2,]
  write.table(paste(colnames(data)[i],result2[1],result2[2],result2[3],result2[4],sep="\t"), "cessation_past_1107.txt", sep="\n", row.names=F, col.names=F, quote=F, append=T)
}

#prs (all, never, ever) #for every threshold
#all

data_prs_all <- data %>% filter(!is.na(Pt_0.5))
attach(data_prs_all)
#27447
#thresholds: 0.05, 0.1, 0.3, 0.5

write.table(paste("Name","Estimate","Std. Error", "z value", "P-value", sep="\t"), "prs_all_0.5_1107.txt", sep="\n", row.names=F, col.names=F, quote=F)
for (i in 58:68) {
  result1 <- lm(data_prs_all[,i] ~ scale(Pt_0.5)  + week_drinks + waist_hip + age_completed_imputed + Income + bmi+ dbp + sbp + site1_sex + site1_date + site1_age + site1_head_size + site1_rfMRI_motion + site1_tfMRI_motion + site1_age_2 + site1_date_2 + site1_age_sex + site2_sex + site2_date + site2_age + site2_head_size + site2_rfMRI_motion + site2_tfMRI_motion + site2_age_2 + site2_date_2 + site2_age_sex + site3_sex + site3_date + site3_age + site3_head_size + site3_rfMRI_motion + site3_tfMRI_motion + site3_age_2 + site3_date_2 + site3_age_sex + n_22009_0_1 + n_22009_0_10 + n_22009_0_2 + n_22009_0_3 + n_22009_0_4+n_22009_0_5+n_22009_0_6+n_22009_0_7+n_22009_0_8+n_22009_0_9)
  result2 <- summary.lm(result1)$coefficients[2,]
  write.table(paste(colnames(data)[i],result2[1],result2[2],result2[3],result2[4],sep="\t"), "prs_all_0.5_1107.txt", sep="\n", row.names=F, col.names=F, quote=F, append=T)
}

#daily smoked
data_prs_smokers <- data %>% filter(ever_daily_smoked == "1")
attach(data_prs_smokers)

write.table(paste("Name","Estimate","Std. Error", "z value", "P-value", sep="\t"), "prs_smokers_0.5_1107.txt", sep="\n", row.names=F, col.names=F, quote=F)
for (i in 58:68) {
  result1 <- lm(data_prs_smokers[,i] ~ scale(Pt_0.5) + week_drinks + waist_hip + age_completed_imputed + Income + bmi+ dbp + sbp + site1_sex + site1_date + site1_age + site1_head_size + site1_rfMRI_motion + site1_tfMRI_motion + site1_age_2 + site1_date_2 + site1_age_sex + site2_sex + site2_date + site2_age + site2_head_size + site2_rfMRI_motion + site2_tfMRI_motion + site2_age_2 + site2_date_2 + site2_age_sex + site3_sex + site3_date + site3_age + site3_head_size + site3_rfMRI_motion + site3_tfMRI_motion + site3_age_2 + site3_date_2 + site3_age_sex + n_22009_0_1 + n_22009_0_10 + n_22009_0_2 + n_22009_0_3 + n_22009_0_4+n_22009_0_5+n_22009_0_6+n_22009_0_7+n_22009_0_8+n_22009_0_9)
  result2 <- summary.lm(result1)$coefficients[2,]
  write.table(paste(colnames(data)[i],result2[1],result2[2],result2[3],result2[4],sep="\t"), "prs_smokers_0.5_1107.txt", sep="\n", row.names=F, col.names=F, quote=F, append=T)
}

#never smoked
data_prs_never_smokers <- data %>% filter(ever_daily_smoked =="0")
attach(data_prs_never_smokers)

write.table(paste("Name","Estimate","Std. Error", "z value", "P-value", sep="\t"), "prs_never_smokers_0.5_1107.txt", sep="\n", row.names=F, col.names=F, quote=F)
for (i in 58:68) {
  result1 <- lm(data_prs_never_smokers[,i] ~ scale(Pt_0.5)  + week_drinks + waist_hip + age_completed_imputed + Income + bmi+ dbp + sbp + site1_sex + site1_date + site1_age + site1_head_size + site1_rfMRI_motion + site1_tfMRI_motion + site1_age_2 + site1_date_2 + site1_age_sex + site2_sex + site2_date + site2_age + site2_head_size + site2_rfMRI_motion + site2_tfMRI_motion + site2_age_2 + site2_date_2 + site2_age_sex + site3_sex + site3_date + site3_age + site3_head_size + site3_rfMRI_motion + site3_tfMRI_motion + site3_age_2 + site3_date_2 + site3_age_sex + n_22009_0_1 + n_22009_0_10 + n_22009_0_2 + n_22009_0_3 + n_22009_0_4+n_22009_0_5+n_22009_0_6+n_22009_0_7+n_22009_0_8+n_22009_0_9)
  result2 <- summary.lm(result1)$coefficients[2,]
  write.table(paste(colnames(data)[i],result2[1],result2[2],result2[3],result2[4],sep="\t"), "prs_never_smokers_0.5_1107.txt", sep="\n", row.names=F, col.names=F, quote=F, append=T)
}

#repeated for other PRS threshold

