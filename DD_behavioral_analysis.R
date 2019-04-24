# GO TO DIRECTORY WHERE PREPROCESED BEHAVIORAL DATA HAS BEEN OUTPUT 
library(ggplot2)
library(afex)
# merge data by condition into one data frame 
DD_behavioral <- rbind(DD_behavioral_analysis_A, DD_behavioral_analysis_E, DD_behavioral_analysis_C)

# test normality 
shapiro.test(DD_behavioral$k_smallM)
shapiro.test(DD_behavioral$k_largeM)
# both significantly different from normal distribution 

# get log() for discounting coefficient (-> normally distributed)
DD_behavioral <- DD_behavioral %>% mutate(logk_s = log(k_smallM), 
                                          logk_l = log(k_largeM))

# test normality again 
shapiro.test(DD_behavioral$logk_s)
shapiro.test(DD_behavioral$logk_l)
# both okay now 


############
# first analyses -- small magnitude discounting, hyperbolic model 
# first plot 
ggplot(DD_behavioral, aes(x = factor(condition), y = logk_s, 
           color = condition)) + geom_boxplot() + 
  ylab("log(k)") + xlab("condition")
# no apparent differences.. check ANOVA 

fit_smK <- afex::aov_ez("subject_num", "logk_s", DD_behavioral, within=c("condition"))
summary(fit_smK)

# F = 0.0012, p = 0.9988, Greenhouse-Geisser correction: p = 0.9963
# no significant differences... 
############


## next, large magnitude for hyperbolic model coefficient 

ggplot(DD_behavioral, aes(x = factor(condition), y = logk_l, 
                          color = condition)) + geom_boxplot() + 
  ylab("log(k)") + xlab("condition")
## looks like E has greater variance, but no apparent large differences ..
fit_lgK <- afex::aov_ez("subject_num", "logk_l", DD_behavioral, within=c("condition"))
summary(fit_lgK)

# F = 0.0949, p = 0.9098, Greenhouse-Geisser correction: p = 0.8566
# no significant differences 
############

## next, looking at AUC 
# first, just AUC by condition 

# test normality 
shapiro.test(DD_behavioral$AUC_smallM)
shapiro.test(DD_behavioral$AUC_largeM)
# BOTH violate 

# .. see the distributions 
ggplot(DD_behavioral, aes(x=AUC_smallM)) + 
  geom_density()

ggplot(DD_behavioral, aes(x=AUC_largeM)) + 
  geom_density()

#.. to validate, let's bootstrap the normality test results 
boot_AUCsmall_normality_test <- NULL 
boot_AUClarge_normality_test <- NULL 
for(i in 1:10000){
  AUCsmall_resample <- sample(DD_behavioral$AUC_smallM, nrow(DD_behavioral), replace = TRUE)
  AUClarge_resample <- sample(DD_behavioral$AUC_largeM, nrow(DD_behavioral), replace = TRUE)
  boot_AUCsmall_normality_test[i] <- shapiro.test(AUCsmall_resample)$p.value
  boot_AUClarge_normality_test[i] <- shapiro.test(AUClarge_resample)$p.value
}

# 99% CI 
CI_small <- quantile(boot_AUCsmall_normality_test, c(.015, .995))
CI_large <- quantile(boot_AUClarge_normality_test, c(.015, .995))


# test against normality in sample 
small_P <- shapiro.test(DD_behavioral$AUC_smallM)$p.value
large_P <- shapiro.test(DD_behavioral$AUC_largeM)$p.value

small_P > CI_small[1]
small_P < CI_small[2]
# TRUE 

large_P > CI_large[1]
large_P < CI_large[2]
# ALSO TRUE 

# ... the normality test results are within that of the 99% CI estimate 
# of the population parameter 

# .. onto the tests for the small magnitude: 

# first plot 
ggplot(DD_behavioral, aes(x = factor(condition), y = AUC_smallM, 
                          color = condition)) + geom_boxplot() + 
  ylab("log(k)") + xlab("condition")
# no apparent differences 

fit_smAUC <- afex::aov_ez("subject_num", "AUC_smallM", DD_behavioral, within=c("condition"))
summary(fit_smAUC)
# F = 0.0127, p = 0.9874, Greenhouse-Geisser correction: p = 0.972
# no differences 


########
# large magnitude AUC 
ggplot(DD_behavioral, aes(x = factor(condition), y = AUC_largeM, 
                          color = condition)) + geom_boxplot() + 
  ylab("log(k)") + xlab("condition")
# again, no apparent differences 
fit_lgAUC <- afex::aov_ez("subject_num", "AUC_largeM", DD_behavioral, within=c("condition"))
summary(fit_lgAUC)
# F = 0.0418, p = 0.9591, Greenhouse-Geisser correction: p = 0.9371
# no differences 


####### finally, just to check, a proportional change for 
#prospection/baseline using each subject's control condition 

# split 
DD_EFT <- subset(DD_behavioral, condition == "E")
DD_AFT <- subset(DD_behavioral, condition == "A")
DD_CTRL <- subset(DD_behavioral, condition == "C")

# get just AUC info and subject number
DD_EFT_AUC <- DD_EFT[,c("subject_num", "AUC_smallM", "AUC_largeM")]
colnames(DD_EFT_AUC)[2:3] <- c("AUC_smallM_E", "AUC_largeM_E")
DD_AFT_AUC <- DD_AFT[,c("subject_num", "AUC_smallM", "AUC_largeM")]
colnames(DD_AFT_AUC)[2:3] <- c("AUC_smallM_A", "AUC_largeM_A")
DD_CTRL_AUC <- DD_CTRL[,c("subject_num", "AUC_smallM", "AUC_largeM")]
colnames(DD_CTRL_AUC)[2:3] <- c("AUC_smallM_C", "AUC_largeM_C")

# join by subj number
AUC_EC <- full_join(DD_EFT_AUC, DD_CTRL_AUC)
AUC_EC <- na.omit(AUC_EC)
AUC_AC <- full_join(DD_AFT_AUC, DD_CTRL_AUC)
AUC_AC <- na.omit(AUC_AC)


# get proportional change 
AUC_EC <- AUC_EC %>% mutate(AUC_diff_sm = AUC_smallM_E/AUC_smallM_C, 
                            AUC_diff_lg = AUC_largeM_E/AUC_largeM_C)

AUC_AC <- AUC_AC %>% mutate(AUC_diff_sm = AUC_smallM_A/AUC_smallM_C, 
                            AUC_diff_lg = AUC_smallM_A/AUC_largeM_C)

# visualize small M proportional change in AUC from control condition 
boxplot(AUC_EC$AUC_diff_sm, AUC_AC$AUC_diff_sm)

# visualize large M proportional change in AUC from control condition 
boxplot(AUC_EC$AUC_diff_lg, AUC_AC$AUC_diff_lg)

which((AUC_EC$subject_num %in% AUC_AC$subject_num) == FALSE)
which((AUC_AC$subject_num %in% AUC_EC$subject_num) == FALSE)

# 108, 109, 112, 114
AUC_EC <- subset(AUC_EC, (subject_num %in% c("s108", "s109", "s112", "s114") == FALSE))
AUC_AC <- subset(AUC_AC, (subject_num %in% c("s108", "s109", "s112", "s114") == FALSE))


# test for differences between prospection conditions 
# small magnitude 
t.test(AUC_EC$AUC_diff_sm, AUC_AC$AUC_diff_sm, paired = TRUE)

# large magnitude 
t.test(AUC_EC$AUC_diff_lg, AUC_AC$AUC_diff_lg, paired = TRUE)

# no differences 
