library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(afex)


# FOR FIRST TIME FILE IS RUN ONLY: 

# read in data 

LPP_E <- read_csv('LPP_AVE_E_MEAS.csv')
LPP_A <- read_csv('LPP_AVE_A_MEAS.csv')
LPP_C <- read_csv('LPP_AVE_C_MEAS.csv')

# get subj number and condition 
LPP_E <- LPP_E %>% mutate(subj_num = substr(LPP_E$ERPset, 1, 3),
                            condition = "E")
LPP_A <- LPP_A %>% mutate(subj_num = substr(LPP_A$ERPset, 1, 3),
                            condition = "A")
LPP_C <- LPP_C %>% mutate(subj_num = substr(LPP_C$ERPset, 1, 3),
                            condition = "C")


# merge conditions
LPP_ACE <- full_join(LPP_E, LPP_A)
LPP_ACE <- full_join(LPP_ACE, LPP_C)

# get session number 
LPP_ACE <- LPP_ACE %>% mutate(se_num = substr(ERPset, 7,7))


# write to CSV 
# write.csv(LPP_ACE, "LPP_ACE_prepped.csv")


###############################################################

# FOR SUBSEQUENT ANALYSES : 

LPP_ACE <- read_csv("LPP_ACE_prepped.csv")

# get each DV for average of channels 
LPP_ACE <- LPP_ACE %>% mutate(neutral_ave = (bin1_baseline_neutral_Fz 
                                             + bin1_baseline_neutral_CPz 
                                             + bin1_baseline_neutral_Cz 
                                             + bin1_baseline_neutral_Pz)/4, 
                              basepos_ave = (bin2_baseline_positive__Fz 
                                             + bin2_baseline_positive__CPz 
                                             + bin2_baseline_positive__Cz 
                                             + bin2_baseline_positive__Pz)/4, 
                              suppr_ave = (bin3_main_suppress_positive_Fz 
                                           + bin3_main_suppress_positive_Cz 
                                           + bin3_main_suppress_positive_CPz 
                                           + bin3_main_suppress_positive_Pz)/4, 
                              # suppression minus baseline differences for each electrode 
                              Fz_suppr_minus_baseline = bin3_main_suppress_positive_Fz - bin2_baseline_positive__Fz, 
                              Cz_suppr_minus_baseline = bin3_main_suppress_positive_Cz - bin2_baseline_positive__Cz, 
                              Fz_suppr_minus_baseline = bin3_main_suppress_positive_Fz - bin2_baseline_positive__Fz, 
                              CPz_suppr_minus_baseline = bin3_main_suppress_positive_CPz - bin2_baseline_positive__CPz, 
                              Pz_suppr_minus_baseline = bin3_main_suppress_positive_Pz - bin2_baseline_positive__Pz,                           
                              # and average of all suppression minus baseline difference waves 
                              ave_suppr_minus_baseline = (Fz_suppr_minus_baseline 
                                                          + Cz_suppr_minus_baseline 
                                                          + CPz_suppr_minus_baseline 
                                                          + Pz_suppr_minus_baseline)/4)



# resplit for later use 
LPP_ACE_A <- subset(LPP_ACE, condition == "A")
LPP_ACE_C <- subset(LPP_ACE, condition == "C")
LPP_ACE_E <- subset(LPP_ACE, condition == "E")


#####################################

# first neutral condition 

# ave 
fit <- afex::aov_ez("subj_num", "neutral_ave", LPP_ACE, within=c("condition"))
summary(fit)
# INSIGNIFICANT 
shapiro.test(LPP_ACE_A$neutral_ave)
shapiro.test(LPP_ACE_E$neutral_ave)
shapiro.test(LPP_ACE_C$neutral_ave)
# DOES NOT VIOLATE, JUST INSIGNIFICANT 


# at each electrode site 

# FZ 
fit <- afex::aov_ez("subj_num", "bin1_baseline_neutral_Fz", LPP_ACE, within=c("condition"))
summary(fit)
# INSIGNIFICANT 
shapiro.test(LPP_ACE_A$bin1_baseline_neutral_Fz)
shapiro.test(LPP_ACE_E$bin1_baseline_neutral_Fz)
shapiro.test(LPP_ACE_C$bin1_baseline_neutral_Fz)
# DOES NOT VIOLATE, JUST INSIGNIFICANT 

# CZ 
fit <- afex::aov_ez("subj_num", "bin1_baseline_neutral_Cz", LPP_ACE, within=c("condition"))
summary(fit)
# INSIGNIFICANT 
shapiro.test(LPP_ACE_A$bin1_baseline_neutral_Cz)
shapiro.test(LPP_ACE_E$bin1_baseline_neutral_Cz)
shapiro.test(LPP_ACE_C$bin1_baseline_neutral_Cz)
# DOES NOT VIOLATE, JUST INSIGNIFICANT 


# CPZ 
fit <- afex::aov_ez("subj_num", "bin1_baseline_neutral_CPz", LPP_ACE, within=c("condition"))
summary(fit)
# INSIGNIFICANT 
shapiro.test(LPP_ACE_A$bin1_baseline_neutral_CPz)
shapiro.test(LPP_ACE_E$bin1_baseline_neutral_CPz)
shapiro.test(LPP_ACE_C$bin1_baseline_neutral_CPz)
# DOES NOT VIOLATE, JUST INSIGNIFICANT 

# PZ 
fit <- afex::aov_ez("subj_num", "bin1_baseline_neutral_Pz", LPP_ACE, within=c("condition"))
summary(fit)
# INSIGNIFICANT 
shapiro.test(LPP_ACE_A$bin1_baseline_neutral_Pz)
shapiro.test(LPP_ACE_E$bin1_baseline_neutral_Pz)
shapiro.test(LPP_ACE_C$bin1_baseline_neutral_Pz)
# DOES NOT VIOLATE, JUST INSIGNIFICANT 

#####################################

# baseline positive condition.. 


#ave
fit <- afex::aov_ez("subj_num", "basepos_ave", LPP_ACE, within=c("condition"))
summary(fit)

# SIGNIFICANT 
shapiro.test(LPP_ACE_A$basepos_ave)
shapiro.test(LPP_ACE_E$basepos_ave)
shapiro.test(LPP_ACE_C$basepos_ave)
# DOES NOT VIOLATE 

# t.tests 
t.A <- subset(LPP_ACE_A, (subj_num %in% c(102, 103, 108, 112, 114, 119) == FALSE))
t.E <- subset(LPP_ACE_E, (subj_num %in% c(102, 103, 108, 112, 114, 119) == FALSE))
t.C <- subset(LPP_ACE_C, (subj_num %in% c(102, 103, 108, 112, 114, 119) == FALSE))

# run t-tests 
AC <- t.test(t.A$basepos_ave, t.C$basepos_ave, paired = TRUE)
EC <- t.test(t.E$basepos_ave, t.C$basepos_ave, paired = TRUE)
AE <- t.test(t.A$basepos_ave, t.E$basepos_ave, paired = TRUE)

# view results 
AC
EC
AE

# get p.value vector for corrections 
p <- c(AC$p.value, EC$p.value, AE$p.value)
# correct using Holm-Bonferonni method 

p.adjusted <- p.adjust(p, method = "holm")
p.adjusted


## ...... (basically all the rest is the same!) ...... ## 


#### example of normality violation check 

# (baseline positive, site CPz)

# cpz 
fit <- afex::aov_ez("subj_num", "bin2_baseline_positive__CPz", LPP_ACE, within=c("condition"))
summary(fit)
# TREND LEVEL  , .051
# check normality 
shapiro.test(LPP_ACE_A$bin2_baseline_positive__CPz)
shapiro.test(LPP_ACE_E$bin2_baseline_positive__CPz)
shapiro.test(LPP_ACE_C$bin2_baseline_positive__CPz)
# AFT condition - DATA VIOLATES NORMALITY, .028
# check skew 
skewness(LPP_ACE_A$bin2_baseline_positive__CPz)
# check if skewness squared + 1 is less than kurtosis 
skewness(LPP_ACE_A$bin2_baseline_positive__CPz)^2 + 1
# check kurtosis 
kurtosis(LPP_ACE_A$bin2_baseline_positive__CPz)

########
# bootstrap population estimates for skew and kurtosis 
sample_skew <- NULL 
sample_kurt <- NULL 

for(i in 1:10000){
  sample_v <- sample(LPP_ACE$bin2_baseline_positive__CPz, length(LPP_ACE$bin2_baseline_positive__CPz), replace = TRUE)
  sample_skew[i] <- skewness(sample_v)
  sample_kurt[i] <- kurtosis(sample_v)
}
skew_CI_lpp_basepos_A <- c(mean(sample_skew) - 2.576*(sd(sample_skew)/sqrt(length(sample_skew))), 
                           mean(sample_skew) + 2.576*(sd(sample_skew)/sqrt(length(sample_skew))))

kurt_CI_lpp_basepos_A<- c(mean(sample_kurt) - 2.576*(sd(sample_kurt)/sqrt(length(sample_kurt))), 
                          mean(sample_kurt) + 2.576*(sd(sample_kurt)/sqrt(length(sample_kurt))))
# NONE IN BOOTSTRAP
# CONCLUSION: AFT DATA VIOLATES NORMALITY, BUT MORE SKEW THAN ESTIMATED DEVIATION IN POPULATION
                                            # AND SMALLER KURTOSIS 

######## 
