
# combine all by-condition data
GNG <- rbind(GNG_behavioral_analysis_A, GNG_behavioral_analysis_E, GNG_behavioral_analysis_C)


# first, check normality of variables to be analyzed for ANOVAs


# accuracy rate on go trials 
shapiro.test(GNG$acc_go)
# NON-NORMAL 
# check distribution 
ggplot(GNG, aes(x=acc_go)) + 
  geom_density()
# VERY left skewed


# accuracy rate on no-go trials 
shapiro.test(GNG$acc_nogo)
# NON-NORMAL 
# check distribution 
ggplot(GNG, aes(x=acc_nogo)) + 
  geom_density()
# VERY left skewed

 
# mean RT, go trials only-- correct (response actually given)
shapiro.test(GNG$meanRT_go_cor)
# NON-NORMAL 
# check distribution 
ggplot(GNG, aes(x=meanRT_go_cor)) + 
  geom_density()
# right skewed

# mean RT, incorrect responses on no-go trials (NOTE small n)
shapiro.test(GNG$meanRT_nogo_incor)
# NON-NORMAL 
# check distribution 
ggplot(GNG, aes(x=meanRT_nogo_incor)) + 
  geom_density()
# right skewed


# bootstrap to test against population estimate CI for normality violation
boot_acc_go_norm <- NULL 
boot_acc_nogo_norm <- NULL 
boot_RT_go_norm <- NULL 
boot_RT_nogo_norm <- NULL 

for(i in 1:10000){
  resample_data <- sample_n(GNG, size = nrow(GNG), replace = TRUE)
  boot_acc_go_norm[i] <- shapiro.test(resample_data$acc_go)$p.value
  boot_acc_nogo_norm[i] <- shapiro.test(resample_data$acc_nogo)$p.value
  boot_RT_go_norm[i] <- shapiro.test(resample_data$meanRT_go_cor)$p.value
  boot_RT_nogo_norm[i] <- shapiro.test(resample_data$meanRT_nogo_incor)$p.value
}

# get real data test results as variables 
p_acc_go <- shapiro.test(GNG$acc_go)$p.value
p_acc_nogo <- shapiro.test(GNG$acc_nogo)$p.value
p_RT_go <- shapiro.test(GNG$meanRT_go_cor)$p.value
p_RT_nogo <- shapiro.test(GNG$meanRT_nogo_incor)$p.value

# get 99% CIs 
boot_acc_go_norm_CI <- quantile(boot_acc_go_norm, c(.005, .995))
boot_acc_nogo_norm_CI <- quantile(boot_acc_nogo_norm, c(.005, .995))
boot_RT_go_norm_CI <- quantile(boot_RT_go_norm, c(.005, .995))
boot_RT_nogo_norm_CI <- quantile(boot_RT_nogo_norm, c(.005, .995))

# go accuracy 
p_acc_go > boot_acc_go_norm_CI[1]
p_acc_go < boot_acc_go_norm_CI[2]

# nogo accuracy 
p_acc_nogo > boot_acc_nogo_norm_CI[1]
p_acc_nogo < boot_acc_nogo_norm_CI[2]

# go RT 
p_RT_go > boot_RT_go_norm_CI[1]
p_RT_go < boot_RT_go_norm_CI[2]

# nogo RT 
p_RT_nogo > boot_RT_nogo_norm_CI[1]
p_RT_nogo < boot_RT_nogo_norm_CI[2]



#### ALL TRUE 
# DEVIATION FROM NORMALITY IS EXPECTED IN THE POPULATION'S DISTRIBUTIONS 

## ANALYSES 
# acc_go
# acc_nogo
# meanRT_go_cor
# meanRT_nogo_incor

# go accuracy 
ggplot(GNG, aes(x = factor(condition), y = acc_go, 
                          color = condition)) + geom_boxplot() + 
  ylab("log(k)") + xlab("condition")
# does not appear to have any differences 

## looks like E has greater variance, but no apparent large differences ..
fit <- afex::aov_ez("subject_num", "acc_go", GNG, within=c("condition"))
summary(fit)
# F = 1.3584, p = 0.2724, Greenhouse-Guisser corrected: p = 0.2667
# NO DIFFERENCES BETWEEN CONDITIONS 

#########

# no-go accuracy 
ggplot(GNG, aes(x = factor(condition), y = acc_nogo, 
                color = condition)) + geom_boxplot() + 
  ylab("log(k)") + xlab("condition")
# maybe difference between A and E OR A and C 

fit <- afex::aov_ez("subject_num", "acc_nogo", GNG, within=c("condition"))
summary(fit)
# F = 0.3203, p = 0.7283, Greenhouse-Guisser corrected: p = 0.723
# NO DIFFERENCES BETWEEN CONDITIONS 

#########

# go trials mean RT 
ggplot(GNG, aes(x = factor(condition), y = meanRT_go_cor, 
                color = condition)) + geom_boxplot() + 
  ylab("log(k)") + xlab("condition")
# maybe difference between A and E, but not A/C or E/C .. 

fit <- afex::aov_ez("subject_num", "meanRT_go_cor", GNG, within=c("condition"))
summary(fit)
# F = 0.0455, p = 0.9556, Greenhouse-Guisser corrected: p = 0.65015
# NO DIFFERENCES BETWEEN CONDITIONS 


####### and finally.. 

# no-go (incorrect) trials mean RT 
ggplot(GNG, aes(x = factor(condition), y = meanRT_nogo_incor, 
                color = condition)) + geom_boxplot() + 
  ylab("log(k)") + xlab("condition")
# maybe difference between E and C ? 

fit <- afex::aov_ez("subject_num", "meanRT_nogo_incor", GNG, within=c("condition"))
summary(fit)
# F = 1.5508, p = 0.2286, Greenhouse-Guisser corrected: p = 0.23
# NO DIFFERENCES BETWEEN CONDITIONS 

