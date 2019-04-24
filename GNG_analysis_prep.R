library(reshape2)
library(plyr)
library(easycsv)


# CLEAR GLOBAL WORK ENVIRONMENT
# current condition = C 
# SET FOLDER WITH CURRENT CONDITION INDIVIDUAL DATA AS WORKING DIRECTORY


# FIRST IMPORT ALL CSVs AND APPEND _[CONDITION_LABEL] TO END OF NAME

temp = list.files(pattern="*.csv")

subj_list_strings <- NULL
for(i in 1:length(temp)){
  assign(paste(strsplit(temp[i], '.csv')[1], "_C", sep = ''), read_csv(temp[i]))
  subj_list_strings[i] <- paste(strsplit(temp[i], '.csv')[1], "_C", sep = '')
}

# get list of subjects 
subj_list <- lapply(names(which(unlist(eapply(.GlobalEnv,is.data.frame)))), as.name)


# analyze all subject data together: 

# first initialize matrix  
all_gng_analys <- matrix(data = NA, nrow = length(subj_list), 
                        ncol = 11, dimnames = list(NULL, 
                                                  c("subject_num", "session_num",
                                                    "task_type", "condition", 
                                                    "acc_rate", "acc_go", 
                                                    "acc_nogo", "meanRT", 
                                                    "meanRT_go", 
                                                    "meanRT_go_cor", "meanRT_nogo_incor")), 
                        byrow = FALSE)

# iterate over all subjects to get statistics 
# for reaction time and accuracy analyses 
for(iSubj in 1:length(subj_list)){
  # set current data
  subj_data <- as.data.frame(eval(subj_list[[iSubj]]))
  # get the name as a string
  subject_string <- as.character(subj_list_strings[iSubj])
  # get the basic info: subject number, session number, which task, condition
  cols_subject_info <- as.matrix(colsplit(string=subject_string, pattern="_", names=c("subject_num", "session_num", "task_type", "condition")))
  # put in matrix 
   all_gng_analys[iSubj, 1:4] <- cols_subject_info

  # subset go / nogo
  go_trials <- subset(subj_data, target_type == 'GO')
  nogo_trials <- subset(subj_data, target_type == 'NO-GO')
  
  
  # get overall accuracy 
  acc <- length(which(subj_data$cresp == subj_data$response))/nrow(subj_data)
  all_gng_analys[iSubj, "acc_rate"] <- acc
  
  # get go accuracy 
  acc_subGo <- length(which(go_trials$cresp == go_trials$response))/nrow(go_trials)
  all_gng_analys[iSubj, "acc_go"] <- acc_subGo


  # get nogo accuracy 
  acc_subNogo <- length(which(nogo_trials$cresp == nogo_trials$response))/nrow(nogo_trials)
  all_gng_analys[iSubj, "acc_nogo"] <- acc_subNogo
  
  
  # mean RT overall 
  all_gng_analys[iSubj, "meanRT"] <- mean(subj_data$response_time)
  # mean RT for just go trials (really only RT that matters)
  all_gng_analys[iSubj, "meanRT_go"] <- mean(go_trials$response_time)

  
  # subset correct go trials and incorrect no-go trials 
  cor_go <- subset(go_trials, cresp == response)
  incor_nogo <- subset(nogo_trials, cresp != response)
  
  # mean RT correct go 
  all_gng_analys[iSubj, "meanRT_go_cor"] <- mean(cor_go$response_time)
  
  # mean RT incorrect nogo 
  all_gng_analys[iSubj, "meanRT_nogo_incor"] <- mean(incor_nogo$response_time)
  
  
}

# set as data frame 
all_gng_processed <- as.data.frame(all_gng_analys)


# write as CSV 
# CHANGE CONDITION LETTER HERE 
write.csv(all_gng_processed, "GNG_behavioral_analysis_C.csv")

