library(reshape2)
library(easycsv)
library(readr)
library(dplyr)


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
all_ddRT_analys <- as.data.frame(matrix(data = 0, nrow = length(subj_list), 
                        ncol = 28, dimnames = list(NULL, c("subject_num", "session_num",
                                                    "task_type", "condition", 
                                                    "delayed_d1_Lg_meanRT", "delayed_d1_Sm_meanRT", 
                                                    "delayed_d2_Lg_meanRT", "delayed_d2_Sm_meanRT",
                                                    "delayed_d3_Lg_meanRT", "delayed_d3_Sm_meanRT", 
                                                    "delayed_d4_Lg_meanRT", "delayed_d4_Sm_meanRT", 
                                                    "delayed_d5_Lg_meanRT", "delayed_d5_Sm_meanRT", 
                                                    "delayed_d6_Lg_meanRT", "delayed_d6_Sm_meanRT", 
                                                    "immediate_d1_Lg_meanRT", "immediate_d1_Sm_meanRT", 
                                                    "immediate_d2_Lg_meanRT", "immediate_d2_Sm_meanRT",
                                                    "immediate_d3_Lg_meanRT", "immediate_d3_Sm_meanRT", 
                                                    "immediate_d4_Lg_meanRT", "immediate_d4_Sm_meanRT", 
                                                    "immediate_d5_Lg_meanRT", "immediate_d5_Sm_meanRT", 
                                                    "immediate_d6_Lg_meanRT", "immediate_d6_Sm_meanRT")), 
                        byrow = FALSE))

# iterate over all subjects to get statistics 
# for reaction time analyses 
for(iSubj in 1:length(subj_list)){
  # set current data
  subj_data <- as.data.frame(eval(subj_list[[iSubj]]))
  # get the name as a string
  subject_string <- as.character(subj_list_strings[iSubj])
  # get the basic info: subject number, session number, which task, condition
  cols_subject_info <- as.data.frame(colsplit(string=subject_string, pattern="_", names=c("subject_num", "session_num", "task_type", "condition")))
  # put in matrix 
  all_ddRT_analys[iSubj, 1:4] <- cols_subject_info

  # get just the question order value per delay
  subj_data <- subj_data %>% mutate(whichQ = 0)
  for(rowN in 1:nrow(subj_data)){
    subj_data$whichQ[rowN] <- strsplit(as.character(subj_data$delayed_trigger[rowN]), '')[[1]][3]
  }
  
  # get small and large magnitude sets separated 
  d_l <- subset(subj_data, delayed_magnitude == "1000")
  d_s <- subset(subj_data, delayed_magnitude == "100")
  
  
  # get mean RTs by delay and by delayed/immediate response choice 
  summ_dl_gr <- d_l %>% group_by(delay, response) %>% summarise(meanRT = mean(response_time))
  summ_dl_gr <- as.data.frame(summ_dl_gr)
  summ_dl_gr <- summ_dl_gr %>% filter(response != "None")
  
  summ_dl_gr <- summ_dl_gr %>% mutate(choice = case_when(response == '/' ~ "delayed", 
                                                         response == 'z' ~ "immediate"))
  
  summ_dl_gr <- summ_dl_gr[,c(1,4, 3)]
  colnames(summ_dl_gr)[3] <- "meanRT"
  summ_dl_gr$magnitude <- "large"
  
  
  # same for small magnitude 
  
  summ_ds_gr <- d_s %>% group_by(delay, response) %>% summarise(meanRT = mean(response_time))
  summ_ds_gr <- as.data.frame(summ_ds_gr)
  summ_ds_gr <- summ_ds_gr %>% filter(response != "None")
  
  summ_ds_gr <- summ_ds_gr %>% mutate(choice = case_when(response == '/' ~ "delayed", 
                                                         response == 'z' ~ "immediate"))
  
  summ_ds_gr <- summ_ds_gr[,c(1,4, 3)]
  colnames(summ_ds_gr)[3] <- "meanRT"
  summ_ds_gr$magnitude <- "small"
  
  
  # join together
  RTmeans <- full_join(summ_dl_gr, summ_ds_gr)
  
  # set arbitrary for making wide 
  RTmeans$subj <- 0

  # make wide 
  RTmeans_wide <- as.data.frame(acast(RTmeans, subj ~ choice + delay + magnitude, value.var = "meanRT"))

  RTmeans_blank <- as.data.frame(matrix(data = 0, nrow = 1, 
                         ncol = 24, dimnames = list(NULL, 
                                                    c("delayed_1_large", "delayed_1_small", 
                                                      "delayed_2_large", "delayed_2_small",
                                                      "delayed_3_large", "delayed_3_small", 
                                                      "delayed_4_large", "delayed_4_small", 
                                                      "delayed_5_large", "delayed_5_small", 
                                                      "delayed_6_large", "delayed_6_small", 
                                                      "immediate_1_large", "immediate_1_small", 
                                                      "immediate_2_large", "immediate_2_small",
                                                      "immediate_3_large", "immediate_3_small", 
                                                      "immediate_4_large", "immediate_4_small", 
                                                      "immediate_5_large", "immediate_5_small", 
                                                      "immediate_6_large", "immediate_6_small"))))
  
  RTmeans_filled <- full_join(RTmeans_blank, RTmeans_wide)
  RTmeans_final <- RTmeans_filled[2,1:24]
  # set to dataframe 
  all_ddRT_analys[iSubj, 5:28] <- RTmeans_final
}

# write to CSV 
# CHANGE CONDITION LETTER HERE 
write.csv(all_ddRT_analys, "DD_RT_analysis_C.csv")