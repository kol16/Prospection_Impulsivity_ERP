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


# set parameters and functions for writing overall csv 


# set vector of delay values in number of days 
days_delay_list <- c(14, 30, 90, 1080, 1800, 3600)

# set discounting function for optimization
sumsquares_hyperbolic <- function(subjective_value_list, magnitude, k, delay_list){
  predicted_subjective_value_list <- magnitude/(1+k*delay_list)
  sumsquares <- sum((subjective_value_list - predicted_subjective_value_list)^2)
  return(sumsquares)
}

# function to get final immediate values to use as indifference points 
get_indifference_pt <- function(delay_data_set){
  final_row <- subset(delay_data_set, counter == 7)
  indifference_pt <- final_row$immediate_value
  return(indifference_pt)
}

# analyze all subject data together: 

# first initialize matrix 
all_dd_analys <- matrix(data = NA, nrow = length(subj_list), 
                        ncol = 8, dimnames = list(NULL, 
                                                  c("subject_num", "session_num",
                                                    "task_type", "condition", 
                                                    "k_smallM", "k_largeM", 
                                                    "AUC_smallM", "AUC_largeM")), 
                        byrow = FALSE)

# iterate over all subjects to get statistics 
  # including discounting coefficient for small and large magnitudes 
  # and AUC for small and large magnitudes 
for(iSubj in 1:length(subj_list)){
  # set current data
  subj_data <- as.data.frame(eval(subj_list[[iSubj]]))
  # get the name as a string
  subject_string <- as.character(subj_list_strings[iSubj])
  # get the basic info: subject number, session number, which task, condition
  cols_subject_info <- as.matrix(colsplit(string=subject_string, pattern="_", names=c("subject_num", "session_num", "task_type", "condition")))
  # put in matrix 
  all_dd_analys[iSubj, 1:4] <- cols_subject_info
  # get small and large magnitude sets separated 
  large_data <- subset(subj_data, delayed_magnitude == 1000)
  small_data <- subset(subj_data, delayed_magnitude == 100)
  
  # get individual delays separated 
  l_d1 <- subset(large_data, delay == 1)
  l_d2 <- subset(large_data, delay == 2)
  l_d3 <- subset(large_data, delay == 3)
  l_d4 <- subset(large_data, delay == 4)
  l_d5 <- subset(large_data, delay == 5)
  l_d6 <- subset(large_data, delay == 6)
  
  s_d1 <- subset(small_data, delay == 1)
  s_d2 <- subset(small_data, delay == 2)
  s_d3 <- subset(small_data, delay == 3)
  s_d4 <- subset(small_data, delay == 4)
  s_d5 <- subset(small_data, delay == 5)
  s_d6 <- subset(small_data, delay == 6)
  
  # store delay sets as lists for use in loops, by magnitude set 
  large_Dlist <- list(l_d1, l_d2, l_d3, l_d4, l_d5, l_d6)
  small_Dlist <-  list(s_d1, s_d2, s_d3, s_d4, s_d5, s_d6)
  
  
  # initialize vectors for storing indifference points 
  indifference_point_list_small <- NULL 
  indifference_point_list_large <- NULL 
  
  # get two vectors for all indifference points of large and small magnitude sets, respectively 
  for(iDelayset in 1:6){
    # which delay set
    current_set_L <- data.frame(large_Dlist[iDelayset])
    # get the indifference point 
    current_indifference_pt_L <- get_indifference_pt(current_set_L)
    # save the indifference point to the list 
    indifference_point_list_large[iDelayset] <- current_indifference_pt_L
    # .. same for small magnitudes 
    current_set_S <- data.frame(small_Dlist[iDelayset])
    current_indifference_pt_S <- get_indifference_pt(current_set_S)
    indifference_point_list_small[iDelayset] <- current_indifference_pt_S
  }
  
  # obtain k values 
  
  # small set 
  optimize_for_k_smallM <- optimize(sumsquares_hyperbolic, c(-1, 1), 
                                    subjective_value_list = indifference_point_list_small, 
                                    magnitude = 100, 
                                    delay_list = days_delay_list, 
                                    tol = .000001)
  k_smallM <- optimize_for_k_smallM$minimum
  all_dd_analys[iSubj, "k_smallM"] <- k_smallM
  
  
  
  #large set 
  optimize_for_k_largeM <- optimize(sumsquares_hyperbolic, c(-1, 1), 
                                    subjective_value_list = indifference_point_list_large, 
                                    magnitude = 1000, 
                                    delay_list = days_delay_list, 
                                    tol = .000001)
  k_largeM <- optimize_for_k_largeM$minimum
  all_dd_analys[iSubj, "k_largeM"] <- k_largeM
  
  ## get area under the curve 
  
  # first get proportional delays and proportional subjective values, using an initial value set of delay=0,value=1
  proportional_delays_set <- c(0, days_delay_list/max(days_delay_list))
  proportional_delay_calc_1 <- proportional_delays_set[2]-proportional_delays_set[1]
  proportional_delay_calc_2 <- proportional_delays_set[3]-proportional_delays_set[2]
  proportional_delay_calc_3 <- proportional_delays_set[4]-proportional_delays_set[3]
  proportional_delay_calc_4 <- proportional_delays_set[5]-proportional_delays_set[4]
  proportional_delay_calc_5 <- proportional_delays_set[6]-proportional_delays_set[5]
  proportional_delay_calc_6 <- proportional_delays_set[7]-proportional_delays_set[6]
  
  proportional_indifference_point_list_small <- c(1, indifference_point_list_small/100)
  proportional_indifference_point_list_large <- c(1, indifference_point_list_large/1000)
  
  # calculate the AUC using methods from Myerson et al., 2001 (between-point trapezoidal areas)
  AUC_smallM <-  as.numeric(proportional_delay_calc_1*(proportional_indifference_point_list_small[2]+proportional_indifference_point_list_small[1])/2 
                            + proportional_delay_calc_2*(proportional_indifference_point_list_small[3]+proportional_indifference_point_list_small[2])/2 
                            + proportional_delay_calc_3*(proportional_indifference_point_list_small[4]+proportional_indifference_point_list_small[3])/2 
                            + proportional_delay_calc_4*(proportional_indifference_point_list_small[5]+proportional_indifference_point_list_small[4])/2 
                            + proportional_delay_calc_5*(proportional_indifference_point_list_small[6]+proportional_indifference_point_list_small[5])/2
                            + proportional_delay_calc_6*(proportional_indifference_point_list_small[7]+proportional_indifference_point_list_small[6])/2)
  
  AUC_largeM <- as.numeric(proportional_delay_calc_1*(proportional_indifference_point_list_large[2]+proportional_indifference_point_list_large[1])/2 
                           + proportional_delay_calc_2*(proportional_indifference_point_list_large[3]+proportional_indifference_point_list_large[2])/2 
                           + proportional_delay_calc_3*(proportional_indifference_point_list_large[4]+proportional_indifference_point_list_large[3])/2 
                           + proportional_delay_calc_4*(proportional_indifference_point_list_large[5]+proportional_indifference_point_list_large[4])/2 
                           + proportional_delay_calc_5*(proportional_indifference_point_list_large[6]+proportional_indifference_point_list_large[5])/2
                           + proportional_delay_calc_6*(proportional_indifference_point_list_large[7]+proportional_indifference_point_list_large[6])/2)
  
  all_dd_analys[iSubj, "AUC_smallM"] <- AUC_smallM
  all_dd_analys[iSubj, "AUC_largeM"] <- AUC_largeM
  
}

# get matrix into data frame 
all_dd_processed <- as.data.frame(all_dd_analys)

# reset variables to numeric 
all_dd_processed$k_smallM <- as.numeric(as.character(all_dd_processed$k_smallM))
all_dd_processed$k_largeM <- as.numeric(as.character(all_dd_processed$k_largeM))
all_dd_processed$AUC_smallM <- as.numeric(as.character(all_dd_processed$AUC_smallM))
all_dd_processed$AUC_largeM <- as.numeric(as.character(all_dd_processed$AUC_largeM))

# save as CSV 
#write.csv(all_dd_processed, "DD_behavioral_analysis_C.csv")


