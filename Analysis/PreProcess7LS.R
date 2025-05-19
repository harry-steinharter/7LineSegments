library(magrittr)
library(dplyr)

MC <- function(L_max, L_min = 112.6482, nDigits=3){
  # Function to find michelson contrast using L_min as the background luminance in PsychoPy units
  michelson_contrast <- (L_max-L_min) / (L_max+L_min)
  michelson_contrast %<>% round(nDigits)
  return(michelson_contrast)
}

toCandela <- function(x, nDigits = 10){
  CandelaPerMeterSquared <- (x * 112.417) + 112.6482
  CandelaPerMeterSquared %<>% round(nDigits)
  return(CandelaPerMeterSquared)
}

categorize <- function(df){
  df %<>%mutate(
    WidthGroup = case_when(
      CondShort %in% c("Constant", "Single") ~ "controls",
      CondShort %in% c('IW','DW') ~ "wide",
      CondShort %in% c('IN','DN') ~ "narrow"
    ),
    ExpectedGroup = case_when(
      CondShort %in% c('Constant','IN','IW') ~ 'high',
      CondShort %in% c('Single','DN','DW') ~ 'low'
    )
  )
  return(df)
}

setwd('/Users/harrysteinharter/Documents/MSc/Timo Internship/7LineSegments/Analysis')
# List all CSV files in the directory
file_list <- list.files(path = "../Outputs/", pattern = "\\.csv$", full.names = TRUE)
# Read and bind all CSV files into one data frame
all_data <- do.call(rbind, lapply(file_list, read.csv))
df <- all_data %>%  subset(Reaction_Time < 99 & Correct_Response != 'None')
df$Correct_Response %<>% as.numeric()


#### Perform transformations ####
if (TRUE){
  df$TC_c <- scale(df$Target_Contrast,scale=F) %>% c()
  df$TC_michelson <- toCandela(df$Target_Contrast) %>% MC()
  df$TC_michelson_c <- scale(df$TC_michelson,scale=F) %>% c()
  
  df$FCfar_c <- scale(df$FCfar,scale=F) %>% c()
  df$FCfar_michelson <- toCandela(df$FCfar) %>% MC()
  df$FCfar_michelson_c <- scale(df$FCfar_michelson,scale=F) %>% c()
  
  df$FCmed_c <- scale(df$FCmed,scale=F) %>% c()
  df$FCmed_michelson <- toCandela(df$FCmed) %>% MC()
  df$FCmed_michelson_c <- scale(df$FCmed_michelson,scale=F) %>% c()
  
  df$FCclose_c <- scale(df$FCclose,scale=F) %>% c()
  df$FCclose_michelson <- toCandela(df$FCclose) %>% MC()
  df$FCclose_michelson_c <- scale(df$FCclose_michelson,scale=F) %>% c()
  
  df %<>% mutate(TC_factor = ggplot2::cut_number(Target_Contrast, 3, labels = c("Low", "Medium", "High")))
}
#### Seperate ####
# Seperate into Real and Null Files
dfNull <- subset(df, endsWith(Condition,"_null"))
#dfNull$CondShort %<>% droplevels()
dfReal <- subset(df, !endsWith(Condition,"_null"))
dfReal$CondShort <- car::recode(dfReal$Condition, recodes = 
                         "'FarHighNarrow' = 'IN'; 'FarLowNarrow' = 'DN';
                        'FarHighWide' = 'IW'; 'FarLowWide' = 'DW';
                        'ThreeLinesControl' = 'Single'; 'Constant' = 'Constant'") %>% as.factor()
dfReal$CondShort %<>% droplevels()
dfReal %<>% categorize()

#### Mutate real data ####
grandMean_real <- dfReal %>% 
  group_by(Participant_Number,CondShort) %>% 
  mutate(overall_mean = mean(TC_michelson), weights = n()) %>% 
  select(Participant_Number,CondShort,overall_mean,WidthGroup,ExpectedGroup) %>% 
  ungroup() %>% 
  distinct()

meanByTC <- dfReal %>% 
  group_by(Participant_Number,CondShort,TC_factor) %>% 
  mutate(mean = mean(Correct_Response), weights = n()) %>% 
  select(Participant_Number,TC_factor,CondShort,mean,WidthGroup,ExpectedGroup) %>% 
  ungroup() %>% 
  distinct()

rawIndividual <- dfReal %>% 
  group_by(Participant_Number,CondShort,Target_Contrast) %>% 
  mutate(performance = mean(Correct_Response), weights = n()) %>% 
  ungroup() %>% 
  distinct()

# Save the raw data
write.csv(rawIndividual,'glmDataRaw7LS.csv', row.names=FALSE)
write.csv(meanByTC,'subjectMeansByTCFactorRaw7LS.csv',row.names = FALSE)
write.csv(grandMean_real,'subjectMeansOverallRaw7LS.csv',row.names = FALSE)

