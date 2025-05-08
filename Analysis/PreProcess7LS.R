library(magrittr)
library(dplyr)
setwd('/Users/harrysteinharter/Documents/MSc/Timo Internship/7LineSegments/Analysis')
# List all CSV files in the directory
file_list <- list.files(path = "../Outputs/", pattern = "\\.csv$", full.names = TRUE)
# Read and bind all CSV files into one data frame
all_data <- do.call(rbind, lapply(file_list, read.csv))
# Seperate into Real and Null Files
dfNull <- subset(all_data, endsWith(Condition,"_null"))
dfReal <- subset(all_data, !endsWith(Condition,"_null"))

# Save the raw data
write.csv(dfReal,'rawData7LS.csv', row.names=FALSE)

# Calculate false-positive rate
FPs <- c()

dfNull$Condition %<>% as.factor
dfReal$Condition %<>% as.factor
condsNull <- levels(dfNull$Condition)
condsReal <- levels(dfReal$Condition)

people <- dfReal$Participant_Number %>% unique

df_summary <- dfNull %>% 
  group_by(Participant_Number, Condition) %>% 
  summarise(false_positive_rate = mean(Correct_Response, na.rm = TRUE)) %>% 
  ungroup()
