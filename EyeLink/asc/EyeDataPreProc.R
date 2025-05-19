library(dplyr)
library(magrittr)
library(eyelinker)
library(purrr)
library(ivs)

fileList <- list.files()
fileList  <- fileList[which(endsWith(fileList, '.asc'))]

for (File in fileList){
  subNumber <- strsplit(File,'.',fixed = TRUE)[[1]][1] %>% as.integer()
  newFileName <- paste0('summaryData',as.character(subNumber),'.csv')
  cat("Working on File:",as.character(subNumber))
  
  ## Load
  asc <- read.asc(File)
  str(asc, max.level = 1)
  msg <- asc$msg
  button <- asc$button
  sacc <- asc$sacc
  
  ## Transform `button`
  button %<>% group_by( # Keep only 'easy' trials with the expected number of buttons
    block) %>% mutate(nPer=n()) %>% ungroup() %>% subset(nPer == 4)
  button$stim <- c('fix','fix','target','target') %>% rep(nrow(button)/4) %>% c() # What is presented
  button %<>% subset(stim == 'target') # Keep only target
  button_w <- button %>% 
    group_by(block) %>% 
    arrange(time, .by_group = TRUE) %>% 
    mutate(stimS = min(time), stimE = max(time)) %>% 
    ungroup() %>% 
    select(c('block','button','stim','stimS','stimE')) %>% 
    distinct()
  
  ## Set internal vectors
  sacc_IVs <- iv(sacc$stime,sacc$etime)
  button_IVs <- iv(button_w$stimS,button_w$stimE)
  locations <- iv_locate_overlaps(sacc_IVs,button_IVs)
  IV_indexing <- locations[!is.na(locations$haystack),]
  
  # Index the button_w df based on the index values of IV_indexing$haystack
  button_w_saccade <- button_w[IV_indexing$haystack,]
  saccade_w_button <- sacc[IV_indexing$needles,]
  
  ## Subsetting
  blocks <- saccade_w_button$block
  msg_w_saccades <- msg[which(msg$block %in% blocks),]
  msg_w_saccades <- msg_w_saccades[which(grepl("TRIAL_START",msg_w_saccades$text)),]
  
  split_cols <- do.call(rbind, strsplit(msg_w_saccades$text, " | ", fixed = TRUE))
  msg_w_saccades$trialN    <- split_cols[, 1]
  msg_w_saccades$Condition <- split_cols[, 2]
  msg_w_saccades$TC        <- split_cols[, 3]
  msg_w_saccades$trialN %<>% gsub("TRIAL_START ", '', .)
  
  msg_w_saccades <- subset(msg_w_saccades, !endsWith(Condition,"_null"))
  msg_w_saccades$CondShort <- car::recode(msg_w_saccades$Condition, recodes = 
                                            "'FarHighNarrow' = 'IN'; 'FarLowNarrow' = 'DN';
                        'FarHighWide' = 'IW'; 'FarLowWide' = 'DW';
                        'ThreeLinesControl' = 'Single'; 'Constant' = 'Constant'") %>% as.factor()
  
  ## Re-subsetting
  # Step 1: Find common block values
  common_blocks <- reduce(
    list(
      button_w_saccade$block,
      saccade_w_button$block,
      msg_w_saccades$block
    ),
    intersect
  )
  
  # Step 2: Filter all data frames
  button_w_saccade_filt <- button_w_saccade %>% filter(block %in% common_blocks)
  saccade_w_button_filt <- saccade_w_button %>% filter(block %in% common_blocks)
  msg_w_saccades_filt   <- msg_w_saccades   %>% filter(block %in% common_blocks)
  
  ## Saccade summary stats
  saccadeFinal <- saccade_w_button_filt %>% group_by(block) %>% 
    mutate(nSaccades = n(), 
           meamAmplitude = mean(ampl), 
           meanPeakVelocity = mean(pv), 
           meanDuration = mean(dur)) %>% 
    select(nSaccades,meamAmplitude,meanPeakVelocity,meanDuration,block) %>% 
    ungroup() %>% 
    distinct()
  
  ## Grand Means by Condition
  all_data <- cbind(saccadeFinal,msg_w_saccades[c(4,5,6,7)])
  dataByCondition <- all_data %>% group_by(CondShort) %>% 
    mutate(nTrialsWithSaccade = n(),
           grandMeanAmp = mean(meamAmplitude, na.rm = TRUE),
           grandMeanVel = mean(meanPeakVelocity, na.rm = TRUE),
           grandMeanDurt = mean(meanDuration, na.rm = TRUE),
           meanSaccades = mean(nSaccades, na.rm = TRUE)) %>% 
    select(nTrialsWithSaccade,meanSaccades,grandMeanVel,grandMeanDurt,grandMeanAmp,CondShort) %>% 
    ungroup() %>% 
    distinct()
  dataByCondition %<>% cbind(subNumber = subNumber)
  write.csv(dataByCondition, newFileName)
}