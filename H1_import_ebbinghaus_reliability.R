main_dir = "/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/CAPR Ebbinghaus and Mooney/"
source("api/dataRequest.R")
source("api/getTask.R")
source("api/getTask.R")
source('~/Desktop/R_functions/funcs.R') # big group of functions
dataRequest('ebbinghaus')

ebb_reliability<- ebbinghaus_clean[ebbinghaus_clean$visit == 'bl',]
ebb_reliability$src_subject_id <- as.integer(ebb_reliability$src_subject_id)
#merge with main_df
load(file=paste0(main_dir,"RData/cleaned.RData")) 

str(ebb_reliability$src_subject_id)
str(main_df$src_subject_id)
ebb_reliability_merged <- base::merge(main_df, ebb_reliability, by = 'src_subject_id',
                                      all.x = TRUE)

save(ebb_reliability_merged, file = paste0(main_dir, 
                                           'RData/ebb_reliability_merged.RData'))

