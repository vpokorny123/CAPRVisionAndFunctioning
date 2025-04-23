main_dir = "/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/CAPR Ebbinghaus and Mooney/"
source("api/dataRequest.R")
source("api/getTask.R")
source("api/dataMerge.R")
source('~/Desktop/R_functions/funcs.R') # big group of functions
dataRequest('ebbinghaus','mooney_vjp')

merged_df<-dataMerge(ebbinghaus_clean, mooney_vjp_clean, 
                     by = c('src_subject_id','visit'))

merged_df<- merged_df[merged_df$visit == 'bl',]
merged_df$src_subject_id <- as.integer(merged_df$src_subject_id)
#merge with main_df
load(file=paste0(main_dir,"RData/cleaned.RData")) 

rt_merged <- base::merge(main_df, merged_df, by = 'src_subject_id',
                                      all.x = TRUE)

save(rt_merged, file = paste0(main_dir, 'RData/rt_merged.RData'))
