 
source("api/dataRequest.R")
source("api/dataMerge.R")
source("api/getRedcap.R")
library(data.table)
main_dir = "/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/CAPR Ebbinghaus and Mooney/"
source('~/Desktop/R_functions/funcs.R') # big group of functions
load(file=paste0(main_dir,"RData/cleaned.RData")) #read in the data

# what is medication status of folks:
dataRequest('medlog','scid')
medlog_scid_merge<-dataMerge(medlog_clean,scid_clean, by = c('src_subject_id','visit'))
medlog_scid_merge$src_subject_id<- as.numeric(medlog_scid_merge$src_subject_id)
save(medlog_scid_merge, file = paste0(main_dir,'RData/medlog_scid_merge.RData'))




