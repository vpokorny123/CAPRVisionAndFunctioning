main_dir = "/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/CAPR Ebbinghaus and Mooney/"
#merge with main_df
load(file=paste0(main_dir,'RData/ebb_reliability_merged.RData')) 
source('~/Desktop/R_functions/funcs.R') # big group of functions

#compute reliablity
reliability<-cor.test(ebb_reliability_merged$context_sensitivity_evens, 
                      ebb_reliability_merged$context_sensitivity_odds)
pub_ready_stats(reliability)


#spearman brown
round(2*reliability$estimate / (1+ reliability$estimate),2)
