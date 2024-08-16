source("api/dataRequest.R")
dataRequest('ebbinghaus')

library(splithalf)
#compute reliablity
reliability<-cor.test(ebbinghaus_clean$context_sensitivity_evens, 
         ebbinghaus_clean$context_sensitivity_odds)
pub_ready_stats(reliability)


#spearman brown
2*reliability$estimate / (1+ reliability$estimate)
