library(data.table)
library(dplyr)
library(tidyr)
library(rstatix)
library(reshape2)
library(berryFunctions)

main_dir = "/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/CAPR Ebbinghaus and Mooney/"
load(file=paste0(main_dir,"RData/cleaned.RData")) #read in the data
source(paste0(main_dir,'R_scripts/funcs.R')) # big group of functions

#so what demographic variables and groups dp we want
#probably want sex, age, race, and years of education
#need to figure out if we have acuity information
main_vars <- c('BACS Total',"HVLT Total","WRAT Standard",
               "Global Functioning: Role", "Global Functioning: Social", "Social Phobia Scale",
               "Context Sensitivity", "Upright Faces Reported", "Inverted Faces Reported")
table_groups <- c('chr','hsc_sub','hsc_other','hc') 
group_names  <- c('CHR','PLE','CLN','HC')

# change order of groups
main_df$phenotype_final <- factor(main_df$phenotype_final,
                                              levels = table_groups)
main_df <- as.data.frame(main_df)
res_table = c('',group_names,'Statistics')
post_hoc_res = NULL

for (j in seq(main_vars)){
  var = sym(main_vars[j])
  reg_var = main_vars[j]
  summary_df <- main_df %>% 
    group_by(phenotype_final) %>%
    get_summary_stats(!!var, type = "mean_sd" )
  summary_df<-paste0(round(summary_df$mean,2) , ' (',round(summary_df$sd,2),')')
  res<-aov(main_df[,reg_var] ~ phenotype_final, data =main_df)
  summary_res <- summary(res)[[1]]
  row_for_table <- cbind(reg_var,t(summary_df), pub_ready_stats(summary_res))
  ph_res<-pairwise.t.test(main_df[,reg_var],main_df$phenotype_final, p.adjust.method = 'fdr')
  
  post_hoc_res[[reg_var]] <- ph_res$p.value
  res_table <- rbind(res_table, row_for_table)
}
res_table_df <- data.frame(res_table)

#add some blank rows to make copy/pasting easier later
#res_table_df_name<-insertRows(data.frame(row.names(res_table_df)),c(5,9),new = "")
res_table_df <- insertRows(res_table_df, c(2,6,10) , new = "")
res_table_df[c(2,6,10),'reg_var'] <- c("Cognitive","Social/Role","Task")

write.csv(res_table_df, paste0(main_dir,'Tables/table2_raw.csv'))

#print post_hoc_res 
post_hoc_res

