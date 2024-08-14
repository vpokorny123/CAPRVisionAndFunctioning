library(data.table)
library(dplyr)
library(tidyr)
library(rstatix)
library(reshape2)

main_dir = "/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/CAPR Ebbinghaus and Mooney/"
load(file=paste0(main_dir,"RData/cleaned.RData")) #read in the data
source(paste0(main_dir,'R_scripts/funcs.R')) # big group of functions

continuous_demo_vars <- c("demo_age_in_years","demo_household_income")
categorical_demo_vars <- c("demo_biological_sex", "demo_race")
table_groups <- c('chr','hsc_sub','hsc_other','hc') 
group_names  <- c('CHR','PLE','CLN','HC')

#if you want to change order of groups
main_df$phenotype_final <- factor(main_df$phenotype_final,
                                              levels = table_groups)
demo_table = NULL

#Age
summary_df <- main_df %>% 
  group_by(phenotype_final) %>%
  get_summary_stats(demo_age_in_years, type = "mean_sd" )
summary_df =paste0(round(summary_df$mean,2) , ' (',round(summary_df$sd,2),')')
res<-aov(demo_age_in_years~ phenotype_final, data =main_df)
paste_stats <- pub_ready_stats(res)
demo_table = cbind("Age",t(summary_df),unname(paste_stats))


#Sex
res<- chisq.test(table(as.factor(main_df$demo_biological_sex), 
                       as.factor(main_df$phenotype_final)))
paste_stats  = pub_ready_stats(res)
blank_row = c('Sex','','','','',unname(paste_stats))
demo_table = rbind(demo_table,blank_row)
summary_df <- main_df %>% 
  group_by(phenotype_final) %>% 
  summarise(`  % Female` = paste0(round(mean(demo_biological_sex, na.rm = TRUE),2)*100,'%'),
            `  % Male` = paste0(round(1-mean(demo_biological_sex, na.rm = TRUE),2)*100,'%')) %>%
  t() %>% .[-1,]
rows<-unname(cbind(rownames(summary_df),summary_df,''))
demo_table = rbind(demo_table,rows)

#Race 
freqs<-table(as.factor(main_df$demo_race), 
      as.factor(main_df$phenotype_final))
res<- chisq.test(freqs)
chisq.posthoc.test::chisq.posthoc.test(freqs)
paste_stats  = pub_ready_stats(res)
blank_row = c('Race','','','','',paste_stats)
original_labels = unique(main_df$demo_race)
relabels <- c('  % African American','  % Caucasian','  % Asian','  % Multiracial',
              '  % Hawaiian','  % American Indian',NA,'  % Not Reported')
main_df$demo_race<-plyr::mapvalues(main_df$demo_race, from = original_labels, to = relabels)
demo_table = rbind(demo_table,blank_row)
summary_df <- main_df %>% 
  group_by(phenotype_final,demo_race) %>%
  summarise(group_ns = n()) %>% 
  group_by(phenotype_final) %>%
  mutate(
         percent_race = round((group_ns/sum(group_ns)) *100,1),
         percentage_race = paste0(percent_race,'%')) %>%
  filter(!is.na(demo_race)) %>%
  dcast(., demo_race ~ phenotype_final,
            value.var = 'percentage_race') %>% as.matrix()
summary_df[is.na(summary_df)] = '0%'
demo_table = rbind(demo_table,cbind(summary_df,rep('',7)))

#Hispanic
main_df$demo_hispanic[main_df$demo_hispanic==4] = 0
summary_df <- main_df %>% 
  group_by(phenotype_final) %>%
  summarise(`% Hispanic` = paste0(round(mean(demo_hispanic, na.rm = TRUE),2)*100,'%')) %>%
  t() %>% .[-1,]
res<- chisq.test(table(as.factor(main_df$demo_hispanic), 
                       as.factor(main_df$phenotype_final)))
paste_stats  = pub_ready_stats(res)
rows<-cbind('% Hispanic',t(summary_df),paste_stats)
demo_table = rbind(demo_table,rows)

#Income
summary_df <- main_df %>% 
  group_by(phenotype_final) %>%
  summarise(median = median(demo_household_income,na.rm = TRUE)) %>% 
  t() %>% .[2,] %>% as.numeric() %>% 
  format(scientific = FALSE, big.mark = ",",trim = TRUE)
summary_df = cbind('Median Household Income',t(summary_df))
res<-kruskal.test(demo_household_income~ phenotype_final, data =main_df)
paste_stats<- pub_ready_stats(res)
demo_table = rbind(demo_table,cbind(summary_df,paste_stats))

#create colnames
demo_df <- as.data.frame(demo_table)
ns <- main_df %>% count(phenotype_final)
colnames(demo_df)[-1] <- cbind(t(paste0(group_names,' (n=',ns$n,')')),"Statistics")

#save out as csv to make nice looking table in excel
write.csv(demo_df, paste0(main_dir,'Tables/table_raw.csv'))
