source("api/dataRequest.R")
source("api/dataMerge.R")
source("api/getRedcap.R")
library(data.table)
main_dir = "/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/CAPR Ebbinghaus and Mooney/"
source('~/Desktop/R_functions/funcs.R') # big group of functions
load(file=paste0(main_dir,"RData/cleaned.RData")) #read in the data
load(paste0(main_dir,'RData/medlog_scid_merge.RData'))

#merge new data with main df
merged_df<- base::merge(main_df, medlog_scid_merge, 
                        by = c('src_subject_id','visit'))

group_names  <- rev(c('CHR','PLE','CLN','HC'))
meds            <- c('antidepressant_current', 'antipsychotic_current', 
                     'moodstabilizer_current', 'stimulant_current', 'benzodiazepine_current',
                      'nonbenzo_anx_current')
meds_nice_names <- c('  Antidepressants', '  Antipsychotics', 
                     '  Mood Stabilizers',
                     '  Stimulants', '  Benzodiazepines',
                     '  Nonbenzodiazepine Anxiolytics ')
group_ns = table(merged_df$phenotype_final)
#for table add first row just for label
med_percentages = c('Current Medications','','','','')
for (j in seq(meds)){
  med <- meds[j]
  med_nice_name<-meds_nice_names[j] 
  freqs<-table(merged_df[[med]],merged_df$phenotype_final)
  norm_freqs<-round(sweep(freqs, 2, colSums(freqs), `/`) [2,]*100,2)
  percentages<- unname(cbind(med_nice_name,t(paste0(norm_freqs,'%'))))
  med_percentages <- unname(rbind(med_percentages, percentages))
}

med_percentages
# let's just pick some very common mental health disorders and report those


scids <- c('scid_mdd_curr.y', 'scid_aud_curr.y','scid_panic_curr.y',
                          'scid_socanx_curr.x','scid_gad_curr.y','scid_ocd_curr.y',
                          'scid_anorex_curr.y', 'scid_adhd.y','scid_ptsd_curr.y',
                          'scid_schiz_curr.y')
scids_nice_names <- c('  Major Depressive Disorder','  Alcohol Use Disorder',
                      '  Panic Disorder','  Social Anxiety',
                      '  Generalized Anxiety Disorder',
                     '  Obsessive-Compulsive Disorder', 
                     '  Anorexia Nervosa',
                     '  Attention-Deficit/Hyperactivity Disorder',
                     '  Post-Traumatic Stress Disorder','  Schizophrenia')
scid_percentages = c('Current Diagnoses','','','','')
for (j in seq(scids)){
  scid <- scids[j]
  scid_nice_name<-scids_nice_names[j] 
  freqs<-table(merged_df[[scid]],merged_df$phenotype_final)
  if (nrow(freqs) == 1){
    freqs[]<-(rep(rownames(freqs),length(unique(merged_df$phenotype_final))))
    norm_freqs<- freqs
  } else {
  norm_freqs<-round(sweep(freqs, 2, colSums(freqs), `/`) [2,]*100,2)
  }
  percentages<- cbind(scid_nice_name,t(paste0(norm_freqs,'%')))
  scid_percentages <- unname(rbind(scid_percentages, percentages))
  colnames(scid_percentages) <- c('', group_names)
  print(j)
  print(problem_hcs)
}

supp_table<- rbind(med_percentages,scid_percentages)
write.csv(supp_table,paste0(main_dir,'Tables/supp_table1.csv'))



