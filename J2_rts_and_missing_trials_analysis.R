main_dir = "/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/CAPR Ebbinghaus and Mooney/"
load(file=paste0(main_dir,'RData/rt_merged.RData')) 
rt_merged$mooney_rt_faces_all_conditions <- 
  (rt_merged$catch_mooney_mean_rt_faces +
  rt_merged$upright_mooney_mean_rt_faces +
  rt_merged$inverted_mooney_mean_rt_faces) /3

rt_merged$mooney_response_too_slow_all_conditions <- 
  (rt_merged$catch_mooney_response_too_slow +
     rt_merged$upright_mooney_response_too_slow +
     rt_merged$inverted_mooney_response_too_slow)

#test for group differences in response times 
#ebbinghaus
pub_ready_stats(anova(lm(ebbinghaus_mean_rt ~ phenotype_final, data = rt_merged)))
plot_groups(rt_merged, 'phenotype_final', 'ebbinghaus_mean_rt' )

#mooney
pub_ready_stats(anova(lm(mooney_rt_faces_all_conditions ~ phenotype_final,
                         data = rt_merged)))
plot_groups(rt_merged, 'phenotype_final', 'mooney_rt_faces_all_conditions' )

#test for group differences in timed-out non-responses
#ebbinghaus
pub_ready_stats(anova(lm(ebbinghaus_response_too_slow ~ phenotype_final,
                         data = rt_merged)))
plot_groups(rt_merged, 'phenotype_final', 'ebbinghaus_response_too_slow' )

#mooney
pub_ready_stats(anova(lm(mooney_response_too_slow_all_conditions ~ phenotype_final,
                         data = rt_merged)))
plot_groups(rt_merged, 'phenotype_final', 'mooney_response_too_slow_all_conditions' )

vjp_hist(rt_merged$ebbinghaus_response_too_slow)
#do results cognition with ebbinghaus results hold when controlling for response
#too slows?

pub_ready_stats(lm(bacs_total ~ ebbinghaus_response_too_slow + 
                        context_sensitivity_all_trials.x,
           data = rt_merged))

pub_ready_stats(lm(hvlt_total_score ~ ebbinghaus_response_too_slow + 
             context_sensitivity_all_trials.x,
           data = rt_merged))

summary(lm(wrat_standardscore~ ebbinghaus_response_too_slow + 
             context_sensitivity_all_trials.x,
           data = rt_merged))


