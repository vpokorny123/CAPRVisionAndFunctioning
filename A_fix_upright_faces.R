# there are bunch of zeros that shouldn't be there for the upright faces responses 
# due to a coding error in the original cleaning script
#here we will merge the main data frame with the correct upright faces values

library(data.table)
library(dplyr)

#so first get main_df and then drop bad upright faces
main_dir = "/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/CAPR Ebbinghaus and Mooney/"
main_df_1<-fread(paste0(main_dir,"csvs/victor_visual_project_7.19.24.csv"))
main_df_1<- main_df_1 %>% dplyr::select(!starts_with('upright'))

#then get correct mooney_face values
load(paste0(main_dir,'RData/mooney_01032025.RData'))
upright_faces <- mooney_vjp_clean %>% 
  dplyr::select(src_subject_id, starts_with('upright'),visit) %>%
  dplyr::mutate(src_subject_id = as.numeric(as.character(src_subject_id)))

#now merge by subject id
main_df<-base::merge(main_df_1, upright_faces, 
                     by = c('src_subject_id','visit'), 
                     all.x = TRUE)

save(main_df,file = paste0(main_dir,'RData/fixed_upright_faces_data.RData'))