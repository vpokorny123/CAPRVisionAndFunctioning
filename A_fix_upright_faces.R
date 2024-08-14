# there are bunch of zeros that shouldn't be there for the upright faces responses 
# due to a coding error in the original cleaning script
#here we will merge the main data frame with the correct upright faces values

library(data.table)
library(dplyr)

#so first get main_df and then drop bad upright faces
main_dir = "/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/CAPR Ebbinghaus and Mooney/"
main_df_1<-fread(paste0(main_dir,"csvs/victor_visual_project_7.19.24.csv"))
main_df_1<- main_df_1 %>% dplyr::select(-upright_faces_reported)
#then get correct mooney_face values
#annoyingly this df is also named main_df
load(paste0(main_dir,'RData/requested_data.RData'))
upright_faces <- main_df %>% dplyr::select(src_subject_id,
                                           upright_faces_reported,
                                           visit)

#now merge by subject id
main_df<-base::merge(main_df_1, upright_faces, by = c('src_subject_id','visit'))
save(main_df,file = paste0(main_dir,'RData/fixed_upright_faces_data.RData'))