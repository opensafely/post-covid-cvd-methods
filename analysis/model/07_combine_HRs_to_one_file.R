library(purrr)
library(data.table)
library(dplyr)

fs::dir_create(here::here("output", "review", "model"))

output_dir <- "output/review/model"


#Read in R HRs
print("Combining HR files")

hr_files=list.files(path = output_dir, pattern = "to_release")
hr_files=hr_files[endsWith(hr_files,".csv")]
hr_files=paste0(output_dir,"/", hr_files)
hr_file_paths <- pmap(list(hr_files),
                      function(fpath){
                        df <- fread(fpath)
                        return(df)
                      })
estimates <- rbindlist(hr_file_paths, fill=TRUE)

estimates$redacted_results <- factor(estimates$redacted_results, levels = c("Redacted results",
                                                                                        "No redacted results"))
estimates <- estimates[order(estimates$redacted_results),]

write.csv(estimates,paste0(output_dir,"/R_HR_output_pre_vax.csv") , row.names=F)

#Read in R event counts
print("Combining event count files")

event_counts=list.files(path = output_dir, pattern = "suppressed_compiled_event_counts")
event_counts=event_counts[endsWith(event_counts,".csv")]
event_counts=paste0(output_dir,"/", event_counts)
event_count_file_paths <- pmap(list(event_counts),
                      function(fpath){
                        df <- fread(fpath)
                        return(df)
                      })
event_counts_df <- rbindlist(event_count_file_paths, fill=TRUE)

event_counts_df$redacted_results <- factor(event_counts_df$redacted_results, levels = c("Redacted results",
                                                                            "No redacted results"))
event_counts_df <- event_counts_df[order(event_counts_df$redacted_results),]

write.csv(event_counts_df,paste0(output_dir,"/R_event_count_output_pre_vax.csv") , row.names=F)

#Get event counts by time period for day zero analyses
event_counts_df$events_total <- as.numeric(event_counts_df$events_total)
event_counts_day_zero <- event_counts_df %>% filter(time_points == "day_zero_reduced"
                                                    & event %in% c("ate_extended_follow_up", "vte_extended_follow_up",
                                                                   "ate_primary_position_extended_follow_up", "vte_primary_position_extended_follow_up"))%>%
  select(event,cohort,subgroup,time_points,expo_week,events_total)


tmp_hosp <- event_counts_day_zero %>% filter(subgroup == "main") %>%
  left_join(event_counts_day_zero %>% filter(subgroup == "covid_pheno_non_hospitalised") %>%
              select(!subgroup)%>%
              rename(events_total_non_hosp = events_total))

tmp_hosp$events_total_hosp <- tmp_hosp$events_total - tmp_hosp$events_total_non_hosp
tmp_hosp$events_total <- NULL
tmp_hosp$events_total_non_hosp <- NULL

tmp_hosp$subgroup <- "covid_pheno_hospitalised"
tmp_hosp <- rename(tmp_hosp, events_total=events_total_hosp)

event_counts_day_zero <- rbind(event_counts_day_zero, tmp_hosp)

write.csv(event_counts_day_zero,paste0(output_dir,"/R_event_count_day_zero_output_pre_vax.csv") , row.names=F)

#Get event counts by time period for first month split analyses
event_counts_df$events_total <- as.numeric(event_counts_df$events_total)
event_counts_month1_split <- event_counts_df %>% filter(time_points == "month1_split_reduced"
                                                        & event %in% c("ate_extended_follow_up", "vte_extended_follow_up",
                                                                       "ate_primary_position_extended_follow_up", "vte_primary_position_extended_follow_up"))%>%
  select(event,cohort,subgroup,time_points,expo_week,events_total)


tmp_hosp <- event_counts_month1_split %>% filter(subgroup == "main") %>%
  left_join(event_counts_month1_split %>% filter(subgroup == "covid_pheno_non_hospitalised") %>%
              select(!subgroup)%>%
              rename(events_total_non_hosp = events_total))

tmp_hosp$events_total_hosp <- tmp_hosp$events_total - tmp_hosp$events_total_non_hosp
tmp_hosp$events_total <- NULL
tmp_hosp$events_total_non_hosp <- NULL

tmp_hosp$subgroup <- "covid_pheno_hospitalised"
tmp_hosp <- rename(tmp_hosp, events_total=events_total_hosp)

event_counts_month1_split <- rbind(event_counts_month1_split, tmp_hosp)

write.csv(event_counts_month1_split,paste0(output_dir,"/R_event_count_month1_split_output.csv") , row.names=F)