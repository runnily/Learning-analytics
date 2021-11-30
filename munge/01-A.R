# Example preprocessing script.
total_enrollments = translateCodeToCnt(distinct(rbind(cyber.security.1_enrolments, cyber.security.2_enrolments, 
                          cyber.security.3_enrolments, cyber.security.4_enrolments, 
                          cyber.security.5_enrolments, cyber.security.6_enrolments,
                          cyber.security.7_enrolments), learner_id, .keep_all = TRUE))

total_activties = rbind(cyber.security.1_step.activity, cyber.security.2_step.activity,
                        cyber.security.3_step.activity, cyber.security.4_step.activity,
                        cyber.security.5_step.activity, cyber.security.6_step.activity,
                        cyber.security.7_step.activity)

total_quizes = rbind(cyber.security.1_question.response, cyber.security.2_question.response,
                     cyber.security.3_question.response, cyber.security.4_question.response,
                     cyber.security.5_question.response, cyber.security.6_question.response,
                     cyber.security.7_question.response)

total_vids_stats = totalVidStats(list(translateVideoStats(cyber.security.3_video.stats,4), translateVideoStats(cyber.security.4_video.stats,4),
                                      translateVideoStats(cyber.security.5_video.stats,4), translateVideoStats(cyber.security.6_video.stats,4),
                                      translateVideoStats(cyber.security.7_video.stats,4)))

total_enrollements_model_data = total_enrollments[-c(2:6,13,8)]
total_enrollements_model_data = total_enrollements_model_data %>% filter_all(all_vars(.!="Unknown"))
total_enrollements_model_data = na.omit(performanceVsDf(total_enrollements_model_data))
total_enrollements_model_data = total_enrollements_model_data[-1]
performance = total_enrollements_model_data$performance
total_enrollements_model_data = apply(total_enrollements_model_data[-7], 2, asDoubleFactor)
total_enrollements_model_data = data.frame(total_enrollements_model_data, performance)



