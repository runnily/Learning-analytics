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