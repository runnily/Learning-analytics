# Example preprocessing script.
total_enrollments = translateCodeToCnt(distinct(rbind(cyber.security.1_enrolments, cyber.security.2_enrolments, 
                          cyber.security.3_enrolments, cyber.security.4_enrolments, 
                          cyber.security.5_enrolments, cyber.security.6_enrolments,
                          cyber.security.7_enrolments), learner_id, .keep_all = TRUE))
