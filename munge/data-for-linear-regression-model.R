#' This file is specifically used to define 
#' the data which is going to be used for our
#' predictive model (1). This model predicts the users performance.

# Select rows we need
total_enrollements_model_data = total_enrollments[-c(2:6,13,8)]
total_enrollements_model_data = total_enrollements_model_data %>% 
  filter_all(all_vars(.!="Unknown")) # remove all rows that contain "unknown"

# Remove NA values
total_enrollements_model_data = 
  na.omit(performanceVsDf(total_enrollements_model_data)) 

# Calculates average quizzes performed by an individual
avg_quizes_weekly = avgNumberOfQuizCompletedWeekly(total_quizes)
avg_quizes_weekly = data.frame(row.names =
                                 avg_quizes_weekly$learner_id, vals =
                                 avg_quizes_weekly$mean_num_quizes_weekly)
total_enrollements_model_data$mean_num_quizes_weekly = 
  avg_quizes_weekly[total_enrollements_model_data$learner_id,]

# Calculates average activities performed by an individual
avg_activties_weekly = avgNumberOfActivtiesCompletedWeekly(total_activties)
avg_activties_weekly = data.frame(row.names 
                                  = avg_activties_weekly$learner_id, vals 
                                  = avg_activties_weekly$mean_num_activties_weekly)
total_enrollements_model_data$mean_num_activties_weekly = 
  avg_activties_weekly[total_enrollements_model_data$learner_id,]

# Removing learner id column
total_enrollements_model_data = total_enrollements_model_data[-1]

# Translate columns into numbers
performance = total_enrollements_model_data$performance
avg_quizes_weekly = total_enrollements_model_data$mean_num_quizes_weekly
avg_activties_weekly = total_enrollements_model_data$mean_num_activties_weekly
total_enrollements_model_data = 
  apply(total_enrollements_model_data[-c(7:9)], 2, asDoubleFactor)

# Combine dataframe again
total_enrollements_model_data = data.frame(total_enrollements_model_data, 
                                           avg_quizes_weekly, 
                                           avg_activties_weekly, performance)

# Our finial defined dataframe used for the model
total_enrollements_model_data = na.omit(total_enrollements_model_data)