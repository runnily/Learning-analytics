numberOfTypes = function(df, type){
  #' Count the number of distinct of a specified
  #' type withing a data frame
  #'
  #' @description from a given data frame and specified
  #'              type it calculates the number of distinct
  #'              types within the data frame
  #' @param df  data frame name
  #' @param type The type to count 
  #' @return A tibble illustrating the number of distinct types
  df = df %>% filter({{type}} != c("Unknown"))
  return ((df %>% group_by({{type}})) %>% tally())
}

typeOfUser = function(df, type) {
  #' This is used to find the type of user for a particular
  #' Data frame
  #'
  #' @description This function will get the type of user
  #'              from a specific data frame
  #' @param df  data frame name
  #' @param type The type to count 
  #' @return A tibble illustrating the number of distinct types
  total_en = translateCodeToCnt(distinct(rbind(cyber.security.1_enrolments, cyber.security.2_enrolments, 
                                                        cyber.security.3_enrolments, cyber.security.4_enrolments, 
                                                        cyber.security.5_enrolments, cyber.security.6_enrolments,
                                                        cyber.security.7_enrolments), learner_id, .keep_all = TRUE))
  map = data.frame(row.names = total_en$learner_id, val = total_en[[type]])
  df$type = map[df$learner_id,]
  df = df %>% filter(type != c("Unknown"))
  return(df)
}

performanceVsDf  = function(df) {  
  #' This is used to calculate the performance
  #' of each individual pupil.
  #' 
  #' type withing a data frame
  #'
  #' @description This is used to calculate the performance
  #'              of each individual pupil  from a given
  #'              data frame. Data frame must have a 
  #'              "learner_id" column
  #'              
  #' @param df  data frame consisting of pupils and learner id column
  #' @return A data frame with a mapped performance.
  performance = measurePerformance(total_quizes)
  performance = data.frame(row.names = performance$learner_id, vals = performance$mean)
  df$performance = performance[df$learner_id,]
  return(df)
}

translateCodeToCnt = function(srows) {
  #' This is translates the ISO country code
  #' into a region for an individual/'s
  #' 
  #'
  #' @description Given a data frame would select the
  #'              column "detected_country" and translate
  #'              this into a continent code, produced on the
  #'              column "region code".
  #'              
  #' @param df  data frame consisting of country codes
  #' @return A data frame with a new column specfiying the contitent
  #'         code
  map = data.frame(row.names = code$alpha.2, val = code$region)
  srows$region = map[srows$detected_country,]
  return(srows)
}

measureEngagement = function(df, by = 1) {
  #' This measures the student engagement either by
  #' number of activities or number of engagement.
  #'
  #' @description Given a data frame and a specified 
  #'              by it would provide the number of
  #'              weekly/activities by a specific demographic
  #'              type. Must have a type column to specify demographic
  #'              (gender, age etc). 
  #'              
  #' @param df  data frame consisting of pupils of their demographic
  #' @param by 1: denotes measure student engagement by quizzes
  #'           2: denotes measure student engagement by activities
  #' @return A data frame specifying the number of weekly activities/quizzes
  #'         by demographic type
  if (by == 1) {
    return(df %>% group_by(type) %>% summarise(mean = mean(mean_num_quizes_weekly)))
  } else {
    return(df %>% group_by(type) %>% summarise(mean = mean(mean_num_activties_weekly)))
  }
}

measurePerformance = function(df) {
  #' This will measure the performance of each individual/'s in 
  #' a df.
  #'
  #' @description Given a data frame consisting of a learner id and
  #'              "correct" column to specifying question correct 
  #'              would calculate the average (mean) performance of
  #'              each individual within the data frame
  #'              
  #' @param df  data frame consisting of pupils.
  #' @return A data frame consiting of pupils and the average performance.
  return(df %>% group_by(learner_id) %>% summarize(mean = sum(correct =="true")/length(correct)))
}

caculateAveragePerformance = function(df, type) {
  #' This will calculate the average performance by demographic
  #' group
  #'
  #' @description Given a data frame and a type to specify the demographic
  #'              it would calculate the average group performance for 
  #'              different types of demographic within the group. The data frame
  #'              much have a "mean" column which specifics the average performance
  #'              for every individual before hand.
  #'              
  #' @param df  data frame consisting of a type column specifying demographic group.
  #' @return A data frame of the group and the average performance of the group
  return(df %>% group_by(type) %>% summarise(mean = mean(mean)))
}

numberOfActivtiesCompletedWeekly = function(df) {
  #' This will calculate the number of activities performed
  #' for each week by individual/'s
  #'
  #' @description Given a data frame specifying the step activities for an 
  #'              individual/'s calculate the number average of activities 
  #'              taken by an individual/'s for each week
  #'              
  #' @param df  data frame consisting of pupils step activities.
  #' @return A data frame with learner id and week number and 
  #'        average number of activities for an individual each week.
  df = df %>% filter(last_completed_at != "")
  return(df %>% group_by(learner_id, week_number) %>% summarize(num_of_activties_completed = length(last_completed_at), .groups = "drop"))
}

avgNumberOfActivtiesCompletedWeekly= function(df) {
  #' This will calculate the mean weekly activities performed
  #'by individual/'s
  #'
  #' @description Given a data frame specifying the step activities for an 
  #'              individual/'s calculate the number average of activities 
  #'              taken by an individual/'s weekly.
  #'              
  #' @param  A data frame with learner id and week number and 
  #'        average number of activities for an individual each week.
  #' @return A data frame of learner id mean number of weekly activities.
  df = numberOfActivtiesCompletedWeekly(df)
  df = df %>% group_by(learner_id) %>% summarise(mean_num_activties_weekly = mean(num_of_activties_completed ))
  return(df)
}

numberOfQuizCompletedWeekly = function(df) {
  #' This will calculate the number of quizzes performed
  #' for each week by individual/'s
  #'
  #' @description Given a data frame specifying the step quizzes for an 
  #'              individual/'s calculate the number average of quizzes 
  #'              taken by an individual/'s for each week
  #'              
  #' @param df  data frame consisting of pupils step quizzes.
  #' @return A data frame with learner id and week number and 
  #'        average number of quizzes for an individual each week.
  return(df  %>% group_by(learner_id, week_number) %>% summarize(num_quizes_done = length(quiz_question), .groups = "drop"))
}

avgNumberOfQuizCompletedWeekly = function(df) {
  #' This will calculate the mean weekly quizzes performed
  #'by individual/'s
  #'
  #' @description Given a data frame specifying the step quizzes for an 
  #'              individual/'s calculate the number average of quizzes 
  #'              taken by an individual/'s weekly.
  #'              
  #' @param  A data frame with learner id and week number and 
  #'        average number of quizzes for an individual each week.
  #' @return A data frame of learner id mean number of weekly quizzes.
  df = numberOfQuizCompletedWeekly(df)
  df = df %>% group_by(learner_id) %>% summarise(mean_num_quizes_weekly = mean(num_quizes_done))
  return(df)
}

asDoubleFactor = function(x) {
  #' Given an vector would convert the levels in numerical 
  #'
  #' @param x The vector consisting of levels
  #' @return A data frame of learner id mean number of weekly quizzes.
  as.numeric(as.factor(x))
  }

