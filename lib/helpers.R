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
  performance = measurePerformance(total_quizes)
  performance = data.frame(row.names = performance$learner_id, vals = performance$mean)
  df$performance = performance[df$learner_id,]
  return(df)
}

translateCodeToCnt = function(srows) {
  #'This takes a specific data frame and translates
  #'them applies a region to it.
  #'
  map = data.frame(row.names = code$alpha.2, val = code$region)
  srows$region = map[srows$detected_country,]
  return(srows)
}

measureEngagement = function(df, by = 1) {
  if (by == 1) {
    return(df %>% group_by(type) %>% summarise(mean = mean(mean_num_quizes_weekly)))
  } else {
    return(df %>% group_by(type) %>% summarise(mean = mean(mean_num_activties_weekly)))
  }
}

measurePerformance = function(df) {
  #' This is going to be used to measure the performance 
  #' of an individual
  return(df %>% group_by(learner_id) %>% summarize(mean = sum(correct =="true")/length(correct)))
}

caculateAveragePerformance = function(df, type) {
  return(df %>% group_by(type) %>% summarise(mean = mean(mean)))
}

numberOfActivtiesCompletedWeekly = function(df) {
  df = df %>% filter(last_completed_at != "")
  return(df %>% group_by(learner_id, week_number) %>% summarize(num_of_activties_completed = length(last_completed_at), .groups = "drop"))
}

avgNumberOfActivtiesCompletedWeekly= function(df) {
  #' Provides the number of activities completed
  #' by a student
  df = numberOfActivtiesCompletedWeekly(df)
  df = df %>% group_by(learner_id) %>% summarise(mean_num_activties_weekly = mean(num_of_activties_completed ))
  return(df)
}

numberOfQuizCompletedWeekly = function(df) {
  return(df  %>% group_by(learner_id, week_number) %>% summarize(num_quizes_done = length(quiz_question), .groups = "drop"))
}

avgNumberOfQuizCompletedWeekly = function(df) {
  #' Provides the number of quizzes done
  df = numberOfQuizCompletedWeekly(df)
  df = df %>% group_by(learner_id) %>% summarise(mean_num_quizes_weekly = mean(num_quizes_done))
  return(df)
}

asDoubleFactor = function(x) {
  as.numeric(as.factor(x))
  }

avgPerformance = function(type) {
  avg_performance_1 = caculateAveragePerformance(typeOfUser(measurePerformance(cyber.security.1_question.response), type), type)
  avg_performance_2 = caculateAveragePerformance(typeOfUser(measurePerformance(cyber.security.2_question.response), type), type)
  avg_performance_3 = caculateAveragePerformance(typeOfUser(measurePerformance(cyber.security.3_question.response), type), type)
  avg_performance_4 = caculateAveragePerformance(typeOfUser(measurePerformance(cyber.security.4_question.response), type), type)
  avg_performance_5 = caculateAveragePerformance(typeOfUser(measurePerformance(cyber.security.5_question.response), type), type)
  avg_performance_6 = caculateAveragePerformance(typeOfUser(measurePerformance(cyber.security.3_question.response), type), type)
  avg_performance_7 = caculateAveragePerformance(typeOfUser(measurePerformance(cyber.security.7_question.response), type), type)
  avg_performance_1$group = "run 1"
  avg_performance_2$group = "run 2"
  avg_performance_3$group = "run 3"
  avg_performance_4$group = "run 4"
  avg_performance_5$group = "run 5"
  avg_performance_6$group = "run 6"
  avg_performance_7$group = "run 7"
  avg_performance = rbind(avg_performance_1, avg_performance_2, avg_performance_3, avg_performance_4, avg_performance_5,avg_performance_6, avg_performance_7)
  return(avg_performance)
}

plotAvgPerformance = function(types) {
  #' This will be used for plotting 
  #' a performance against, based on the type of user
  graphs = list()
  for (i in seq(1:length(types))) {
    type = types[i]
    avg_performance = avgPerformance(type)
    graphs[[i]] = ggplot(avg_performance, aes(x=group, y=mean, group=type, col=type, fill=type)) +
      geom_point() + geom_line() + labs(x="runs", y="Performance")
  }
  return(graphs)
}

plotPerformanceDist = function(type) {
  df = rbind(cyber.security.1_question.response, cyber.security.2_question.response, 
                        cyber.security.3_question.response, cyber.security.4_question.response, 
                        cyber.security.5_question.response, cyber.security.6_question.response,
                        cyber.security.7_question.response)
  df =  typeOfUser(measurePerformance(df), type)
  return(ggplot(data = df, aes(x = mean, y = type, fill=type)) 
         + geom_boxplot(outlier.colour="red") + 
           geom_jitter(alpha=0.1, color="black") + 
           scale_colour_brewer("Diamond\nclarity") + 
           labs(x="Avg performance", y=type))
}