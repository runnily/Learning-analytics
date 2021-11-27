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

performanceVsActivity = function(activityDf) {
  
  ## 1. calculate performance for all users
  
  questionDf = rbind(cyber.security.1_question.response, cyber.security.2_question.response, 
                     cyber.security.3_question.response, cyber.security.4_question.response, 
                     cyber.security.5_question.response, cyber.security.6_question.response,
                     cyber.security.7_question.response)
  questionDf = measurePerformance(questionDf)
  questionDf = data.frame(row.names = questionDf$learner_id, val = questionDf$mean)
  
  ## 2. calculate the number of activities completed
  
  ## filter out where the column last completed at has no data
  activityDf = activityDf %>% filter(last_completed_at != "")
  
  ## counts how many are completed
  activityDf = activityDf %>% group_by(learner_id) %>% summarize(num_of_activties_completed = length(last_completed_at))
  df = data.frame(learner_id = activityDf$learner_id)
  
  ## focus only on the the number of activities completed
  activityDf = data.frame(row.names = activityDf$learner_id, val = activityDf$num_of_activties_completed)
  
  df$performance = questionDf[df$learner_id,] # provides mean
  df$num_of_activties_completed = activityDf[df$learner_id,] # provides activities completed
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

measurePerformance = function(df) {
  #' This is going to be used to measure the performance 
  #' of an individual
  return(df %>% group_by(learner_id) %>% summarize(mean = sum(correct =="true")/length(correct)))
}

caculateAveragePerformance = function(df, type) {
  return(df %>% group_by(type) %>% summarise(mean = mean(mean)))
}

plotAvgPerformance = function(types) {
  #' This will be used for plotting 
  #' a performance against, based on the type of user
  graphs = list()
  for (i in seq(1:length(types))) {
    type = types[i]
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
    graphs[[i]] = ggplot(avg_performance, aes(x=group, y=mean, group=type, col=type, fill=type)) +
      geom_point() + geom_line() + labs(x="runs", y="Performance")
  }
  return(graphs)
}

plotPerformanceDist = function(type, nrow = 4, ncol = 2, overall = FALSE) {
  dfs = list()
  if (overall) {
    df_1 = rbind(cyber.security.1_question.response, cyber.security.2_question.response, 
                          cyber.security.3_question.response, cyber.security.4_question.response, 
                          cyber.security.5_question.response, cyber.security.6_question.response,
                          cyber.security.7_question.response)
    df_1 =  typeOfUser(measurePerformance(df_1), type)
    dfs = list(df_1)
  } else {
    df_1 = typeOfUser(measurePerformance(cyber.security.1_question.response), type)
    df_2 = typeOfUser(measurePerformance(cyber.security.2_question.response), type)
    df_3 = typeOfUser(measurePerformance(cyber.security.3_question.response), type)
    df_4 = typeOfUser(measurePerformance(cyber.security.4_question.response), type)
    df_5 = typeOfUser(measurePerformance(cyber.security.5_question.response), type)
    df_6 = typeOfUser(measurePerformance(cyber.security.6_question.response), type)
    df_7 = typeOfUser(measurePerformance(cyber.security.7_question.response), type)
    dfs = list(df_1, df_2, df_3, df_4, df_5, df_6, df_7)
  }
  graphs = list()
  for (i in seq(1:length(dfs))) {
    df = dfs[[i]]
    graphs[[i]] = ggplot(data = df, aes(x = mean, y = type, fill=type)) + geom_boxplot(outlier.colour="red")+geom_jitter(alpha=0.2, color="black")+scale_colour_brewer("Diamond\nclarity")
  }
  return(grid.arrange(grobs = graphs, nrow= nrow, ncol = ncol))
}