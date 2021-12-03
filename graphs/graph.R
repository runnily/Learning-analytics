avgPerformance = function(type) {
  #' This would plot the average performance across run for a specific demographics
  #' 
  #' @description When given a type would calculate the average performance for a 
  #'              demographics across all the 7 runs
  #'
  #' @param type Specifies the demographic ("age_range", "gender" etc)
  #' @return a data frame consisting of the demographic specified by type, which run and specific performance.
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
  #' This would plot the average performance for a specific type
  #' 
  #' @description When given a type would calculate the average performance for a 
  #'              demographics.
  #'
  #' @param type Specifies the demographic ("age_range", "gender" etc)
  #' @return a data frame consisting of the demographic specified by type and specific performance.
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
  #' This is used to plot box plot to illustrate the distribution of average performance
  #' of a a specific demographic
  #' 
  #' @description The demographic is specified by type. This would construct a box plot
  #'              of the distribution of the average performance for the demographic.
  #'
  #' @param type Specifies the demographic ("age_range", "gender" etc)
  #' @return A ggplot box plot
  df = rbind(cyber.security.1_question.response, cyber.security.2_question.response, 
             cyber.security.3_question.response, cyber.security.4_question.response, 
             cyber.security.5_question.response, cyber.security.6_question.response,
             cyber.security.7_question.response)
  df =  typeOfUser(measurePerformance(df), type)
  return(ggplot(data = df, aes(x = mean, y = type, fill=type)) 
         + geom_boxplot(outlier.colour="red") + 
           geom_jitter(alpha=0.1, color="black") + 
           scale_colour_brewer("Diamond\nclarity") + 
           labs(x="(mean) Avg performance", y=type))
}