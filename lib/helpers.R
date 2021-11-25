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

typeOfUser = function(df, id, type) {
  #' This is used to find the type of user for a particular
  #' Data frame
  #'
  #' @description This function will get the type of user
  #'              from a specific data frame
  #' @param df  data frame name
  #' @param type The type to count 
  #' @return A tibble illustrating the number of distinct types
  return(df[[type]][df$learner_id == id])
}

translateCodeToCnt = function(srows) {
  #'This takes a specific data frame and translates
  #'them applies a region to it.
  #'
  map = data.frame(row.names = code$alpha.2, val = code$region)
  srows$region = map[srows$detected_country,]
  return(srows)
}