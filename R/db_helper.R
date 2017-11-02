#' Get Session Parameters
#'
#' This function extracts the session parameters for all
#' participants from the database.
#' 
#' @param db dyplr database handle
#' @param session.ids sessions from which to load data
#' @keywords session, database, parameters
#' @export
#' @return table of session parameters
#' @examples
#' parameters <- get_session_parameters(db, session.ids = c(45))
#' 
get_session_parameters<-function(db,session.ids=c(45)){
  
  experiment_parameters <- db %>% tbl("experiment_parameter") 
  session_parameter_values <- db %>% tbl("session_parameter_values") %>%
    filter(session_id %in% session.ids)
  
  session_parameters <- left_join(x = experiment_parameters %>% rename(experiment_parameter_id =id),y = session_parameter_values,by = c("experiment_parameter_id" = "experiment_parameter_id"))
  
  parameters<-dcast(as.data.frame(session_parameters), formula = session_id ~ parameter_description) %>%
    mutate_all(funs(type.convert(as.character(.))))
  
  names(parameters) <- tolower(names(parameters))
  names(parameters) <- sub(" ", "_", names(parameters))
  
  return(parameters)
}