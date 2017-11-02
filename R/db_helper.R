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
  session_parameters <- db %>% tbl("experiment_parameter") %>%
    filter(session_id %in% session.ids) %>% collect()
  
  parameters<-dcast(session_parameters, formula = session_id ~ parameter_descr) %>%
    mutate_each(funs(type.convert(as.character(.))))
  
  names(parameters) <- tolower(names(parameters))
  names(parameters) <- sub(" ", "_", names(parameters))
  
  return(parameters)
}