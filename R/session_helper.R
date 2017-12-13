#' Get Session Scene Time Information
#'
#' This function extracts a a scene id by the scene
#' name.
#' @param db dyplr database handle
#' @param session.id session which to analyse
#' @param scene.name scene which to analyse
#' @param string.form returns results as strings if true or as numeric otherwise
#' @keywords session, database, lazy, orientation
#' @export
#' @examples
#' scene_name <- get_session_scene_time_information(db, session.id = "5", scene.name = "Test")
#'  
get_session_scene_time_information <- function(db, session.id= "0", scene.name = "Tolman_01", string.form = F){
  
  start_id <- get_sensor_id_by_name(db = db, sensor.name = "start")
  end_id <- get_sensor_id_by_name(db = db, sensor.name = "end")
  
  start_time <- db %>%
    tbl("system_data") %>%
    filter(value==scene.name && session_id == session.id && data_description_id==start_id$id) %>%
    select(time) %>%
    collect()
  
  end_time <- db %>%
    tbl("system_data") %>%
    filter(value==scene.name && session_id == session.id && data_description_id==end_id$id) %>%
    select(time) %>%
    collect()
  
  duration <- as.numeric(as.POSIXlt(end_time$time)-as.POSIXlt(start_time$time))
  
  if (string.form){
    return(list(start=start_time$time,end=end_time$time,duration=as.character(duration)))
  }else{
    return(list(start=as.POSIXct(start_time$time),end=as.POSIXct(end_time$time),duration=duration))
  }
}

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
  
  parameters <- NULL
  if (nrow(as.data.frame(session_parameters))>0){
    parameters<-dcast(as.data.frame(session_parameters), formula = session_id ~ parameter_description) %>%
      mutate_all(funs(type.convert(as.character(.))))
    
    names(parameters) <- tolower(names(parameters))
    names(parameters) <- sub(" ", "_", names(parameters))
  }
  return(parameters)
}