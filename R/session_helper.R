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
    return(list(start=as.POSIXct(start_time$time),end=as.POSIXct(end_time$time),duration=duration))
  }else{
    return(list(start=as.POSIXct(start_time$time),end=as.POSIXct(end_time$time),duration=duration))
  }
}