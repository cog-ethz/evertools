#' Get Participant Orientations (Pitch)
#'
#' This function extracts a participants orientation 
#' (y/yaw in euler angle) and time from
#' the database. The evaluation is lazy and this data can be
#' used as input for further remote computations.
#' @param db dyplr database handle
#' @param session.id session from which to load data
#' @param scene.id scene from which to load the data
#' @keywords session, database, lazy, orientation
#' @export
#' @examples
#' yaws <- get_participant_orientations_yaw(session.id = 5)
#' 
get_participant_orientations_yaw <- function(db, session.id = 1, scene.id = 0){
  
  
  device_id <- get_sensor_id_by_name(db,"euler_angles")
  
  orientations <- db %>% 
    tbl("sensor_data_3d") %>% 
    filter(session_id==session.id && data_description_id ==device_id$id) %>%
    select(y,time) %>% 
    collapse() 
  return(orientations)
}

#' Get Participant Orientations (Pitch)
#'
#' This function extracts a participants orientation 
#' (x/roll,y/yaw,z/pitch in euler angle) and time from
#' the database. The evaluation is lazy and this data can be
#' used as input for further remote computations.
#' @param db dyplr database handle
#' @param session.id session from which to load data
#' @param scene.id scene from which to load the data
#' @keywords session, database, lazy, orientation
#' @export
#' @examples
#' orientations <- get_participant_orientations(session.id = 5)
#' 
get_participant_orientations<- function(db, session.id = 1, scene.id = 0){
  
  device_id <- get_sensor_id_by_name(db,"euler_angles")
  
  orientations <- db %>% 
    tbl("sensor_data_3d") %>% 
    filter(session_id==session.id && data_description_id ==device_id$id) %>%
    select(x,y,z,time) %>% 
    collapse() 
  return(orientations)
}