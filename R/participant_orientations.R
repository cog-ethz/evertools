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
get_participant_orientations_yaw <- function(db, session.id = 1, scene.name = "Tolman_01"){
  device_id <- get_sensor_id_by_name(db,"euler_angles")
  time_info <- get_session_scene_time_information(db = db, 
                                                  session.id = session.id, 
                                                  scene.name = scene.name)
  print(time_info)
  data <- get_sensor_data_3d(db = db, 
                             session.id = session.id, 
                             sensor.id = as.numeric(device_id), 
                             start.time = time_info$start, 
                             end.time = time_info$end)
  return(data %>% select(y,time))
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
get_participant_orientations<- function(db, session.id = 1, scene.name = "Tolman_01"){
  device_id <- get_sensor_id_by_name(db,"euler_angles")
  time_info <- get_session_scene_time_information(db = db, 
                                                  session.id = session.id, 
                                                  scene.name = scene.name)
  print(time_info)
  data <- get_sensor_data_3d(db = db, 
                             session.id = session.id, 
                             sensor.id = as.numeric(device_id), 
                             start.time = time_info$start, 
                             end.time = time_info$end)
  return(data %>% select(x,y,z,time))
}