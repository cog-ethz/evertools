#' Get Participant Positions (XYZ)
#'
#' This function extracts a participants position (x,y,z) and time from
#' the database. The evaluation is lazy and this data can be
#' used as input for further remote computations.
#' @param db dyplr database handle
#' @param session.id session from which to load data
#' @param scene.id scene from which to load the data
#' @keywords session, database, lazy, position
#' @export
#' @examples
#' positions <- get_participant_positions_YXZ(db, session.id = 1, scene.name = "Tolman_01")
#' 
get_participant_positions_XYZ <- function(db, session.id = 1, scene.name = "Tolman_01"){
  device_id <- get_sensor_id_by_name(db,"position")
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

#' Get Participant Positions (XYZ)
#'
#' This function extracts a participants position (x,z) and time from
#' the database. The evaluation is lazy and this data can be
#' used as input for further remote computations.
#' Note that the Y axis corresponds to the Up Vector in unity data.
#' This function corresponds to position in a projection to a plane 
#' with the Up vector as a normal.
#' @param db dyplr database handle
#' @param session.id session from which to load data
#' @param scene.id scene from which to load the data
#' @keywords session, database, lazy, position
#' @export
#' @examples
#' positions <- get_participant_positions_YXZ(db, session.id = 1, scene.name = "Tolman_01")
#' 
get_participant_positions_XZ <- function(db, session.id = 1, scene.name = "Tolman_01"){
  
  device_id <- get_sensor_id_by_name(db,"position")
  time_info <- get_session_scene_time_information(db = db, 
                                                  session.id = session.id, 
                                                  scene.name = scene.name)
  print(time_info)
  data <- get_sensor_data_3d(db = db, 
                             session.id = session.id, 
                             sensor.id = as.numeric(device_id), 
                             start.time = time_info$start, 
                             end.time = time_info$end)
  device_id <- get_sensor_id_by_name(db,"position")
  
  return(data %>% select(x,z,time))
  return(positions)
}