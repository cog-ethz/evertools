#' Get Participant Orientations (Yaw)
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
  data <- get_sensor_data_3d_by_scene(db=db,session.id=session.id,sensor.name="euler_angles",scene.name=scene.name)
  return(data %>% select(y,time))
}

#' Get Participant Orientations
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
  data <- get_sensor_data_3d_by_scene(db=db,session.id=session.id,sensor.name="euler_angles",scene.name=scene.name)
  return(data %>% select(x,y,z,time))
}

#' Get Participants Orientations (Yaw)
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
#' yaws <- get_participants_orientations_yaw(db, session.ids = c(1), scene.name = "The_Viking_Village")
#'
get_participants_orientations_yaw <- function(db, session.ids = c(1), scene.name = "Tolman_01"){
  result <- list()
  for(session.id in session.ids){
    result[[session.id]] <- get_participant_orientations_yaw(db=db,session.id=session.id,scene.name=scene.name) %>% collect()
  }
  result <- bind_rows(result, .id = "session_id")
  result$y <- as.numeric(result$y)
  return(result)
}

#' Get Participants Orientations
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
#' orientations <- get_participants_orientations(db, session.ids = c(1), scene.name = "The_Viking_Village")
#'
get_participants_orientations<- function(db, session.ids = c(1), scene.name = "Tolman_01"){
  result <- list()
  for(session.id in session.ids){
    result[[session.id]] <- get_participant_orientations(db=db,session.id=session.id,scene.name=scene.name) %>% collect()
  }
  result <- bind_rows(result, .id = "session_id")
  result$x <- as.numeric(result$x)
  result$y <- as.numeric(result$y)
  result$z <- as.numeric(result$z)
  return(result)
}
