#' Get Participant Positions (XYZ)
#'
#' This function extracts a participants position (x,y,z) and time from
#' the database. The evaluation is lazy and this data can be
#' used as input for further remote computations.
#' @param db dbyplr database handle
#' @param session.id session from which to load data
#' @param scene.id scene from which to load the data
#' @keywords session, database, lazy, position
#' @export
#' @examples
#' positions <- get_participant_positions_XYZ(db, session.id = 1, scene.name = "Tolman_01")
#'
get_participant_positions_XYZ <- function(db, session.id = 1, scene.name = "Tolman_01"){
  data <- get_sensor_data_3d_by_scene(db=db,session.id=session.id,sensor.name="position",scene.name=scene.name)
  return(data %>% select(x,y,z,time))
}

#' Get Participant Positions (XZ)
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
#' positions <- get_participant_positions_XZ(db, session.id = 1, scene.name = "Tolman_01")
#'
get_participant_positions_XZ <- function(db, session.id = 1, scene.name = "Tolman_01"){
  data <- get_sensor_data_3d_by_scene(db=db,session.id=session.id,sensor.name="position",scene.name=scene.name)
  return(data %>% select(x,z,time))
}

#' Get Participants Positions (XZ)
#'
#' This function extracts a participants position (x,z) and time from
#' the database. The evaluation is lazy and this data can be
#' used as input for further remote computations.
#' Note that the Y axis corresponds to the Up Vector in unity data.
#' This function corresponds to position in a projection to a plane
#' with the Up vector as a normal.
#' @param db dbyplr database handle
#' @param session.id session from which to load data
#' @param scene.id scene from which to load the data
#' @keywords session, database, lazy, position
#' @export
#' @examples
#' positions <- get_participants_positions_XZ(db, session.id = c(1), scene.name = "Tolman_01")
#'
get_participants_positions_XZ <- function(db, session.ids = c(1), scene.name = "Tolman_01"){

  result <- list()
  for(session.id in session.ids){
    result[[session.id]] <- get_participant_positions_XZ(db=db,session.id=session.id,scene.name=scene.name) %>% collect()
  }
  result <- bind_rows(result, .id = "session_id")
  result$x <- as.numeric(result$x)
  result$z <- as.numeric(result$z)
  return(result)
}

#' Get Participants Positions (XYZ)
#'
#' This function extracts a participants position (x,z) and time from
#' the database. The evaluation is lazy and this data can be
#' used as input for further remote computations.
#' Note that the Y axis corresponds to the Up Vector in unity data.
#' This function corresponds to position in a projection to a plane
#' with the Up vector as a normal.
#' @param db dbyplr database handle
#' @param session.id session from which to load data
#' @param scene.id scene from which to load the data
#' @keywords session, database, lazy, position
#' @export
#' @examples
#' positions <- get_participants_positions_XYZ(db, session.id = c(1), scene.name = "Tolman_01")
#'
get_participants_positions_XYZ <- function(db, session.ids = c(1), scene.name = "Tolman_01"){
  result <- list()
  for(session.id in session.ids){
    result[[session.id]] <- get_participant_positions_XYZ(db=db,session.id=session.id,scene.name=scene.name) %>% collect()
  }
  result <- bind_rows(result, .id = "session_id")
  result$x <- as.numeric(result$x)
  result$y <- as.numeric(result$y)
  result$z <- as.numeric(result$z)
  return(result)
}
