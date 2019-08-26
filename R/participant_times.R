#' Get Participant Times
#'
#' This function computes the duration a participant spent in an
#' experiment scene
#' @param db dyplr database handle
#' @param session.id session from which to load data
#' @param scene.id scene from which to load the data
#' @keywords EVE, evaluation, duration
#' @export
#' @return Duration of a single scene
#' @examples
#' positions <- get_participant_duration(db, session.id = 5, scene.name "Tolman_01")
#'
get_participant_duration <- function(db, session.id = 1, scene.name = "Tolman_01"){
  device_id <- get_sensor_id_by_name(db,"euler_angles")
  time_info <- get_session_scene_time_information(db = db,
                                                  session.id = session.id,
                                                  scene.name = scene.name)
  return(time_info$duration)
}

#' Get Participants Duration
#'
#' This function extract the path length for a set of
#' participants based on session and one scene.
#' @param db dyplr database handle
#' @param session.ids sessions from which to load data
#' @param scene.id scene from which to load the data
#' @keywords EVE, evaluation, duration
#' @export
#' @return A table with scene ids, session ids and durations.
#' @examples
#' db <- setup_evertools()
#' paths <- get_participants_path_length(db, session.ids = c(1:8), scene.ids = 0)
#'
get_participants_duration <- function(db, session.ids = c(1), scene.name = "Tolman_01"){
  duration <- vector(length = length(session.ids))
  iter <- 1
  for (id in session.ids) {
    duration[iter] <- get_participant_duration(db,session.id = id, scene.name = scene.name)
    iter <- iter + 1
  }
  durations <- data.frame(scene.name,session.ids,duration)
  return(durations)
}


#' Get Participants Duration All
#'
#' This function extract the path length for a set of
#' participants based on sessions and scenes.
#' @param db dyplr database handle
#' @param session.ids sessions from which to load data
#' @param scene.ids scenes from which to load the data
#' @keywords EVE, evaluation, path
#' @export
#' @return A table with session ids and durations.
#' @examples
#' db <- setup_evertools()
#' paths <- get_participants_path_length_all(db, session.ids = c(1:8), scene.ids = c(0:3))
#'
get_participants_duration_all <- function(db, session.ids = c(1), scene.names = c("Tolman")){
  df <- NULL
  for (name in scene.names) {
    dataframe <- get_participants_duration(db,session.ids = session.ids, scene.name = name)
    if (is.null(df)){
      df <- dataframe
    } else {
      df <- merge(x = df, y = dataframe, all = TRUE)
    }
  }
  return(df)
}
