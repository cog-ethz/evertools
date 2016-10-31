#' Get Participant Times
#'
#' This function extracts a participant's time from
#' the database. The evaluation is lazy and this data can be
#' used as input for further remote computations.
#' @param db dyplr database handle
#' @param session.id session from which to load data
#' @param scene.id scene from which to load the data
#' @keywords session, database, lazy, position
#' @export
#' @return time in 
#' @examples
#' positions <- get_participant_positions_YXZ(session.id = 5)
#' 
get_participant_times <- function(db, session.id = 1, scene.id = 0){
  positions <- db %>% 
    tbl("store_positions") %>% 
    filter(session_id==session.id && scene_id ==scene.id) %>%
    select(timestamp) %>% 
    collapse() 
  return(positions)
}

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
#' positions <- get_participant_positions_YXZ(session.id = 5)
#' 
get_participant_duration <- function(db, session.id = 1, scene.id = 0){
  table <- get_participant_times(db,session.id = session.id, scene.id = scene.id) %>% collect()
  table$time <- as.numeric(as.POSIXct( table$timestamp))
  if (nrow(table)> 0) {
    dtable <- table[2:nrow(table),2] - table[1:nrow(table)-1,2]
    length <- sum(apply(X = dtable, MARGIN = 1, FUN = norm, '2'))
  } else {
    length <- NA
  }
  return(length)
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
get_participants_duration <- function(db, session.ids = c(1), scene.id = 0){
  session_id <- session.ids
  scene_id <- rep(scene.id, times = length(session.ids))
  duration <- vector(length = length(session.ids))
  for (id in session_id) {
    duration[id] <- get_participant_duration(db,session.id = id, scene.id = scene.id)
  }
  durations <- data.frame(scene_id,session_id,duration)
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
get_participants_duration_all <- function(db, session.ids = c(1), scene.ids = c(0)){
  session_id <- session.ids
  scene_id <- scene.ids
  df <- NULL
  for (id in scene_id) {
    dataframe <- get_participants_duration(db,session.ids = session_id, scene.id = id)
    if (is.null(df)){
      df <- dataframe 
    } else {
      df <- merge(x = df, y = dataframe, all = TRUE)
    }
  }
  return(df)
}