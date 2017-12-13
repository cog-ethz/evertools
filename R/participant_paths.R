#' Get Participant Path Length
#'
#' This function extract the path length for a
#' given participant's session and scene.
#' @param db dyplr database handle
#' @param session.id session from which to load data
#' @param scene.id scene from which to load the data
#' @keywords EVE, evaluation, path
#' @export
#' @return Length of a single path
#' @examples 
#' db <- setup_evertools()
#' path <- get_participant_path_length(db, session.id = 1, scene.id = 0)
#' 
get_participant_path_length <- function(db, session.id = 1, scene.name = "Tolman_01"){
  table <- get_participant_positions_XZ(db,session.id = session.id, scene.name = scene.name) %>% collect()
  if (nrow(table)> 0) {
    dtable <- data.matrix(table[2:nrow(table),1:2]) - data.matrix(table[1:nrow(table)-1,1:2])
    length <- sum(apply(X = dtable, MARGIN = 1, FUN = norm, '2'))
  } else {
    length <- NA
  }
  return(length)
}

#' Get Participants Path Length
#'
#' This function extract the path length for a set of
#' participants based on session and one scene.
#' @param db dyplr database handle
#' @param session.ids sessions from which to load data
#' @param scene.id scene from which to load the data
#' @keywords EVE, evaluation, path
#' @export
#' @return A table with scene ids, session ids and path lengths.
#' @examples 
#' db <- setup_evertools()
#' paths <- get_participants_path_length(db, session.ids = c(1:8), scene.ids = 0)
#' 
get_participants_path_length <- function(db, session.ids = c(1), scene.name = "Tolman_01"){
  path_length <- vector(length = length(session.ids))
  iter <- 1
  for (id in session.ids) {
    path_length[iter] <- get_participant_path_length(db,session.id = id, scene.name = scene.name)
    iter <- iter + 1
  }
  paths <- data.frame(scene.name,session.ids,path_length)
  return(paths)
}

#' Get Participants Path Length All
#'
#' This function extract the path length for a set of
#' participants based on sessions and scenes.
#' @param db dyplr database handle
#' @param session.ids sessions from which to load data
#' @param scene.ids scenes from which to load the data
#' @keywords EVE, evaluation, path
#' @export
#' @return A table with session ids and path lengths.
#' @examples 
#' db <- setup_evertools()
#' paths <- get_participants_path_length_all(db, session.ids = c(1:8), scene.ids = c(0:3))
#' 
get_participants_path_length_all <- function(db, session.ids = c(1), scene.names = c("Tolman_01")){
  df <- NULL
  for (name in scene.names) {
     dataframe <- get_participants_path_length(db,session.ids = session.ids, scene.name = name)
     if (is.null(df)){
       df <- dataframe 
     } else {
       df <- merge(x = df, y = dataframe, all = TRUE)
     }
  }
  return(df)
}