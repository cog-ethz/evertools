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
  positions <- db %>% 
    tbl("store_positions") %>% 
    filter(session_id==session.id && scene_id ==scene.id) %>%
    select(view_euler_angle_y,timestamp) %>% 
    collapse() 
  return(positions)
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
  positions <- db %>% 
    tbl("store_positions") %>% 
    filter(session_id==session.id && scene_id ==scene.id) %>%
    select(view_euler_angle_x,view_euler_angle_y,view_euler_angle_z,timestamp) %>% 
    collapse() 
  return(positions)
}