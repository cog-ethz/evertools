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
#' positions <- get_participant_positions_YXZ(session.id = 5)
#' 
get_participant_positions_XYZ <- function(db, session.id = 1, scene.id = 0){
  device_id <- db %>% 
    tbl("data_description") %>% 
    filter(description=="position") %>%
    select(id) %>%
    collect()
  
  positions <- db %>% 
    tbl("sensor_data_3d") %>% 
    filter(session_id==session.id && data_description_id ==device_id$id) %>%
    select(x,y,z,time) %>% 
    collapse() 
  return(positions)
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
#' positions <- get_participant_positions_YXZ(session.id = 5)
#' 
get_participant_positions_XZ <- function(db, session.id = 1, scene.id = 0){
  device_id <- db %>% 
    tbl("data_description") %>% 
    filter(description=="position") %>%
    select(id) %>%
    collapse()
  
  positions <- db %>% 
    tbl("sensor_data_3d") %>% 
    filter(session_id==session.id && data_description_id ==device_id$id) %>%
    select(x,z,time) %>% 
    collapse() 
  return(positions)
}