#' Get Scene Name By Id
#'
#' This function extracts a a scene id by the scene
#' name.
#' @param db dyplr database handle
#' @param scene.id scene id to be named
#' @keywords session, database, lazy, orientation
#' @export
#' @examples
#' scene_name <- get_sensor_name_by_id(db, scene.id = 5)
#' 
#' 
get_sensor_name_by_id <- function(db, scene.id = 0){
  sensor_name <- db %>% 
    tbl("data_description") %>% 
    filter(id==scene.id) %>%
    select(description) %>%
    collect()
  return(sensor_name)
}

#' Get Scene ID By Name
#'
#' This function extracts a a scene name by the scene
#' id
#' @param db dyplr database handle
#' @param scene.name scene to be identified
#' @keywords session, database, lazy, scene
#' @export
#' @examples
#' scene_name <- get_sensor_id_by_name(db, scene.name = "5")
#' 
get_sensor_id_by_name <- function(db, sensor.name = "0"){
  sensor_id <- db %>%
    tbl("data_description") %>%
    filter(description==sensor.name) %>%
    select(id) %>%
    collect()
  return(scene_id)
}