#' Get Scene ID By Name
#'
#' This function extracts a a scene id by the scene
#' name.
#' @param db dyplr database handle
#' @param scene.id scene from which to load the data
#' @keywords session, database, lazy, orientation
#' @export
#' @examples
#' scene_name <- get_scene_id_by_name(db, scene.id = "5")
#'  
get_scene_id_by_name <- function(db, scene.name = "0"){
  scene_id <- db %>%
    tbl("scene") %>%
    filter(scene_name==scene.name) %>%
    select(id) %>%
    collect()
  return(scene_id)
}

#' Get Scene Name By ID
#'
#' This function extracts a a scene name by the scene
#' id
#' @param db dyplr database handle
#' @param session.id session from which to load data
#' @param scene.name scene from which to load the data
#' @keywords session, database, lazy, orientation
#' @export
#' @examples
#' scene_name <- get_scene_name_by_id(db, scene.name = 5)
#'
get_scene_name_by_id <- function(db, scene.id = 0){
  scene_name <- db %>%
    tbl("scene") %>%
    filter(id==scene.id) %>%
    select(scene_name) %>%
    collect()
  return(scene_name)
}