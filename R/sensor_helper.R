#' Get Scene Name By Id
#'
#' This function extracts a a scene id by the scene
#' name.
#' @param db dyplr database handle
#' @param scene.id scene id to be named
#' @keywords session, database, lazy, orientation
#' @export
#' @examples
#' scene_name <- get_sensor_name_by_id(db, sensor.id = 5)
#'
#'
get_sensor_name_by_id <- function(db, sensor.id = 0){
  sensor_name <- db %>%
    tbl("data_description") %>%
    filter(id==sensor.id) %>%
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
#' sensor_id <- get_sensor_id_by_name(db, scene.name = "5")
#'
get_sensor_id_by_name <- function(db, sensor.name = "0"){
  sensor_id <- db %>%
    tbl("data_description") %>%
    filter(description==sensor.name) %>%
    select(id) %>%
    collect()
  return(sensor_id)
}

#' Get Sensor Ids By Device Name
#'
#' This function extracts sensor ids attached
#' to a particular device.
#' @param db dyplr database handle
#' @param device.name device to be loaded
#' @keywords device, database, lazy, session, sensors
#' @export
#' @return ids of sensors connected to a device.
#' @examples
#' data <- get_sensor_id_by_device_name(db, device.name = "JRD")
#'
get_sensor_ids_by_device_name <- function(db, device.name = "JRD"){
  sensors <- db %>% tbl("data_origin") %>%
    filter (device_name == device.name) %>%
    left_join(db %>% tbl("data_description"),by = c("id"="device_id")) %>%
    select(sensor_id = id.y,description)
  return(sensors)
}

