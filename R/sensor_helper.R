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

#' Get Sensor Data 3D
#'
#' This function extracts data for a given
#' 3d sensor for a given time spawn.
#' @param db dyplr database handle
#' @param session.id session to be analysed
#' @param sensor.id sensor to be analysed
#' @param start.time beginning of time segment
#' @param end.time end of time segment, ignored if 0.
#' @keywords session, database, lazy, scene
#' @export
#' @examples
#' data <- get_sensor_data_3d(db,  session.id = 0, sensor.id = 0, start.time=0, end.time=0)
#'
get_sensor_data_3d <- function(db, session.id = 0, sensor.id = 0, start.time=0, end.time=0){
  if (end.time == 0){
    end.time = as.POSIXct(Sys.time())
  } else {
    as.POSIXct(end.time)
  }
  if (start.time == 0){
    start.time = as.POSIXct("1980-01-01 00:00:01 GMT")
  } else {
    as.POSIXct(start.time)
  }
  if(!is.POSIXct(end.time)||!is.POSIXct(start.time)){
    warning("Invalid time input. Time must be POSIXct: end.time="  %++% end.time %++% ", start.time=" %++% start.time)
  }
  start.time = format(start.time,'%Y-%m-%d %H:%M:%S')
  end.time = format(end.time,'%Y-%m-%d %H:%M:%S')
  data <- db %>%
    tbl("sensor_data_3d") %>%
    filter(session_id==session.id &&
             data_description_id ==sensor.id &&
             time > start.time &&
             time < end.time) %>%
    collapse()
  return(data)
}

#' Get Sensor Data
#'
#' This function extracts data for a given
#' 3d sensor for a given time spawn.
#' @param db dyplr database handle
#' @param session.id session to be analysed
#' @param sensor.id sensor to be analysed
#' @param start.time beginning of time segment
#' @param end.time end of time segment, ignored if 0.
#' @keywords session, database, lazy, scene
#' @export
#' @examples
#' data <- get_sensor_data(db,  session.id = 0, sensor.id = 0, start.time=0, end.time=0)
#'
get_sensor_data <- function(db, session.id = 0, sensor.id = 0, start.time=0, end.time=0){
  if (end.time == 0){
    end.time = as.POSIXct(Sys.time())
  } else {
    as.POSIXct(end.time)
  }
  if (start.time == 0){
    start.time = as.POSIXct("1980-01-01 00:00:01 GMT")
  } else {
    as.POSIXct(start.time)
  }
  if(!is.POSIXct(end.time)||!is.POSIXct(start.time)){
    warning("Invalid time input. Time must be POSIXct: end.time="  %++% end.time %++% ", start.time=" %++% start.time)
  }
  start.time = format(start.time,'%Y-%m-%d %H:%M:%S')
  end.time = format(end.time,'%Y-%m-%d %H:%M:%S')
  data <- db %>%
    tbl("sensor_data") %>%
    filter(session_id==session.id &&
             data_description_id ==sensor.id &&
             time > start.time &&
             time < end.time) %>%
    collapse()
  return(data)
}

#' Get Sensor Data 3D By Scene
#'
#' This function extracts data for a given
#' 3d sensor for a given scene.
#' @param db dyplr database handle
#' @param session.id session to be analysed
#' @param sensor.name sensor to be loaded
#' @param scene.name scene to be loaded
#' @keywords session, database, lazy, scene
#' @export
#' @examples
#' data <- get_sensor_data_3d_by_scene(db,  session.id = 0, sensor.name = "position", scene.name="The_Viking_Village")
#'
get_sensor_data_3d_by_scene  <-function(db,  session.id = 0, sensor.name = "position", scene.name="The_Viking_Village"){
  device_id <- get_sensor_id_by_name(db,sensor.name)
  time_info <- get_session_scene_time_information(db = db,
                                                  session.id = session.id,
                                                  scene.name = scene.name)
  data <- get_sensor_data_3d(db = db,
                             session.id = session.id,
                             sensor.id = as.numeric(device_id),
                             start.time = time_info$start,
                             end.time = time_info$end)
  return(data)
}

#' Get Sensor Data 3D By Scene
#'
#' This function extracts data for a given
#' 3d sensor for a given scene.
#' @param db dyplr database handle
#' @param session.id session to be analysed
#' @param sensor.name sensor to be loaded
#' @param scene.name scene to be loaded
#' @keywords session, database, lazy, scene
#' @export
#' @examples
#' data <- get_sensor_data_3d_by_scene(db,  session.id = 0, sensor.name = "position", scene.name="The_Viking_Village")
#'
get_sensor_data_by_scene  <-function(db,  session.id = 0, sensor.name = "position", scene.name="The_Viking_Village"){
  device_id <- get_sensor_id_by_name(db,sensor.name)
  time_info <- get_session_scene_time_information(db = db,
                                                  session.id = session.id,
                                                  scene.name = scene.name)
  data <- get_sensor_data(db = db,
                             session.id = session.id,
                             sensor.id = as.numeric(device_id),
                             start.time = time_info$start,
                             end.time = time_info$end)
  return(data)
}
