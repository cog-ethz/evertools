#' Get Sensor Data 3D
#'
#' This function extracts data for a given
#' 3d sensor for a given time spawn.
#' @param db dbyplr database handle
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
#' @param db dbyplr database handle
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
#' @param db dbyplr database handle
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
#' @param db dbyplr database handle
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

#' Get Sensors Data By Device Name
#'
#' This function extracts data for a given
#' device.
#' @param db dbyplr database handle
#' @param session.id session to be analysed
#' @param sensor.name sensor to be loaded
#' @param scene.name scene to be loaded
#' @keywords session, database, lazy, scene
#' @export
#' @return List of values for sensors in a device
#' @examples
#' data <- get_sensors_data_by_device_name(db,  session.id = c(1), device.name = "JRD")
#'
get_sensors_data_by_device_name <- function(db, session.ids = c(1), device.name = "JRD"){
  sensor_ids <- get_sensor_ids_by_device_name(db, device.name = device.name)

  result <- sensor_ids %>%
    left_join(db %>% tbl("sensor_data"),by=c("sensor_id"="data_description_id")) %>%
    select(description,session_id,value,time)
  return(result)
}
