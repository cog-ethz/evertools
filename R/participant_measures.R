#' Compute JRD 3D Error
#'
#' This function extracts the angular error in a 3D
#' JRD task as defined in EVE.
#'
#' @param jrd_data Data.frame containing the task combo as indices into the destinations list in the description, the result in the value
#' @param destinations Data.frame containing the destination name in description and an x,y,z coordinate for each.
#' @keywords JRD, angular error
#' @export
#' @examples
#'
#' destinations <- data.frame(destination=c("Blacksmith workshop", "Gift shop", "Smokehouse", "Cooking shed", "Town hall", "Viking boat", "Wood storage", "Bakery"),
#'                            x=c(-39,28.5,-38,49.5,12.5,-67,-67,32.5),
#'                            y=c(6,6.5,5.5,2.5,5.5,4,5.5,4.5),
#'                            z=c(-23,-14.5,22,48.5,-5.5,31,0,24))
#' jrd_data <- get_sensors_data_by_device_name(db, device.name = "JRD_3D") %>% collect()
#'
#' jrd_data$error <- compute_jrd_3d_error(jrd_data, destinations)
#'
compute_jrd_3d_error <- function(jrd_data, destinations){
  jrd_data$value <- as.numeric(jrd_data$value)
  cols <- data.frame(t(as.data.frame(lapply(strsplit(jrd_data$description,""),as.numeric))))+1
  names(cols) <- c("Goal","Position","Direction")

  jrd_data <- cbind(jrd_data,cols)

  dt <- data.table(destinations %>% select(x,y,z))

  x <- destinations[jrd_data$Position,2:4]-destinations[jrd_data$Direction,2:4]
  y <- destinations[jrd_data$Position,2:4]-destinations[jrd_data$Goal,2:4]

  dt2 <- cbind(x,y)
  rownames(dt2)<-jrd_data$description

  jrd_data$correct_angles <- apply(dt2,1,function(a){angle_2d(a[c(1,3)],a[c(4,6)])})

  jrd_data$value <- abs(with(jrd_data, ifelse(value > 180,value-360,value)))

  return(with(jrd_data,value))
}

#' Compute JRD 2D Error
#'
#' This function extracts the angular error in a 2D
#' JRD task as defined in EVE.
#'
#' @param jrd_data Data.frame containing the task combo as indices into the destinations list in the description, the result in the value
#' @param destinations Data.frame containing the destination name in description and an x,y,z coordinate for each.
#' @keywords JRD, angle group error
#' @export
#' @examples
#' positions <- get_participant_positions_XYZ(db, session.id = 1, scene.name = "Tolman_01")
#'
#' destinations <- data.frame(destination=c("Blacksmith workshop", "Gift shop", "Smokehouse", "Cooking shed", "Town hall", "Viking boat", "Wood storage", "Bakery"),
#'                            x=c(-39,28.5,-38,49.5,12.5,-67,-67,32.5),
#'                            y=c(6,6.5,5.5,2.5,5.5,4,5.5,4.5),
#'                            z=c(-23,-14.5,22,48.5,-5.5,31,0,24))
#' jrd_data <- get_sensors_data_by_device_name(db, device.name = "JRD_3D") %>% collect()
#'
#' jrd_data$error <- compute_jrd_3d_error(jrd_data, destinations)
#'
compute_jrd_2d_error <- function(jrd_data, destinations){
  jrd_data$value <- as.numeric(jrd_data$value)
  cols <- data.frame(t(as.data.frame(lapply(strsplit(jrd_data$description,""),as.numeric))))+1
  names(cols) <- c("Goal","Position","Direction")

  jrd_data <- cbind(jrd_data,cols)

  dt <- data.table(destinations %>% select(x,y,z))

  x <- destinations[jrd_data$Position,2:4]-destinations[jrd_data$Direction,2:4]
  y <- destinations[jrd_data$Position,2:4]-destinations[jrd_data$Goal,2:4]

  dt2 <- cbind(x,y)
  rownames(dt2)<-jrd2_data$description

  jrd_data$correct_angles <- apply(dt2,1,function(a){angle_2d(a[c(1,3)],a[c(4,6)])})
  jrd_data$error <- with(jrd_data,ifelse(value==1,correct_angles-45,
                                           ifelse(value==2,correct_angles-90,
                                                  ifelse(value==3,correct_angles-135,
                                                         ifelse(value==5,correct_angles+135,
                                                                ifelse(value==6,correct_angles+90,
                                                                       ifelse(value==7,correct_angles+45,
                                                                              ifelse(value==4  & correct_angles <0,correct_angles+180,correct_angles-180))))))))

  jrd_data$error2 <- with(jrd_data,ifelse(error > -22.5 & error < 22.6,0,
                                            ifelse(error > -67.5 & error < 67.5,1,
                                                   ifelse(error > -112.5 & error < 112.5,2,
                                                          ifelse(error > -147.5 & error < -147.5,3,4)))))

  return(with(jrd_data,error2))
}
