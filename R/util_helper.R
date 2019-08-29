#' Concatenate
#'
#' This function concatenates two strings with
#' a separating space
#' @param s1 First string, no trailing spaces.
#' @param s2 Second string. no leading spaces.
#' @keywords Helper, string
#' @export
#' @return Concatenated strings with a separating space
#' @examples
#' > s1 <- "a"
#' > s2 <- "b"
#' > concatenate(s1,s2)
#' [1] "a b"
concatenate <- function (s1,s2){
  return(paste(s1,s2,sep=" "))
}


#' Concatenate Operator
#'
#' This function wraps concatentate
#' @param s1 First string, no trailing spaces.
#' @param s2 Second string. no leading spaces.
#' @keywords Helper, string
#' @export
#' @return Concatenated strings with a separating space
#' @examples
#' > s1 <- "a"
#' > s2 <- "b"
#' > s1 %++% s2
#' [1] "a b"
`%++%` <- function (s1,s2)concatenate(s1,s2)

#' Year to Age
#'
#' This function converts years to ages. Assumes
#' that age is in the column "age".
#'
#' Note: Assumes that humans do not grow older than 150.
#' @param df Data frame with age in "age".
#' @param base_year Year the experiment took place
#' @keywords Helper, age, year, conversion
#' @export
#' @return Data frame with years converted to age
#' @examples
#' df <- year2age(df,2016)
year2age <- function(df,base_year){

  df$age <- ifelse(df$age>150,base_year-df$age,df$age)
  return(df)
}


#' Is POSIXct
#'
#' Tests whether an object is POSIXct.
#'
#' Note: Suggestion by https://stackoverflow.com/a/26413765
#' @param x Object to be tested
#' @keywords Helper, date, type-testing
#' @export
#' @return whether x is POSIXct
#' @examples
#' x <- "blub"
#' is.X.POSIXct <- is.POSIXct(x)#returns F
is.POSIXct <- function(x) inherits(x, "POSIXct")


#' Is POSIXlt
#'
#' Tests whether an object is POSIXlt.
#'
#' Note: Suggestion by https://stackoverflow.com/a/26413765
#' @param x Object to be tested
#' @keywords Helper, date, type-testing
#' @export
#' @return whether x is POSIXlt
#' @examples
#' x <- "blub"
#' is.X.POSIXlt <- is.POSIXlt(x)#returns F
is.POSIXlt <- function(x) inherits(x, "POSIXlt")


#' Is POSIXt
#'
#' Tests whether an object is POSIXt.
#'
#' Note: Suggestion by https://stackoverflow.com/a/26413765
#' @param x Object to be tested
#' @keywords Helper, date, type-testing
#' @export
#' @return whether x is POSIXt
#' @examples
#' x <- "blub"
#' is.X.POSIXt <- is.POSIXt(x)#returns F
is.POSIXt <- function(x) inherits(x, "POSIXt")


#' Is Date
#'
#' Tests whether an object is Date
#'
#' Note: Suggestion by https://stackoverflow.com/a/26413765
#' @param x Object to be tested
#' @keywords Helper, date, type-testing
#' @export
#' @return whether x is Date
#' @examples
#' x <- "blub"
#' is.X.Date <- is.Date(x)#returns F
is.Date <- function(x) inherits(x, "Date")


#' Point To Image
#'
#' Transforms a 3 point based on a projection and
#' world to camera matrix into the NPC space.
#' @param x Vector in 3D.
#' @param projection Matrix describing the camera projection.
#' @param worldToCamera Matrix describing the camera position.
#' @keywords Helper, projection, transformation, 3D vector
#' @export
#' @return x in NPC space.
#' @examples
#' x <- c(1,2,3)
#' projection <- matrix(c(1,0,0,0,0,1,0,0,0,0,1,-1,0,0,0,1),nrow=4,ncol = 4, byrow = TRUE)
#' worldToCamera <- matrix(c(-1,0,0,-25,0,0,-1,0,0,1,0,-30,0,0,0,1),nrow=4,ncol = 4, byrow = TRUE)
#' npc_coordinates <- point2image(x)
point2image <- function(x, projection, worldToCamera){
  projection %*% worldToCamera %*% as.vector(x)
}


#' Transform 3D Positions To Image
#'
#' Takes an data.frame of positions (coded with x,y,z)
#' and returns their position in NPC space.
#'
#' Note: Suggestion by https://stackoverflow.com/a/26413765
#' @param x Path data.
#' @param projection Matrix describing the camera projection.
#' @param worldToCamera Matrix describing the camera position.
#' @keywords Helper, path transformation, NPC projection
#' @export
#' @return Returns transformed path.
#' @examples
#' projection <- matrix(c(1,0,0,0,0,1,0,0,0,0,1,-1,0,0,0,1),nrow=4,ncol = 4, byrow = TRUE)
#' worldToCamera <- matrix(c(-1,0,0,-25,0,0,-1,0,0,1,0,-30,0,0,0,1),nrow=4,ncol = 4, byrow = TRUE)
#' position_data <- get_participants_positions_XYZ(db = db, session.ids = session.ids, scene.name = scene.name )
#' position_data <- transform_3d_positions_to_image(position_data,projection,worldToCamera)
transform_3d_positions_to_image <-function(position_data,projection,worldToCamera){

  dt <- apply(data.table(with(position_data,data.frame(x,y,z,1))),1,point2image,projection,worldToCamera)

  position_data$x <- dt[1,]
  position_data$y <- dt[2,]
  position_data$z <- dt[3,]

  return(position_data)
}
