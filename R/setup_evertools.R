#' Setup EVE R Tools
#'
#' This function loads basic R libraries for the 
#' use with EVE and sets up the connection with
#' the database
#' @param db Type of database. Defaults to "mysql".
#' @keywords EVE, database
#' @export
#' @return Dplyr handle for the database12
#' @examples 
#' setup_evertools()
#' 
setup_evertools <- function(db="mysql"){
  library(DBI, warn.conflicts = FALSE,quietly = TRUE)
  if(db=="mysql"){
    library(RMySQL, warn.conflicts = FALSE,quietly = TRUE)
  }
  else {
    print("Requested database" + db + " not supported.")
  }
  library(dplyr, warn.conflicts = FALSE,quietly = TRUE)
  library(ggplot2, warn.conflicts = FALSE,quietly = TRUE)
  my_db = src_mysql(dbname = "neighborhood_walk", host = "localhost", user = "svictor", password = "vicDB4study.", port = 3306)
  #data <- tbl(my_db,sql("SELECT * FROM neighborhood_walk.store_positions WHERE session_id = \"1\" AND scene_id = \"0\""))
  return(my_db)
}