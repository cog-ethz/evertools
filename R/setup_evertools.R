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
setup_evertools <- function(db="mysql",dbname = "virtual_study", host = "localhost", user = "user", password = "password", port = 3306){
  my_db <- src_mysql(dbname = dbname, host = host, user = user, password = password, port = port)
  return(my_db)
}