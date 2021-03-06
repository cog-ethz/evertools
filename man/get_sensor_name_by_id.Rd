% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sensor_helper.R
\name{get_sensor_name_by_id}
\alias{get_sensor_name_by_id}
\title{Get Scene Name By Id}
\usage{
get_sensor_name_by_id(db, sensor.id = 0)
}
\arguments{
\item{db}{dyplr database handle}

\item{scene.id}{scene id to be named}
}
\description{
This function extracts a a scene id by the scene
name.
}
\examples{
scene_name <- get_sensor_name_by_id(db, sensor.id = 5)


}
\keyword{database,}
\keyword{lazy,}
\keyword{orientation}
\keyword{session,}
