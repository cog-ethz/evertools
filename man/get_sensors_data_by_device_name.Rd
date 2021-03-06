% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/participant_sensor_data.R
\name{get_sensors_data_by_device_name}
\alias{get_sensors_data_by_device_name}
\title{Get Sensors Data By Device Name}
\usage{
get_sensors_data_by_device_name(db, session.ids = c(1),
  device.name = "JRD")
}
\arguments{
\item{db}{dbyplr database handle}

\item{session.id}{session to be analysed}

\item{sensor.name}{sensor to be loaded}

\item{scene.name}{scene to be loaded}
}
\value{
List of values for sensors in a device
}
\description{
This function extracts data for a given
device.
}
\examples{
data <- get_sensors_data_by_device_name(db,  session.id = c(1), device.name = "JRD")

}
\keyword{database,}
\keyword{lazy,}
\keyword{scene}
\keyword{session,}
