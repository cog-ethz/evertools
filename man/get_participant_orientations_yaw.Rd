% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/participant_orientations.R
\name{get_participant_orientations_yaw}
\alias{get_participant_orientations_yaw}
\title{Get Participant Orientations (Yaw)}
\usage{
get_participant_orientations_yaw(db, session.id = 1,
  scene.name = "Tolman_01")
}
\arguments{
\item{db}{dyplr database handle}

\item{session.id}{session from which to load data}

\item{scene.id}{scene from which to load the data}
}
\description{
This function extracts a participants orientation
(y/yaw in euler angle) and time from
the database. The evaluation is lazy and this data can be
used as input for further remote computations.
}
\examples{
yaws <- get_participant_orientations_yaw(session.id = 5)

}
\keyword{database,}
\keyword{lazy,}
\keyword{orientation}
\keyword{session,}
