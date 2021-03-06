% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/participant_orientations.R
\name{get_participants_orientations}
\alias{get_participants_orientations}
\title{Get Participants Orientations}
\usage{
get_participants_orientations(db, session.ids = c(1),
  scene.name = "Tolman_01")
}
\arguments{
\item{db}{dyplr database handle}

\item{session.id}{session from which to load data}

\item{scene.id}{scene from which to load the data}
}
\description{
This function extracts a participants orientation
(x/roll,y/yaw,z/pitch in euler angle) and time from
the database. The evaluation is lazy and this data can be
used as input for further remote computations.
}
\examples{
orientations <- get_participants_orientations(db, session.ids = c(1), scene.name = "The_Viking_Village")

}
\keyword{database,}
\keyword{lazy,}
\keyword{orientation}
\keyword{session,}
