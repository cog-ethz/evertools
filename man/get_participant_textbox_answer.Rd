% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/participant_questions.R
\name{get_participant_textbox_answer}
\alias{get_participant_textbox_answer}
\title{Get Participant Textbox Answer}
\usage{
get_participant_textbox_answer(db, session.id = 45,
  question.name = "socDem2")
}
\arguments{
\item{db}{dyplr database handle}

\item{session.id}{session from which to load data}

\item{question.name}{name of question as identified in xml}
}
\value{
question and answer of participant
}
\description{
This function extracts a participant's answer from
the database. The evaluation is lazy and this data can be
used as input for further remote computations.
}
\details{
Note: Converts to integer, numeric or string
}
\examples{
question <- get_participant_textbox_answer(db,session.id = 45,question.name="socDem2")

}
\keyword{database,}
\keyword{lazy,}
\keyword{questionnaire}
\keyword{session,}
