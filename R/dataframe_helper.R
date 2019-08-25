#' Get Questions as Data Frame
#'
#' This function extracts all participants' answers from
#' the database. Note that this exports the answer indices
#' rather than the output values. So the value 0 describes
#' the first output.
#'
#' @param db dyplr database handle
#' @param session.ids sessions from which to load data
#' @param participant.ids optional, identifier for participants
#' @param utf8 optional allows to turn of encoding in utf8
#' @keywords database, questionnaire, export
#' @export
#' @return all answers of all participants
#' @examples
#' questions.df <- get_questions_as_data_frame(db,session.ids = session.ids ,participant.ids = participant.ids)
get_questions_as_data_frame<-function(db, session.ids,participant.ids=NULL, utf8 = T){
  if (utf8){
    convert <- function(data){return(iconv(data,"UTF-8"))}
  } else {
    convert <- function(data){return(data)}
  }
  if (!is.null(participant.ids)){
    df <- data.frame(participant.ids = participant.ids,session.ids = session.ids)
  } else {
    df <- data.frame(session.ids = session.ids)
  }
  question_info <- db %>% tbl("questions") %>% select(name, type) %>% filter(type != 8)
  text_questions <- question_info  %>% filter(type == 1)
  bool_single_questions <- question_info %>% filter(type > 2 && type < 8)
  bool_multiple_questions <- question_info %>% filter(type ==2 || type == 9)

  for (name in as.data.frame(text_questions)$name){
    question <- get_participants_textbox_answer(db,session.id = session.ids,column.name = name,
                                                question.name=name)
    names(question) <- convert(names(question))
    df<-merge(df,
              as.data.frame(sapply(question,convert)),
              by.x = "session.ids",
              by.y = "session_id",
              all.x=T)
  }

  q_names <- as.data.frame(bool_multiple_questions)$name
  for (name in q_names){
    question <- get_participants_bool_answers(db,session.id = session.ids,
                                              question.name=name)
    names(question) <- convert(names(question))
    question[[question$name[1]]]=question$choice

    if (nrow(question) == length(session.ids)){
      question <- question %>% select(session_id,question$name[1])
    } else {
      question <- reshape(question, direction="wide",idvar = c("session_id","question_id","name","question"),timevar = "question_row",drop=c("pos","choice"))
      question <- question %>% select(-one_of(c("question_id","question","name")))
    }
    df<-merge(df,
              as.data.frame(sapply(question,
                                   convert)),
              by.x = "session.ids",
              by.y = "session_id")

  }
  return(df)
}
