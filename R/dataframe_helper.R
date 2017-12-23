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
    question <- get_participants_textbox_answer(db,session.id = session.ids,
                                                question.name=name)
    names(question) <- convert(names(question))
    if (ncol(question) > 2){
      df<-merge(df,
                as.data.frame(sapply(question,convert)),
                by.x = "session.ids",
                by.y = "session_id")
    } else {
      if (nrow(question)== nrow(df)){
        df[,name] <- convert(question$"NA")
      } else {
        question[,name] <- convert(question$"NA")
        question$"NA" <- NULL
        df<-merge(df,
                  as.data.frame(sapply(question,convert)),
                  by.x = "session.ids",
                  by.y = "session_id",
                  all = T)
      }
    }
  }
  
  for (name in as.data.frame(bool_single_questions)$name){
    question <- get_participants_bool_answers(db,session.id = session.ids,
                                              question.name=name)
    names(question) <- iconv(names(question),"UTF-8")
    df[,name] <-question[!rev(duplicated(rev(question$session_id))),]$pos
  }
  q_names <- as.data.frame(bool_multiple_questions)$name
  for (name in q_names){
    question <- get_participants_bool_answers(db,session.id = session.ids,
                                              question.name=name)
    if(is.na(question$session_id)) next 
    names(question) <- convert(names(question))
    gatherSpread <-question %>% 
      select(-pos) %>%
      gather(key, value, -c(session_id)) %>% 
      na.omit() %>% 
      spread(key, value,drop=F)
    
    names(gatherSpread) <- paste(name,names(gatherSpread))
    if (dim(gatherSpread)[1] > 1)
      df<-merge(df,
                as.data.frame(sapply(gatherSpread,
                                     convert)),
                by.x = "session.ids",
                by.y = paste(name,"session_id"))
    else 
      df<-merge(df,
                t(as.data.frame(sapply(gatherSpread,
                                       convert))),
                by.x = "session.ids",
                by.y = paste(name,"session_id"),all = T)
  }
  return(df)
}