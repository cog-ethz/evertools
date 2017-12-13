#' Get Questions as Data Frame
#'
#' This function extracts all participants' answer from
#' the database. 
#' 
#' @param db dyplr database handle
#' @param session.ids sessions from which to load data
#' @param participant.ids optional, identifier for participants
#' @keywords session, database, questionnaire
#' @export
#' @return all answers of all participants
#' @examples
#' questions.df <- get_questions_as_data_frame(db,session.ids = session.ids ,participant.ids = participant.ids)
#' 
get_questions_as_data_frame<-function(db, session.ids,participant.ids=NULL){
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
    names(question) <- iconv(names(question),"UTF-8")
    df[,name] <- iconv(question$value,"UTF-8")
  }
  
  for (name in as.data.frame(bool_single_questions)$name){
    question <- get_participants_bool_answers(db,session.id = session.ids,
                                              question.name=name)
    names(question) <- iconv(names(question),"UTF-8")
    df[,name] <-question[!rev(duplicated(rev(question$session_id))),]$pos
  }
  
  for (name in as.data.frame(bool_multiple_questions)$name){
    question <- get_participants_bool_answers(db,session.id = session.ids,
                                              question.name=name)
    names(question) <- iconv(names(question),"UTF-8")
    gatherSpread <-question %>% 
      select(-pos) %>%
      gather(key, value, -c(session_id)) %>% 
      na.omit() %>% 
      spread(key, value)
    nAnswers <- apply(gatherSpread,2,nchar)
    textCols <- colSums(nAnswers,na.rm = T)
    gatherSpread[nAnswers==0]<-1
    gatherSpread[rep(!as.logical(textCols),
                     each=nrow(gatherSpread))
                 &is.na(gatherSpread)]<-0
    names(gatherSpread) <- paste(name,names(gatherSpread))
    df<-merge(df,
              as.data.frame(sapply(gatherSpread,
                                   iconv,"UTF-8")),
              by.x = "session.ids",
              by.y = paste(name,"session_id"))
  }
  return(df)
}