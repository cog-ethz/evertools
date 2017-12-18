#' Get Questions as Data Frame
#'
#' This function extracts all participants' answers from
#' the database. 
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
    df[,name] <- convert(question$value)
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
    
    names(question) <- convert(names(question))
    if(sapply(question,is.numeric)[3]){
      gatherSpread <-question %>% 
        select(-pos) %>%
        gather(key, value, -c(session_id)) %>% 
        mutate(value = ifelse(value == 0, NA, value)) %>%
        na.omit() %>% 
        spread(key, value,drop=F,fill = 0)
    } else {
      gatherSpread <-question %>% 
        select(-pos) %>%
        gather(key, value, -c(session_id)) %>% 
        na.omit() %>% 
        spread(key, value,drop=F)
      nAnswers <- apply(gatherSpread,2,nchar)
      textCols <- colSums(nAnswers,na.rm = T)
      gatherSpread[nAnswers==0]<-1
      gatherSpread[rep(!as.logical(textCols),
                       each=nrow(gatherSpread))
                   &is.na(gatherSpread)]<-0
    }
    names(gatherSpread) <- paste(name,names(gatherSpread))
    df<-merge(df,
              as.data.frame(sapply(gatherSpread,
                                   convert)),
              by.x = "session.ids",
              by.y = paste(name,"session_id"))
  }
  return(df)
}