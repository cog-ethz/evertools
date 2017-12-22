#' Get Participant Boolean Answer
#'
#' This function extracts a participant's answer from
#' the database. The evaluation is lazy and this data can be
#' used as input for further remote computations.
#' 
#' @param db dyplr database handle
#' @param session.id session from which to load data
#' @param question.name name of question as identified in xml
#' @keywords session, database, lazy, questionnaire
#' @export
#' @return question and answer of participant
#' @examples
#' question <- get_participant_bool_answers(db,session.id = 45,question.name="socDem1")
#' 
get_participant_bool_answers <-function(db,session.id=45,question.name="socDem1"){
  
  answer_ids <- get_participant_answers_ids(db,session.id=session.id)
  answers_stored_bools <- db %>% tbl("answers_stored_strings")
  questions <- db %>% tbl("questions") %>% filter(name == question.name)
  questions_stored_strings <- db %>% tbl("questions_stored_strings")
  store_strings <- db %>% tbl("store_strings")
  
  #Get Label
  question_options <- left_join(questions,questions_stored_strings,by = c("id" = "questions_id"))
  question_options_text <-  left_join(question_options,store_strings%>% rename(string_id =id),c("string_id" = "string_id"))%>%
    filter(type.y=="string") %>%
    select(pos,label = val)
  
  #Get Value
  bool_ids <- left_join(answer_ids,answers_stored_bools,by = c("answer_id" = "answer_id")) %>%
    select(answer_id,session_id,question_id,string_id)
  bool_answers_ids <- left_join(questions %>% rename(question_id =id),bool_ids,c("question_id" = "question_id"))
  bool_answers <- left_join(bool_answers_ids,store_strings %>% rename(string_id =id),c("string_id" = "string_id"))
  
  result <- left_join(bool_answers,question_options_text,c("pos" = "pos")) %>%
    select(session_id,question_id,name,pos,label,val) 
  return(result)
}

#' Get Participants Boolean Answer
#'
#' This function extracts participants' answers from
#' the database.
#' 
#' @param db dyplr database handle
#' @param session.id session from which to load data
#' @param question.name name of question as identified in xml
#' @keywords session, database, questionnaire
#' @export
#' @return question and answer of participant
#' @examples
#' question <- get_participants_bool_answers(db,session.id = 45,question.name="socDem1")
#' 
get_participants_bool_answers <-function(db,session.ids=c(45),question.name="socDem1"){
  
  df <- data.frame()
  other <- get_participant_bool_answers(db,session.id = session.ids[1],question.name = question.name) %>% collect()
  df <- merge(x = df, y = other, all = TRUE)
  for(id in session.ids){
    other <- get_participant_bool_answers(db,session.id = id,question.name = question.name) %>% collect()
    df <- merge(x = df, y = other, all = TRUE)
  }
  df$question_id <- NULL
  df$name <- NULL
  result<-dcast(df, formula = session_id +pos ~ label)
  return(result)
}

#' Get Participant Textbox Answer
#'
#' This function extracts a participant's answer from
#' the database. The evaluation is lazy and this data can be
#' used as input for further remote computations.
#' 
#' Note: Converts to integer, numeric or string
#' @param db dyplr database handle
#' @param session.id session from which to load data
#' @param question.name name of question as identified in xml
#' @keywords session, database, lazy, questionnaire
#' @export
#' @return question and answer of participant
#' @examples
#' question <- get_participant_textbox_answer(db,session.id = 45,question.name="socDem2")
#' 
get_participant_textbox_answer<-function(db,session.id=45,question.name="socDem2"){
  
  answer_ids <- get_participant_answers_ids(db,session.id=session.id)
  answers_stored_strings <- db %>% tbl("answers_stored_strings")
  
  questions_stored_strings <- db %>% tbl("questions_stored_strings")
  questions <- db %>% tbl("questions") %>% filter(name == question.name)
  store_strings <- db %>% tbl("store_strings")
  
  #Get Integer Value
  answer_id <- left_join(answer_ids,answers_stored_strings,by = c("answer_id" = "answer_id")) %>%
    select(answer_id,session_id,question_id,string_id)
  text_id <- left_join(questions %>% rename(question_id =id),answer_id,c("question_id" = "question_id"))
  
  if (length(text_id)>1){
    #Get Label
    question_options <- left_join(questions,questions_stored_strings,by = c("id" = "questions_id"))
    question_options_text <-  left_join(question_options,store_strings%>% rename(string_id =id),c("string_id" = "string_id"))%>%
      filter(type.y=="string") %>%
      select(pos,label = val)
    
    result <- left_join(text_id,store_strings %>% rename(string_id =id),c("string_id" = "string_id")) %>% 
      left_join(question_options_text, c("pos"="pos")) %>%
      select(session_id,question_id,label,name,val) %>% collect () %>%
      mutate_all(funs(type.convert(as.character(.)))) #finds optimal type for characters
    
    result <- result %>% dcast( session_id + question_id + name ~ label)
  } else {
    result <- left_join(text_id,store_strings %>% rename(string_id =id),c("string_id" = "string_id")) %>% 
      select(session_id,question_id,name,val) %>% collect () %>%
      mutate_all(funs(type.convert(as.character(.)))) #finds optimal type for characters
  }
  
  return(result)
}

#' Get Participants Textbox Answer
#'
#' This function extracts participants' answers from
#' the database. The evaluation is lazy and this data can be
#' used as input for further remote computations.
#' 
#' Note: Converts to integer, numeric or string
#' @param db dyplr database handle
#' @param session.id session from which to load data
#' @param question.name name of question as identified in xml
#' @keywords session, database, lazy, questionnaire
#' @export
#' @return question and answer of participant
#' @examples
#' question <- get_participants_textbox_answer(db,session.id = 45,question.name="socDem2")
#' 
get_participants_textbox_answer<-function(db,session.ids=c(45),column.name="value",question.name="socDem2"){
  df <- data.frame()
  new_entry <- get_participant_textbox_answer(db,session.id = session.ids[1],question.name = question.name) %>% collect()
  df <- merge(x = df, y = new_entry, all = TRUE)
  for(id in session.ids){
    new_entry <- get_participant_textbox_answer(db,session.id = id,question.name = question.name) %>% collect()
    df <- merge(x = df, y = new_entry, all = TRUE)
  }
  df[[column.name]] <- df$val
  df$val <- NULL
  df$question_id <- NULL
  df$name <- NULL
  return(df)
}

#' Get Participant Answer IDs
#'
#' This function extracts a list of a participant's answers ids
#' from the database. The evaluation is lazy and this data can be
#' used as input for further remote computations.
#' 
#' @param db dyplr database handle
#' @param question.name name of question as identified in xml
#' @keywords session, database, lazy, questionnaire
#' @export
#' @return question and answer of participant
#' @examples
#' answer_ids <- get_participant_answers_ids(db,session.id=45)
#' 
#' 
get_participant_answers_ids <- function(db,session.id=45){
  
  user_answers <- db %>% tbl("user_answers") %>% select(session_id, user_answers_id = id)
  store_answers  <- db %>% tbl("store_answers") %>% select(question_id, user_answers_id = user_answer_id, answer_id = id)
  
  answer_ids <- left_join(user_answers,store_answers,by = c("user_answers_id" = "user_answers_id")) %>% 
    filter(session_id==session.id) %>%
    select(session_id,question_id,answer_id)
  
  return(answer_ids)
}