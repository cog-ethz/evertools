#' Get Participant Boolean Answer
#'
#' This function extracts a participant's answer from
#' the database. The evaluation is lazy and this data can be
#' used as input for further remote computations.
#' 
#' Note: Does not work with text answers
#' @param db dyplr database handle
#' @param session.id session from which to load data
#' @param question.name name of question as identified in xml
#' @keywords session, database, lazy, questionnaire
#' @export
#' @return question and answer of participant
#' @examples
#' question <- get_participants_answer(db,session.id = 45,question.name="socDem1")
#' 
get_participant_bool_answers <-function(db,session.id=45,question.name="socDem1"){
  
  #Get tables
  answer_ids <- get_participant_answers_ids(db,session.id=session.id)
  answers_stored_bools <- db %>% tbl("answers_stored_bools")
  questions <- db %>% tbl("questions") %>% filter(name == question.name)
  questions_stored_strings <- db %>% tbl("questions_stored_strings")
  store_strings <- db %>% tbl("store_strings")
  store_bools <- db %>% tbl("store_bools")
  
  #Get Label
  question_options <- left_join(questions,questions_stored_strings,by = c("id" = "questions_id"))
  question_options_text <-  left_join(question_options,store_strings%>% rename(string_id =id),c("string_id" = "string_id"))%>%
    select(pos,label = val)
  
  #Get Value
  bool_ids <- left_join(answer_ids,answers_stored_bools,by = c("answer_id" = "answer_id")) %>%
    select(answer_id,session_id,question_id,bool_id)
  bool_answers_ids <- left_join(questions %>% rename(question_id =id),bool_ids,c("question_id" = "question_id"))
  bool_answers <- left_join(bool_answers_ids,store_bools %>% rename(bool_id =id),c("bool_id" = "bool_id")) 
  
  result <- left_join(bool_answers,question_options_text,c("pos" = "pos")) %>%
    select(session_id,question_id,name,label,val)
  
  return(result)
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