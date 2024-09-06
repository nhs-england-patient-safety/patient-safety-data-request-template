get_code_text <-function(column, code, database){
  
  if (database=="STEIS"){
    return(code)
  }else if(database=="NRLS"){
    code_text_df<-codes |> 
      filter(col_name==column, SASCODE== code) |>
      select(OPTIONTEXT)
  } else if(database=="LFPSE"){
    code_text_df<-ResponseReference |> 
      filter(QuestionId==column, ResponseCode==code) |>
      filter(TaxonomyVersion==max(TaxonomyVersion)) |>
      select(ResponseText) %>%
      distinct(ResponseText)
  }
  if (nrow(code_text_df) == 1) {
    code_text <- pull(code_text_df)
  } else {
    code_text<-  code
    print(str_glue("{code} was not found in {column} column in lookup table for {database}. (or it was found with duplicates)"))
  }
  return(code_text)
}

get_column_text<-function(column, database){
  if (database=="STEIS"){
    return(column)
  } else if (database=="LFPSE"){
    column_text_df<-QuestionReference |> 
      filter(QuestionId==column) |>
      filter(TaxonomyVersion==max(TaxonomyVersion)) |>
      distinct(QuestionId, Property) |>
      select(Property)
  }else if (database=="NRLS"){
    column_text_df<- nrls_colname_lookup %>% 
      filter(NAME==column) %>% 
      select(LABEL)
  }
  if (nrow(column_text_df) == 1){
    column_new <- pull(column_text_df)
  } else{
    column_new <- column
    print(str_glue("{column} column was not found in lookup table for {database}"))
  }
  
  return(column_new)   
}





expand_categorical_filters <- function(string,
                                       list_of_filters,
                                       database) {
  
  
  string_formatted<-string
  string_formatted<- str_replace_all(string_formatted,'\"','')
  #loop through all filters, replacing codes with text
  for (i in list_of_filters) {
    #ignore empty filters
    if (i != quote(1 == 1)) {
      #split each filter into column, value and operator
      column <- as.character(i[2])
      value <- i[[3]]
      operator <- as.character(i[1])
      
      #create vector for value
      value_old <- c()
      value_new <- c()
      
      #get column name
      column_new <- get_column_text(column, database)
      
      # get value descriptions
      # loop through each element in the vector (so can handle 1 or c(1,2,3))
      for (j in 1:length(value)) {
        #this is required because value is of the type class when it is in c(1,2,3) form
        if (value[[j]]!="c"){
          value_old <- append(value_old, value[[j]])
          #get text for this element of value 
          code_text <- get_code_text(column, value[[j]], database)
          #append this value to a vector of values
          value_new <- append(value_new, code_text)
        }
      }
      
      #convert from vector to string
      value_old <- str_c(value_old, collapse = ", ")
      value_new_string <- str_c(value_new, collapse = ", ")
      
      #formatting is a little different when value has one or more elements
      if (length(value_new) > 1) {
        # add brackets to either side of value_old - to allow matching to occur
        value_old <- str_c("\\(", value_old, "\\)")
        # add brackets to either side of value_new_string- helpful for clarity
        value_new_string <- str_c("(", value_new_string, ")")
      }
      
      #recreate the filter by combining column, operator and value old
      filter_initial <- str_c(column, operator, value_old , sep = " ")
      filter_nice <- str_c(column_new, operator, value_new_string, sep = " ")
      
      # remove c in front of brackets
      string_formatted <-  string_formatted %>% 
        str_replace_all("c\\(", "\\(") %>%
        #replace initial filter with the formatted filter
        str_replace_all(filter_initial, filter_nice)
    }
  }
  
  #replace |, & , == and %in% with more understandable phrases
  string_formatted<- string_formatted %>%  
    str_replace_all("\\|", "OR") %>%
    str_replace_all("&", "AND") %>%
    str_replace_all("==", "=") %>%
    str_replace_all("%in%", "IN")
  
  return(string_formatted)
}