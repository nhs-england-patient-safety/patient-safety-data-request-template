get_code_text <-function(column, code, dataset){
  
  if (dataset=="steis"){
    return(code)
  }else if(dataset=="nrls"){
    code_text_df<-codes |> 
      filter(col_name==column, SASCODE== code) |>
      select(OPTIONTEXT)
  } else if(dataset=="lfpse"){
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
    print(str_glue("{code} was not found in {column} column in lookup table for {dataset}. (or it was found with duplicates)"))
  }
  return(code_text)
}

get_column_text<-function(column, dataset){
  if (dataset=="steis"){
    return(column)
  } else if (dataset=="lfpse"){
    column_text_df<-QuestionReference |> 
      filter(QuestionId==column) |>
      filter(TaxonomyVersion==max(TaxonomyVersion)) |>
      distinct(QuestionId, Property) |>
      select(Property)
  }else if (dataset=="nrls"){
    column_text_df<- nrls_colname_lookup %>% 
      filter(NAME==column) %>% 
      select(LABEL)
  }
  if (nrow(column_text_df) == 1){
    column_new <- pull(column_text_df)
  } else{
    column_new <- column
    print(str_glue("{column} column was not found in lookup table for {dataset}"))
  }
  
  return(column_new)   
}





expand_categorical_filters <- function(string,
                                       dataset) {
  
  #create a list of filters starting with dataset name _filter
  vector_of_filters <-   apropos(str_glue("{dataset}_filter_"))
  list_of_filters <- vector_of_filters %>%
    set_names() %>%
    map(~get(.))

  #manipulate full string - to make later processing simpler
  string_formatted<- string %>%
    str_replace_all( '\"','') %>% #get rid of speech marks
    str_replace_all(" +"," ") %>% #get rid of excess spaces - most important for (" " + col_name + " ") pattern
    str_replace_all("\\( \\+ ","") %>% #replace ( + pattern - important for (" " + col_name + " ") pattern
    str_replace_all(" \\+ \\)","") %>% # replace + ) pattern - important for (" " + col_name + " ") pattern
    str_replace_all("c\\(", "\\(") #remove c from the start of vectors 
  
  #loop through all filters, replacing  filter with full text version of filter
  for (i in list_of_filters) {
   string_formatted<- replace_filter(string_formatted, i, dataset)
  }
  #replace |, & , ==, like and %in% with more understandable phrases
  string_formatted<- string_formatted %>%  
    str_replace_all("\\|", "OR") %>%
    str_replace_all("&", "AND") %>%
    str_replace_all("==", "=") %>%
    str_replace_all("%in%", "IN") %>%
    str_replace_all("%LIKE%", "CONTAINS")
  
  return(string_formatted)
}



replace_filter<-function(string_formatted, i, dataset){
  filter_category<-find_filter_category(i)
  if (filter_category=="in"){
    #split each filter into column, value and operator
    
    column <- as.character(i[2])
    column_new <- get_column_text(column, dataset)  
    value <- i[[3]]
    operator <- as.character(i[1])

    #create vector for value
    value_old <- c()
    value_new <- c()
    
    # get value descriptions
    # loop through each element in the vector (so can handle 1 or c(1,2,3))
    for (j in 1:length(value)) {
      #this is required because value is of the type class when it is in c(1,2,3) form
      if (value[[j]]!="c"){
        j_value<- value[[j]]

        #add old value to vector
        value_old <- append(value_old, j_value)
        #get text for this element of value 
        code_text <- get_code_text(column, j_value, dataset)
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
  }else if (filter_category=="equals"){
    
    column <- as.character(i[2])
    column_new <- get_column_text(column, dataset)  
    value_old <- i[[3]]
    operator <- as.character(i[1])
    
    value_new_string <- get_code_text(column, value_old, dataset)
    #recreate the filter by combining column, operator and value old
    filter_initial <- str_c(column, operator, value_old , sep = " ")
    filter_nice <- str_c(column_new, operator, value_new_string, sep = " ")
  }else if (filter_category=="multi"){
    
    column <- as.character(i[2])%>% 
      str_extract("[a-zA-Z0-9_.-]+")
    column_new <- get_column_text(column, dataset)  
        
    value_old <- i[[3]]
    operator <- as.character(i[1])
    
    value_pretty <- value_old %>% 
      str_replace_all("% ","") %>% #get rid of "% "- which is present with a multi-select column
      str_replace_all(" %", "") #get rid of " %"- which is present with a multi-select column
    value_new_string <- get_code_text(column, value_pretty, dataset)
    #recreate the filter by combining column, operator and value old
    filter_initial <- str_c(column, operator, value_old , sep = " ")
    filter_nice <- str_c(column_new, operator, value_new_string, sep = " ")
  }else if (filter_category=="filter_na"){
    not_na<-as.character(i)[1]=="!"
    column<- as.character(i)[2] %>%
        str_extract_all("[a-zA-Z0-9_-]{3,}")
    column_new <- get_column_text(column , dataset) 
    if (not_na){
      filter_initial <- str_glue("!is.na\\({column}\\)")
      filter_nice <- str_glue("{column_new} IS NOT NA")
    } else {
        filter_initial <- str_glue("([^!]|^)is.na\\({column}\\)")
        filter_nice <- str_glue("{column_new} IS NA")
    }

  } 
  
  if(!is.na(filter_category)){
    string_formatted <-  string_formatted %>% 
      #replace initial filter with the formatted filter
      str_replace_all(filter_initial, filter_nice)

  }
  return(string_formatted)
}


find_filter_category<- function(i){
  
  like_present <-sum(str_detect(as.character(i), "%LIKE%")) == 1
  space_number_space_present <- sum(str_detect(as.character(i), "% \\d+ %")) == 1 
  space_colname_space_present <- sum(str_detect(as.character(i), '" " *\\+ *\\w+ *\\+ *" "')) == 1
  vector_present <- sum(str_detect(as.character(i), "c\\(")) == 1
  in_present <- sum(str_detect(as.character(i), "%in%")) == 1
  equals_present <- sum(str_detect(as.character(i), "==")) == 1

  
  type_na <- sum(str_detect(as.character(i), "na"))

  type_equals <- if_else(equals_present & !vector_present, 1, 0)
  
  type_in <- if_else(vector_present & in_present, 1, 0)

  type_multi <- if_else(like_present & space_number_space_present & space_colname_space_present, 1, 0)
  
  filter_category=NA
  if (type_equals == 1  &  sum(type_in, type_na, type_multi)==0){
    filter_category="equals"
  }
  else if (type_in == 1  &  sum(type_equals, type_na, type_multi)==0){
    filter_category="in"
  }
  else if (type_na == 1  &  sum(type_in, type_equals, type_multi)==0){
    filter_category="filter_na"
  }
  else if (type_multi== 1  &  sum(type_in, type_na, type_equals)==0){
    filter_category="multi"
  } else{
    print("Filter category not found")
  }
  return(filter_category)
}
