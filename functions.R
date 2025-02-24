# function to get find the label for a column value from the column name, code and database name
get_code_text <-function(column, code, database_name){
  
  if (database_name=="steis"){
    return(code)
  }else if(database_name=="nrls"){
    code_text_df<-codes |> 
      filter(col_name==column, SASCODE== code) |>
      select(OPTIONTEXT)
  } else if(database_name=="lfpse"){
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
    print(str_glue("{code} was not found in {column} column in lookup table for {database_name}. (or it was found with duplicates)"))
  }
  return(code_text)
}

# function to get find the column label for a column from the column name and database name
get_column_text<-function(column, database_name){
  if (database_name=="steis"){
    return(column)
  } else if (database_name=="lfpse"){
    column_text_df<-QuestionReference |> 
      filter(QuestionId==column) |>
      filter(TaxonomyVersion==max(TaxonomyVersion)) |>
      distinct(QuestionId, Property) |>
      select(Property)
  }else if (database_name=="nrls"){
    column_text_df<- nrls_colname_lookup %>% 
      filter(NAME==column) %>% 
      select(LABEL)
  }
  if (nrow(column_text_df) == 1){
    column_new <- pull(column_text_df)
  } else{
    column_new <- column
    print(str_glue("{column} column was not found in lookup table for {database_name}"))
  }
  
  return(column_new)   
}

#function to identify the variable type from the features it includes
find_filter_category<- function(i){
  
  like_present <-sum(str_detect(i, "%LIKE%")) == 1
  space_number_space_present <- sum(str_detect(i, "% \\d+ %")) == 1 
  space_colname_space_present <- sum(str_detect(i, '# # *% *\\w* *% *# #')) == 1
  vector_present <- sum(str_detect(as.character(i), "c~")) == 1
  in_present <- sum(str_detect(as.character(i), "%in%")) == 1
  equals_present <- sum(str_detect(as.character(i), "==|!=")) == 1
  
  
  type_na <- sum(str_detect(i, "na"))
  
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

#function to translate a filter into a more human readable value, given the filter string and data
translate_individual_filter <- function(one_filter, database_name){
  
  filter_category<-find_filter_category(one_filter) # need to test rest of this
  
  #need to test and change all of below
  if (filter_category=="in"){
    #split each filter into column, value and operator
    
    column <- str_trim(str_split(one_filter,"%in%")[[1]][1])
    column_new <- get_column_text(column, database_name)  
    value_old <- str_trim(str_split(one_filter,"%in%")[[1]][2])
    value_old_no_spaces <- str_replace(value_old," ", "")
    operator <- "IN"
    
    #create vector for value
    value_new <- c()
    
    # get value descriptions
    # loop through each element in the vector (so can handle 1 or c(1,2,3))
    for (j in str_split(value_old_no_spaces,"")[[1]]) {
      #this is required because value is of the type class when it is in c(1,2,3) form
      if (! j %in% c("c", ",", " ", "~")){

        #get text for this element of value 
        code_text <- get_code_text(column, j, database_name)
        #append this value to a vector of values
        value_new <- append(value_new, code_text)
      }
    }
    
    #convert from vector to string
    value_new_string <- str_c(value_new, collapse = ", ")
    
    # add brackets to either side of value_new_string- helpful for clarity
    value_new_string <- str_c("(", value_new_string, ")")
    
    #recreate the filter by combining column, operator and value old
    filter_nice <- str_c(column_new, operator, value_new_string, sep = " ")
  
    } else if (filter_category=="equals"){
    
    column <- str_trim(str_split(one_filter,"==")[[1]][1])
    column_new <- get_column_text(column, database_name)  
    value_old <-str_trim(str_split(one_filter,"==")[[1]][2])
    operator <- "=="
    
    
    value_new_string <- get_code_text(column, value_old, database_name)

    filter_nice <- str_c(column_new, operator, value_new_string, sep = " ")
    
    
  }else if (filter_category=="multi"){
    column <- one_filter%>% 
      str_extract("[a-zA-Z0-9_.-]+")
    column_new <- get_column_text(column, database_name)  
    value_old <- str_extract(one_filter,"% \\d %")
    operator <- "CONTAINS"
    
    value_pretty <- value_old %>% 
      str_replace_all("%","") %>% #get rid of "% "- which is present with a multi-select column
      str_trim()
    value_new_string <- get_code_text(column, value_pretty, database_name)
    #recreate the filter by combining column, operator and value old
    filter_nice <- str_c(column_new, operator, value_new_string, sep = " ")
  }else if (filter_category=="filter_na"){
    not_na<-!str_detect(one_filter,"!")
    column<- str_extract(one_filter,"[a-zA-Z0-9_-]{3,}")
    column_new <- get_column_text(column , database_name) 
    if (not_na){
      filter_nice <- str_glue("{column_new} IS NOT NA")
    } else {
      filter_nice <- str_glue("{column_new} IS NA")
    }
    
  } 
  return(filter_nice)
}

#tranlate a subset of a filter- specifically, a section that occurs within brackets, seperated by | or &
translate_filter_subset<- function(filter_subset, database_name){
  
  #replace the brackets at start and end of the subset
  filter_subset<- filter_subset %>%
    str_replace_all("^~","") %>%
    str_replace_all("~$","")
  
  # split into each filter using | and & 
  filter_vector<- str_split(filter_subset, "\\&|\\|")[[1]]
  
  # loop through filters in vectors, translating them
  for (one_filter in filter_vector){
    
    one_filter_translated<- translate_individual_filter(one_filter, database_name)
    
    #replace the old filter with the translated filter 
    filter_subset<-str_replace_all(filter_subset,
                                   one_filter, 
                                   one_filter_translated) 
  }
  
  #replace the symbols with AND and OR
  filter_subset <- filter_subset %>%
    str_replace_all("\\&", " AND ") %>%
    str_replace_all("\\£", " OR ")
  
  return(filter_subset)
}



#function to translate a categorical filter (as an expression object) into a mure human readable string given a database name
translate_categorical_string<- function(categorical_filter, database_name){

  #turn the categorical filter into a string
  categorical_filter_string <- deparse(categorical_filter, width.cutoff = 500)

  #the maximum width cutoff for deparse is 500- 
  #for strings above this length, it will automatically split up the string into a vector.
  #if this happens, it may break up filters, so we need to combine the vectors back into one string.
  if (length(categorical_filter_string)> 1){
    categorical_filter_string<- str_c(categorical_filter_string, collapse = "")
  }
  
  # brackets, plus symbols and speech marks are regex special characters and working with them is awkward
  # so we replace them with non protected characters 
  categorical_filter_copy<- categorical_filter_string %>%
    str_replace_all("\\(", "~") %>% # replace brackets with ~
    str_replace_all("\\)", "~") %>% # replace brackets with ~
    str_replace_all('\\"', "#") %>% # replace speech marks with #
    str_replace_all('\\+', "%") %>% # replace + with %
    str_replace_all('\\|', "£") # replace | with £
  
  #if the string contains and  or  or- split by brackets, we'll need to seperate and loop through  
  categorical_filter_copy_split <- str_split(categorical_filter_copy, "~ *(£|&) *~")[[1]]
  #pull out whether split by and or or 
  location_of_bracket_breaks<-str_locate_all(categorical_filter_copy, "~ *(£|&) *~")[[1]]
  
  #create empty result string
  result_string<- ""  
  
  #loop through vector of subsets
  for (filter_number in 1:length(categorical_filter_copy_split)){
   
    #translate subset
    filter_subset<- translate_filter_subset(categorical_filter_copy_split[filter_number], database_name)
    
    #put and/ or back in
    if (filter_number<length(categorical_filter_copy_split) ){
      
      symbol <- categorical_filter_copy %>%
        str_sub(start = location_of_bracket_breaks[filter_number,1], 
                end=location_of_bracket_breaks[filter_number,2]) %>%
        str_extract("(&|£)") %>%
        str_replace_all("\\&", " AND ") %>%
        str_replace_all("\\£", " OR ")
    } else{
      symbol <- ""
    }
    result_string<- str_c(result_string, "(",filter_subset ,")", symbol, collapse = "")
    
    }
return(result_string)
}


lfpse_categorical<- expr(
  (A001 == 3) & (A008 %in% c(1,2) & is.na(A001) & !is.na(A001) & ' ' + A001 +  ' ' %LIKE% '% 3 %') |  (!is.na(A001)) )

translate_categorical_string(lfpse_categorical, "lfpse")

