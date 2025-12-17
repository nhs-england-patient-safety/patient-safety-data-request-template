# Text Formatting Utilities


#' Format regex term for display
#' 
#' @param term Regex pattern string (e.g., "(?i)\\bparacetamol")
#' 
#' @return Formatted string with regex special characters replaced by readable symbols

make_text_terms_pretty <- function(term){
  term |>
    str_replace_all(pattern = fixed("(?:\\W|)"), "~") |>
    str_replace_all(pattern = "\\|", " OR ") |>
    str_replace_all(pattern = fixed('\\b'), "%" ) |>
    str_replace_all(pattern = fixed('(?i)'), "" ) |>
    str_replace_all("term_", "term: ") |>
    str_replace_all("group_", "Group ") |>
    str_replace_all("_", " ")
}
