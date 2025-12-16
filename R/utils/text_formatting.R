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