date_filter <- if (date_type == 'occurring') {
  expr(occurred_date)
} else if (date_type == 'reported') {
  expr(reported_date)
}