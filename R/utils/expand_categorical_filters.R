lfpse_categorical <- combine_lfpse_filters(
  lfpse_categorical_incident_level,
  lfpse_categorical_patient_level
)

expanded_categorical_filter_lfpse <- translate_categorical_string(
  lfpse_categorical,
  "lfpse"
)
expanded_categorical_filter_nrls <- translate_categorical_string(
  nrls_categorical,
  "nrls"
)
expanded_categorical_filter_steis <- translate_categorical_string(
  steis_categorical,
  "steis"
)
