
expanded_categorical_filter_lfpse<-translate_categorical_string(lfpse_categorical, "lfpse")
expanded_categorical_filter_nrls<-translate_categorical_string(nrls_categorical, "nrls")
expanded_categorical_filter_steis<-translate_categorical_string(steis_categorical, "steis")
message(str_glue("LFPSE filter is: \n{expanded_categorical_filter_lfpse}"))
message(str_glue("NRLS filter is: \n{expanded_categorical_filter_nrls}"))
message(str_glue("StEIS filter is: \n{expanded_categorical_filter_steis}"))
