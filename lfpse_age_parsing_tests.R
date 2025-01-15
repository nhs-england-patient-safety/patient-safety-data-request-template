lfpse_ages <- lfpse_parsed %>%
  select(P004, P007, RecordingRoute, TaxonomyVersion) %>%
  collect()

lfpse_age_parsed <- lfpse_parsed %>%
  mutate(P004 = as.numeric(P004)) %>%
  select(Reference, Revision, P004, P007, RecordingRoute) %>% 
  collect() %>%
  mutate(age_unit = case_when(
    is.na(P004) ~ 'age missing',
         between(P004, 1, 30) ~ 'days',
         between(P004, 31, 341) ~ 'months',
         between(P004, 372, 74028) ~ 'years',
    .default = 'age outside bounds')) %>%
  mutate(age_compliance = case_when(
    age_unit == 'age outside bounds' ~ 'age outside bounds',
    age_unit == 'age missing' ~ 'age missing',
    age_unit == 'days' & between(P004, 0, 30) ~ 'yes',
    age_unit == 'months' & P004 %% 31 == 0 ~ 'yes',
    age_unit == 'years' & P004 %% 372 == 0 ~ 'yes',
    .default = 'no'))

lfpse_age_parsed %>%
  group_by(RecordingRoute, age_unit, age_compliance) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = RecordingRoute,
              values_from = n) %>%
  adorn_totals('both') %>%
  write_csv('summary.csv')


lfpse_oir_non_compliant <- lfpse_age_parsed %>%
  filter(RecordingRoute == 'OIR') %>%
  filter(age_compliance == 'no' | age_compliance == 'age outside bounds')

write_csv(lfpse_oir_non_compliant, 'lfpse_oir_non_compliant_ages.csv')

refs <- lfpse_parsed %>% 
  filter(Reference %in% lfpse_oir_non_compliant$Reference) %>%
  filter(Revision == 1) %>%
  collect() 

lfpse_parsed %>%
  filter(Reference %in% refs$Reference) %>%
  filter(Revision == 2) %>%
  distinct(RecordingRoute.y)

  
lfpse_oir_years_invalid <- lfpse_parsed %>% 
  filter(RecordingRoute == 'API',
         between(as.numeric(P004), 372, 74028)) %>%
  collect()

lfpse_oir_years_invalid$P004 <- as.numeric(lfpse_oir_years_invalid$P004)

lfpse_oir_years_invalid %>%
  select(Reference, Revision, P004, P007) %>%
  mutate(div = P004 %% 372) %>%
  filter(div != 0) %>%
  group_by(Reference, Revision) 

table <- lfpse_age_parsed %>%
  group_by(age_compliance, age_bracket, RecordingRoute) %>%
  summarise(n=n())

table %>%
  pivot_wider(names_from = RecordingRoute, 
              values_from = n) %>%
  ungroup() %>%
  adorn_totals()

lfpse_age_parsed %>%
  filter(age_compliance == 'no',
         RecordingRoute == 'API',
         age_bracket == 'years')
