patient_measurements <- tibble(CCG = monthly$CCG,
                               avg_Age = rnorm(length(CCG), 65, 2) %>% round(1),
                               men_avg_height = rnorm(monthly$CCG, 175, 1) %>% round(1),
                               women_avg_height = rnorm(monthly$CCG, 162, 1) %>% round(1),
                               men_avg_weight = rnorm(monthly$CCG, 87, 1) %>% round(1),
                               women_avg_weight = rnorm(monthly$CCG, 75, 1) %>% round(1),
                               men_bmi = round(men_avg_weight/((men_avg_height/100)^2), 1),
                               women_bmi = round(women_avg_weight/((women_avg_height/100)^2), 1),
                               bp_sys = rnorm(monthly$CCG, 138, 1) %>% round(),
                               bp_dias = rnorm(monthly$CCG, 88, 1) %>% round(),
                               PLCO = round(rnorm(10, 5, 0.2), 1),
                               Median_IMD_rank = round(rnorm(10, 2700, 50), 0))
                               

for (var in names(x_list)[1:5]) {

  patient_measurements %<>% rowwise() %>% mutate(!!var := x_list[[var]][-1] %>% select(CCG) %>% sum()
                                               )
}

patient_measurements %<>% mutate(no_Men = round(attended*round(rnorm(length(CCG), 0.5, 0.02), 2)),
                                 no_Women = attended-no_Men) %>% select(CCG, avg_Age, no_Men, no_Women, everything())

patient_measurements <- map_dfr(
  patient_measurements$attended %>% set_names(patient_measurements$CCG),
  ~ sample(1:6, .x, T, dgamma(1:6, 3, 2)) %>% tabulate()
) %>% t() %>% `colnames<-`(
  c(
    "Less than 'O' level",
    "'O' level",
    "'A' level",
    "University/college",
    "University degree",
    "Postgraduate/professional"
  )
) %>% as_tibble() %>% bind_cols(patient_measurements, .)

patient_measurements %<>% ungroup() %>% mutate(`Current Smoker` = x_list[["offered_smokecess"]][-1] %>% map_dbl(sum),
                                 `Former Smoker` = attended-`Current Smoker`)

patient_measurements <- patient_measurements %>% mutate(`Duration (years)` = round(rnorm(10, 42, 1), 1),
                                                        `Cigarettes/day` = round(rnorm(10, 23, 2), 1),
                                                        "Pack-years" = round(rnorm(10, 52, 2), 1),
                                                        `FEV~1~` = round(rnorm(10, 2.1, 0.08), 2),
                                                        "% Predicted FEV~1~" = round(rnorm(10, 81, 1), 1),
                                                        FVC = round(rnorm(10, 3, 0.2), 1),
                                                        "% Predicted FVC" = round(rnorm(10, 98, 2), 1),
                                                        `FEV~1~:FVC ratio` = round((`FEV~1~`/FVC)*100, 1),
                                                        `Airflow obstruction, yes (%)` = round(attended*rnorm(1, 0.5, 0.03), 0),
                                                        `COPD/emphysema, yes (%)` = round(attended*rnorm(1, 0.3, 0.03)),
                                                        `FH lung cancer, yes (%)` = round(attended*rnorm(1, 0.26, 0.03)))

patient_measurements <- map2_dfr(
  patient_measurements$attended %>% set_names(patient_measurements$CCG),
  c(2.1,2.2,2.5,1.9,2.05,2.023,2.3,2.29,2, 2.1),
  ~ sample(1:5, .x, T, dgamma(1:5, 3, .y)) %>% tabulate()
) %>% t() %>% `colnames<-`(
  c(
    "MRC_DS_1",
    "MRC_DS_2",
    "MRC_DS_3",
    "MRC_DS_4",
    "MRC_DS_5"
  )
) %>% as_tibble() %>% bind_cols(patient_measurements, .)

patient_measurements <- map2_dfr(
  patient_measurements$attended %>% set_names(patient_measurements$CCG),
  c(rnorm(10, 2.5, 0.2)),
  ~ sample(1:5, .x, T, dgamma(1:5, 3, .y)) %>% tabulate()
) %>% t() %>% `colnames<-`(
  c(
    "PS_0",
    "PS_1",
    "PS_2",
    "PS_3",
    "PS_4"
  )
) %>% as_tibble() %>% bind_cols(patient_measurements, .)

patient_measurements %>% filter(CCG == "Mansfield and Ashfield with Corby") %>% select(-ends_with("avg_height"), -ends_with("avg_weight"), -starts_with("bp")) -> test
  test[,-1] %>% pivot_longer(cols = everything(), names_to = "Variable", values_to = "Attended Patients")
  