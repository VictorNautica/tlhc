moo_complete <-
  patient_measurements %>% select(
    -ends_with("avg_height"),
    -ends_with("avg_weight"),
    -starts_with("bp"),
    -invited,
    -accepted,
    -starts_with("ldct"),
  ) %>% select("CCG",
               "attended",
               "avg_Age",
               "no_Men",
               "no_Women",
               "Median_IMD_rank",
               everything())

attended_total <- sum(moo_complete$attended)

agg_one <- moo_complete %>% summarise_at(vars("no_Men", "no_Women", "Less than 'O' level", "'O' level", "'A' level", "University/college", 
                                "University degree", "Postgraduate/professional", "Current Smoker", 
                                "Former Smoker", "Airflow obstruction, yes (%)", "COPD/emphysema, yes (%)", 
                                  "FH lung cancer, yes (%)", "MRC_DS_1", "MRC_DS_2", "MRC_DS_3", 
                                  "MRC_DS_4", "MRC_DS_5", "PS_0", "PS_1", "PS_2", "PS_3", "PS_4"
), sum)
agg_two <- moo_complete %>% summarise_at(vars("avg_Age", "men_bmi", "women_bmi", "PLCO", "Duration (years)",
                                  "Cigarettes/day", "Pack-years", "FEV~1~", "% Predicted FEV~1~",
                                  "FVC", "% Predicted FVC", "FEV~1~:FVC ratio"),
                             function(x) round(sum(x*(moo_complete$attended %>% prop.table())), 1))


moo_complete <- bind_cols(agg_one, agg_two) %>% 
  pivot_longer(cols = everything(),
               names_to = "Variable",
               values_to = "Programme Average (Weighted Mean/%)")

moo_complete[moo_complete$Variable == "attended", "Variable"] <- "No of attendees"
moo_complete[moo_complete$Variable == "avg_Age", "Variable"] <- "Mean age"
moo_complete[moo_complete$Variable == "Median_IMD_rank", "Variable"] <- "Median IMD rank"
moo_complete[moo_complete$Variable == "no_Men", "Variable"] <- "No of men"
moo_complete[moo_complete$Variable == "no_Women", "Variable"] <- "No of women"
moo_complete[moo_complete$Variable == "men_bmi", "Variable"] <- "BMI of men"
moo_complete[moo_complete$Variable == "women_bmi", "Variable"] <- "BMI of women"
moo_complete[moo_complete$Variable == "PLCO", "Variable"] <- "Lung cancer risk (PLCO~M2012~)"

moo_complete <- moo_complete %>% mutate(
  groupings = case_when(
    Variable %in% c("No of men", "No of women") ~ "Sex",
    Variable %in% c(
      "Less than 'O' level",
      "'O' level",
      "'A' level",
      "University/college",
      "University degree",
      "Postgraduate/professional"
    ) ~ "Education",
    Variable %in% c("Current Smoker", "Former Smoker") ~ "SmokingStatus",
    Variable %in% c("MRC_DS_1", "MRC_DS_2", "MRC_DS_3", "MRC_DS_4", "MRC_DS_5") ~ "MRCDyspnoeaScore",
    Variable %in% c("PS_0", "PS_1", "PS_2", "PS_3", "PS_4") ~ "PerformanceStatus",
    TRUE ~ Variable
  )
) %>% group_by(groupings) %>% mutate(pct = case_when(
  n() > 1 ~ round(`Programme Average (Weighted Mean/%)` / sum(`Programme Average (Weighted Mean/%)`) * 100, 1),
  Variable %in% c(
    "Airflow obstruction, yes (%)",
    "COPD/emphysema, yes (%)",
    "FH lung cancer, yes (%)"
  ) ~ round(`Programme Average (Weighted Mean/%)` / attended_total * 100, 1),
  TRUE ~ NA_real_
)) ## create pct

moo_complete <- moo_complete %>% mutate_at(vars("Programme Average (Weighted Mean/%)"), prettyNum) %>% 
  mutate_at(vars("pct"), as.character) %>% 
  mutate_at(vars("pct"), ~str_c("(", ., ")")) %>% 
  mutate(`Programme Average (Weighted Mean/%)` = coalesce(pct, `Programme Average (Weighted Mean/%)`)) %>%
  select(-pct) %>% 
  mutate_at(vars("Programme Average (Weighted Mean/%)"), str_replace_all, pattern = " NA", replacement = "") %>% 
  ungroup %>% select(-groupings) ## mutate pcts for relevant grouped vars