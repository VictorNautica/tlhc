library(tidyverse)
library(readxl)
library(patchwork)

source("C:/Users/victor.yu/OneDrive - Midlands and Lancashire CSU/TLHC random/sql pull script.R")

#### Incidence ####

incidence <- read_csv("data/incidence_age_standardised_rates_20200506T135748.csv")

#### Codes

## Mansfield and Ashfield - 04E and Corby - 03V
## Blackburn with Darwen - 00Q and Liverpool - 99A
## Doncaster - 02X
## Hull - 03F
## Knowlsey - 01J and Halton - 01F
## Newcastle Gateshead - 13T
## North Kirklees - 03J
## Southampton - 10X
## Tameside Glossop - 01Y
## Thurrock - 07G and Luton - 06P

#### Tidy initial
  ccg_codes <- c("Mansfield and Ashfield" = "04E",
                 "Corby" = "03V", 
                 "Blackburn with Darwen" = "00Q", 
                 "Blackpool" = "99A", 
                 "Doncaster" = "02X", 
                 "Hull" = "03F", 
                 "Knowsley" = "01J", 
                 "Halton" = "01F",
                 "Newcastle Gateshead" = "13T", 
                 "North Kirklees" = "03J", 
                 "Southampton" = "10X", 
                 "Tameside and Glossop" = "01Y", 
                 "Thurrock" = "07G", 
                 "Luton" = "06P")

incidence_plot <- function(ccg_name) {
  
  
  incidence <- incidence %>% filter(HealthGeographyCode %in% c(ccg_codes, 921))
  
  if (ccg_name == "Mansfield and Ashfield with Corby") {
    selected_ccg_code <- c("04E", "03E")
  } else if (ccg_name == "Blackburn with Darwen with Blackpool") {
    selected_ccg_code <- c("00Q", "99A")
  } else if (ccg_name == "Knowsley with Halton") {
    selected_ccg_code <- c("01J", "01F")
  } else if (ccg_name == "Thurrock and Luton") {
    selected_ccg_code <- c("07G", "06P")
  } else {
    selected_ccg_code <- ccg_codes[which(names(ccg_codes) == ccg_name)]
  }
  
  
  incidence <- incidence %>% mutate(
    TLHC_CCG = case_when(
      HealthGeographyCode %in% c(
        ccg_codes[which(!ccg_codes %in% selected_ccg_code)]) ~ "Other TLHC",
      HealthGeographyCode %in% selected_ccg_code ~ ccg_name,
      HealthGeographyCode == 921 ~ "National",
      TRUE ~ "Other"
    )
  ) 
  incidence <- incidence %>% mutate_at(vars("TLHC_CCG"), as_factor)
  incidence <- incidence %>% mutate_at(vars("TLHC_CCG"), fct_relevel, "Other TLHC", "National", ccg_name)
  incidence <- incidence %>% arrange(TLHC_CCG)
  
  incidence_list <- list("55-59" = NA,
                         "60-64" = NA,
                         "65-69" = NA,
                         "70-74" = NA)
  
  
  incidence_list <- imap(
    incidence_list, ~ {
      incidence %>% filter(AgeGroup == .y) %>% ggplot(
        aes(
          Year,
          AgeStandardisedRate,
          group = HealthGeographyCode,
          colour = TLHC_CCG,
          alpha = TLHC_CCG
        )) +
        geom_line(size = 1.1) +
        geom_point() +
        labs(y = "Standardised Rate\nper 100,000") +
        scale_colour_manual(values = c("lightgrey", "#377eb8", "#e41a1c")) +
        scale_alpha_manual(values = c(0.5, 0.75, 1)) +
        theme(legend.position = "top", 
              legend.title = element_blank()) +
        labs(title = .y)
    }
  )
  
  incidence_list[["55-59"]] + incidence_list[["60-64"]] + incidence_list[["65-69"]] + incidence_list[["70-74"]] + plot_layout(guides = "collect") & theme(legend.position = "bottom")
}

#### Mortality 55-74x ####

mortality <- read_csv("data/mortality_age_standardised_rates_20200430T173647.csv")

mortality_plot <- function(ccg_name) {
  
  mortality <- mortality %>% filter(HealthGeographyCode %in% c(ccg_codes, 921))
  
  if (ccg_name == "Mansfield and Ashfield with Corby") {
    selected_ccg_code <- c("04E", "03E")
  } else if (ccg_name == "Blackburn with Darwen with Blackpool") {
    selected_ccg_code <- c("00Q", "99A")
  } else if (ccg_name == "Knowsley with Halton") {
    selected_ccg_code <- c("01J", "01F")
  } else if (ccg_name == "Thurrock and Luton") {
    selected_ccg_code <- c("07G", "06P")
  } else {
    selected_ccg_code <- ccg_codes[which(names(ccg_codes) == ccg_name)]
  }
  
  
  mortality <- mortality %>% mutate(
    TLHC_CCG = case_when(
      HealthGeographyCode %in% c(
        ccg_codes[which(!ccg_codes %in% selected_ccg_code)]) ~ "Other TLHC",
      HealthGeographyCode %in% selected_ccg_code ~ ccg_name,
      HealthGeographyCode == 921 ~ "National",
      TRUE ~ "Other"
    )
  ) 
mortality <- mortality %>% mutate_at(vars("TLHC_CCG"), as_factor)
mortality <- mortality %>% mutate_at(vars("TLHC_CCG"), fct_relevel, "Other TLHC", "National", ccg_name)
mortality <- mortality %>% arrange(TLHC_CCG)

mortality_list <- list("55-59" = NA,
                       "60-64" = NA,
                       "65-69" = NA,
                       "70-74" = NA)


mortality_list <- imap(
  mortality_list, ~ {
  mortality %>% filter(AgeGroup == .y) %>% ggplot(
  aes(
    Year,
    AgeStandardisedRate,
    group = HealthGeographyCode,
    colour = TLHC_CCG,
    alpha = TLHC_CCG
  )) +
  geom_line(size = 1.1) +
  geom_point() +
  labs(y = "Standardised Rate\nper 100,000") +
  scale_colour_manual(values = c("lightgrey", "#377eb8", "#e41a1c")) +
  scale_alpha_manual(values = c(0.5, 0.75, 1)) +
  theme(legend.position = "top", 
        legend.title = element_blank()) +
  labs(title = .y)
  }
)

mortality_list[["55-59"]] + mortality_list[["60-64"]] + mortality_list[["65-69"]] + mortality_list[["70-74"]] + plot_layout(guides = "collect") & theme(legend.position = "bottom")
}

## NCRAS Valid Stage (All Cancers) Proportion of Tumours diagnosed as early syage

ncras_stage <- read_csv("data/NCRAS - Stage at Diagnosis by financial year 2011-Q4-2018-Q1.csv .csv")


relevant_ccgs_and_england <- c("England",
                               "NHS Mansfield and Ashfield CCG",
                               "NHS Corby CCG",
                               "NHS Blackburn with Darwen CCG",
                               "NHS Blackpool CCG",
                               "NHS Doncaster CCG",
                               "NHS Hull CCG",
                               "NHS Knowsley CCG",
                               "NHS Halton CCG",
                               "NHS Newcastle Gateshead CCG",
                               "NHS North Kirklees CCG",
                               "NHS Southampton CCG",
                               "NHS Tameside and Glossop CCG",
                               "NHS Thurrock CCG",
                               "NHS Luton CCG")

ncras_stage <- ncras_stage %>% filter(CCG %in% relevant_ccgs_and_england)

stage_function <- function(ccg_name) {

if (ccg_name == "Mansfield and Ashfield with Corby") {
  selected_ccg <- c("NHS Mansfield and Ashfield CCG", "NHS Corby CCG	")
} else if (ccg_name == "Blackburn with Darwen with Blackpool") {
  selected_ccg <- c("NHS Blackburn with Darwen CCG", "NHS Blackpool CCG")
} else if (ccg_name == "Knowsley with Halton") {
  selected_ccg <- c("NHS Knowsley CCG", "NHS Halton CCG")
} else if (ccg_name == "Thurrock and Luton") {
  selected_ccg <- c("NHS Thurrock CCG", "NHS Luton CCG")
} else {
  selected_ccg <- paste("NHS", ccg_name, "CCG")
}

ncras_stage <- ncras_stage %>% mutate(
  TLHC_CCG = case_when(
    CCG %in% relevant_ccgs_and_england[which(!relevant_ccgs_and_england %in% c("England", selected_ccg))] ~ "Other TLHC",
    CCG %in% selected_ccg ~ ccg_name,
    CCG == "England" ~ "National",
    TRUE ~ "Other"
  )
) 

ncras_stage <- ncras_stage %>% mutate_at(vars("TLHC_CCG"), as_factor)
ncras_stage <- ncras_stage %>% mutate_at(vars("TLHC_CCG"), fct_relevel, "Other TLHC", "National", ccg_name)
ncras_stage <- ncras_stage %>% arrange(TLHC_CCG)

ncras_stage %>% ggplot(aes(`Financial Year and Quarter`, `Quarterly Proportion (%)`, group = CCG, colour = TLHC_CCG, alpha = TLHC_CCG)) + 
  geom_point() +
  geom_line() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_colour_manual(values = c("lightgrey", "#377eb8", "#e41a1c")) +
  scale_alpha_manual(values = c(0.5, 0.75, 1)) +
  theme(legend.position = "top", 
        legend.title = element_blank())

}

## Valid Stage Lung Cancer at Decision to Treat

validstage_dtt <- pull_from_sql("CCG_OIS", "Record_Of_Lung_Cancer_Stage_At_Decision_To_Treat1")

validstage_dtt <- validstage_dtt %>% filter(Level %in% c("National", ccg_codes))

validstage_dtt_function <- function(ccg_name) {
if (ccg_name == "Mansfield and Ashfield with Corby") {
  selected_ccg_code <- c("04E", "03E")
} else if (ccg_name == "Blackburn with Darwen with Blackpool") {
  selected_ccg_code <- c("00Q", "99A")
} else if (ccg_name == "Knowsley with Halton") {
  selected_ccg_code <- c("01J", "01F")
} else if (ccg_name == "Thurrock and Luton") {
  selected_ccg_code <- c("07G", "06P")
} else {
  selected_ccg_code <- ccg_codes[which(names(ccg_codes) == ccg_name)]
}


validstage_dtt <- validstage_dtt %>% mutate(
  TLHC_CCG = case_when(
    Level %in% c(
      ccg_codes[which(!ccg_codes %in% selected_ccg_code)]) ~ "Other TLHC",
    Level %in% selected_ccg_code ~ ccg_name,
    Level == "National" ~ "National",
    TRUE ~ "Other"
  )
)

validstage_dtt <- validstage_dtt %>% mutate_at(vars("TLHC_CCG"), as_factor)
validstage_dtt <- validstage_dtt %>% mutate_at(vars("TLHC_CCG"), fct_relevel, "Other TLHC", "National", ccg_name)
validstage_dtt <- validstage_dtt %>% arrange(TLHC_CCG)

validstage_dtt %>% ggplot(aes(Reporting_Period, Indicator_Value, group = Level, colour = TLHC_CCG, alpha = TLHC_CCG)) + 
  geom_point() +
  geom_line() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_colour_manual(values = c("lightgrey", "#377eb8", "#e41a1c")) +
  scale_alpha_manual(values = c(0.5, 0.75, 1)) +
  theme(legend.position = "top", 
        legend.title = element_blank())
}

## 1 year survival

one_yr_survival <- read_excel("data/Data_Tables_IndexofCancerSurvival_2002_2017.xlsx", 
                                                          sheet = "Data_Complete")

one_yr_survival <-
  one_yr_survival %>% filter(
    `Geography type` %in% c("country", "CCG"),
    `Cancer site` == "Lung",
    Sex == "Persons",
    `Geography name` %in% relevant_ccgs_and_england, 
    `Years since diagnosis` == 1
  )

survival_func <- function(ccg_name) {
if (ccg_name == "Mansfield and Ashfield with Corby") {
  selected_ccg <- c("NHS Mansfield and Ashfield CCG", "NHS Corby CCG	")
} else if (ccg_name == "Blackburn with Darwen with Blackpool") {
  selected_ccg <- c("NHS Blackburn with Darwen CCG", "NHS Blackpool CCG")
} else if (ccg_name == "Knowsley with Halton") {
  selected_ccg <- c("NHS Knowsley CCG", "NHS Halton CCG")
} else if (ccg_name == "Thurrock and Luton") {
  selected_ccg <- c("NHS Thurrock CCG", "NHS Luton CCG")
} else {
  selected_ccg <- paste("NHS", ccg_name, "CCG")
}

one_yr_survival <- one_yr_survival %>% mutate(
  TLHC_CCG = case_when(
    `Geography name` %in% relevant_ccgs_and_england[which(!relevant_ccgs_and_england %in% c("England", selected_ccg))] ~ "Other TLHC",
    `Geography name` %in% selected_ccg ~ ccg_name,
    `Geography name` == "England" ~ "National",
    TRUE ~ "Other"
  )
) 

one_yr_survival <- one_yr_survival %>% mutate_at(vars("TLHC_CCG"), as_factor)
one_yr_survival <- one_yr_survival %>% mutate_at(vars("TLHC_CCG"), fct_relevel, "Other TLHC", "National", ccg_name)
one_yr_survival <- one_yr_survival %>% arrange(TLHC_CCG)

one_yr_survival %>% ggplot(aes(`Diagnosis Year`, `Survival (%)`, group = `Geography code`, colour = TLHC_CCG, alpha = TLHC_CCG)) + 
  geom_point() +
  geom_line() +
  scale_x_continuous() +
  scale_colour_manual(values = c("lightgrey", "#377eb8", "#e41a1c")) +
  scale_alpha_manual(values = c(0.5, 0.75, 1)) +
  theme(legend.position = "top", 
        legend.title = element_blank())
}

## sitrep waiting list data (commissioner level) ####

cancer2ww <- pull_from_sql("Sitreps", "Cancer_WL_2_Week_Wait_Comm_Mthly1")
cancer2ww <- cancer2ww %>% select(Commissioner_Code, No_Of_Patients_Seen_Within_14_Days, No_Of_Patients_Seen_After_14_Days, Effective_Snapshot_Date)


cancer2ww <- bind_rows(
  cancer2ww,
  cancer2ww %>% group_by(Effective_Snapshot_Date) %>% summarise(
    No_Of_Patients_Seen_Within_14_Days = sum(No_Of_Patients_Seen_Within_14_Days),
    No_Of_Patients_Seen_After_14_Days = sum(No_Of_Patients_Seen_After_14_Days)
  ) %>% mutate(Commissioner_Code = "England")
) %>% arrange(Effective_Snapshot_Date)


cancer2ww <- cancer2ww %>% filter(Commissioner_Code %in% c("England", ccg_codes)) %>% mutate(
  pct_2ww = No_Of_Patients_Seen_Within_14_Days / (
    No_Of_Patients_Seen_Within_14_Days + No_Of_Patients_Seen_After_14_Days
  )*100)

cancer2ww <- cancer2ww %>% filter(Commissioner_Code %in% c("England", ccg_codes))


twoweekwaitfunction <- function(ccg_name){
if (ccg_name == "Mansfield and Ashfield with Corby") {
  selected_ccg_code <- c("04E", "03E")
} else if (ccg_name == "Blackburn with Darwen with Blackpool") {
  selected_ccg_code <- c("00Q", "99A")
} else if (ccg_name == "Knowsley with Halton") {
  selected_ccg_code <- c("01J", "01F")
} else if (ccg_name == "Thurrock and Luton") {
  selected_ccg_code <- c("07G", "06P")
} else {
  selected_ccg_code <- ccg_codes[which(names(ccg_codes) == ccg_name)]
}


cancer2ww <- cancer2ww %>% mutate(
  TLHC_CCG = case_when(
    Commissioner_Code %in% c(
      ccg_codes[which(!ccg_codes %in% selected_ccg_code)]) ~ "Other TLHC",
    Commissioner_Code %in% selected_ccg_code ~ ccg_name,
    Commissioner_Code == "England" ~ "National",
    TRUE ~ "Other"
  )
) 
cancer2ww <- cancer2ww %>% mutate_at(vars("TLHC_CCG"), as_factor)
cancer2ww <- cancer2ww %>% mutate_at(vars("TLHC_CCG"), fct_relevel, "Other TLHC", "National", ccg_name)
cancer2ww <- cancer2ww %>% arrange(TLHC_CCG)
cancer2ww <- cancer2ww %>% mutate_at(vars("Effective_Snapshot_Date"),
                                    function(x) as.POSIXlt(x, tz = "", format = "%Y-%m-%d") %>% zoo::as.yearmon())

cancer2ww %>% ggplot(
  aes(
    Effective_Snapshot_Date,
    pct_2ww,
    group = Commissioner_Code,
    colour = TLHC_CCG,
    alpha = TLHC_CCG
  )) +
  geom_line() +
  geom_point() +
  labs(y = "% seen within 2 weeks (all cancers)") +
  scale_colour_manual(values = c("lightgrey", "#377eb8", "#e41a1c")) +
  scale_alpha_manual(values = c(0.5, 0.75, 1)) +
  theme(legend.position = "top", 
        legend.title = element_blank())

qic_chart <- qicharts2::qic(
  y = pct_2ww,
  x = Effective_Snapshot_Date,
  data  = cancer2ww %>% filter(TLHC_CCG == ccg_name),
  chart = "i"
)

use <- qic_chart[["data"]]

use %>% ggplot(aes(x, y.sum)) +
  geom_rect(
    data = use[1, ],
    aes(ymin = unique(use$lcl), ymax = unique(use$ucl)), 
    xmin = -Inf,
    xmax = Inf,
    fill = "cadetblue3",
    linetype = 0,
    alpha = 0.5
  ) +
  geom_hline(yintercept = unique(use$ucl), colour = "cadetblue3") +
  geom_hline(yintercept = unique(use$lcl), colour = "cadetblue3") +
  geom_hline(yintercept = unique(use$cl), colour = "red", linetype = "dashed") +
  geom_point(aes(colour = sigma.signal)) +
  geom_line() +
  annotate("text", label = "UCL", x = max(use$x), y = unique(use$ucl), hjust = -1.1) +
  annotate("text", label = "LCL", x = max(use$x), y = unique(use$lcl), hjust = -1.25) +
  annotate("text", label = "CL", x = max(use$x), y = unique(use$cl), hjust = -1.65) +
  labs(y = "Percentage of Patients Seen Within Two Weeks",
       x = "Month",
       title = paste0(ccg_name, " CCG SPC chart")) +
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)),
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.margin = unit(c(0, 2, 0, 0), "cm")
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  coord_cartesian(clip = "off")
}
