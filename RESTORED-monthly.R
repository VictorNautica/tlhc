library(tidyverse)
library(patchwork)
library(magrittr)
library(data.table)

remove(list = ls())

monthly <- tibble::tibble(CCG = c("Mansfield and Ashfield with Corby",
                                  "Blackburn with Darwen with Blackpool",
                                  "Doncaster",
                                  "Hull",
                                  "Knowsley with Halton",
                                  "Newcastle Gateshead",
                                  "North Kirklees",
                                  "Southampton",
                                  "Tameside and Glossop",
                                  "Thurrock and Luton"),
                          no_Inv = c((13341+46245), (27996+32854), 69911, 49492, (32751+29629), 97387, 38301, 40943, 57222, (30359+33019)),
                          no_Accepted = c((3449+12232), (7349+9741), 18142, 14278, (9940+7600), 26294, 9403, 11444, (15421), (8030+8222)),
                          no_Attended = c((11253+3173), (6761+8962), 16691, 13136, (9145+6992), 24191, 8651, 10528, (14188), (7388+7564)))
                          # no_Accepted = round(no_Inv*rbeta(length(CCG), 30, 60)),
                          # Accepted_Inv_PCT = round((no_Accepted/no_Inv)*100, 1),
                          # no_Attended = round(no_Accepted*rbeta(length(CCG), 45, 2)),
                          # Attended_Inv_PCT = round((no_Attended/no_Inv)*100, 1),
                          # Attended_Accepted_PCT = round((no_Attended/no_Accepted)*100, 1),
                          # no_Referred_LDCT = round(no_Attended*rnorm(length(CCG), 0.5, 0.02)),
                          # no_Scans = round(no_Referred_LDCT*rbeta(length(CCG), 10, 2)),
                          # no_SmokeCessation = round(rnorm(length(CCG), 20000, 2000)),
                          # no_CompleteSmokeCessCourse = round(no_SmokeCessation*rbeta(length(CCG), 6, 1)))

monthly <- monthly %>% group_by(CCG)

# example_pivot <- monthly %>% select(-Accepted_Inv_PCT, -Attended_Accepted_PCT) %>% pivot_longer( no_Inv:no_CompleteSmokeCessCourse) %>% mutate_at("name", as_factor)
# 
# example_PCT <- monthly %>% select(CCG, Accepted_Inv_PCT, Attended_Accepted_PCT, Attended_Inv_PCT) %>% pivot_longer(Accepted_Inv_PCT:Attended_Inv_PCT) %>% mutate_at("name", as_factor)

# p1 <- example_PCT %>% ggplot(aes(CCG, value, fill = name)) +
#   theme_bw() +
#   geom_bar(position = "dodge", stat = "identity", colour="black", alpha = 0.66) +
#   geom_text(aes(label = paste0(value,"%")),
#             position = position_dodge(width = 0.9),
#             vjust = -0.5,
#             size = 3) +
#   labs(y = "Percentage of Patients") +
#   scale_fill_brewer(palette = "Set3",
#                     name = "Measure",
#                     labels = c("% of those invited who accepted",
#                                "% of those accepted who attended",
#                                "% of those invited who attended")) +
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
#   theme(legend.position = "top")
# 
# p2 <- example_PCT %>% ggplot(aes(name, value, fill = CCG)) +
#   theme_bw() +
#   geom_bar(position = "dodge", stat = "identity", colour="black", alpha = 0.66) +
#   geom_text(aes(label = paste0(value,"%"),
#                 group = CCG),
#             position = position_dodge(width = 0.9),
#             vjust = -0.5,
#             size = 3) +
#   labs(y = "Percentage of Patients",
#        x = "Metric") +
#   scale_fill_brewer(palette = "Set3") +
#   scale_x_discrete(labels = c("% of those invited who accepted",
#                               "% of those accepted who attended",
#                               "% of those invited who attended") %>% str_wrap(width = 20)) +
#   theme(legend.position = "top") 
# 
# p1 + p2

##

x <- tibble::tibble(Month = seq(as.Date("2019/10/1"), as.Date("2023/3/1"), "months"))
x[,monthly$CCG] <- NA

# dvalues <- dnbinom(1:49, size = 20, mu = 8)

x_list <- list()
x_list[["invited"]] <- bind_cols(Month = x$Month, imap_dfr(x[,-1], function(x, y) {
  agg_inv <- monthly[[which(monthly$CCG == y),"no_Inv"]]
  return(sample(1:18, agg_inv, T, dnorm(1:18, 5, 5)) %>% tabulate(nbins = 42))
}))

x_list[["accepted"]] <- bind_cols(x_list[["invited"]][1], imap_dfr(x[,-1], function(x, y) {
  agg_acc <- monthly[[which(monthly$CCG == y),"no_Accepted"]]
  return(sample(1:18, agg_acc, T, dnorm(1:18, 9, 9)) %>% tabulate(nbins = 42))
}))

x_list[["attended"]] <- bind_cols(x_list[["accepted"]][1], imap_dfr(x[,-1], function(x, y) {
  agg_att <- monthly[[which(monthly$CCG == y),"no_Attended"]]
  return(sample(1:18, agg_att, T, dnorm(1:18, 11, 3.5)) %>% tabulate(nbins = 42))
}))

x_list[["ldct_referrals"]] <- bind_cols(x_list[["accepted"]][1],
                                        map_df(x_list[["attended"]][-1], ~ {
  sample(1:42, sum(.x)*rnorm(1, 0.5, 0.09), T, dgamma(1:42, 5, 0.3)) %>% tabulate(nbins = 42)
  
                                       }))

x_list[["ldct_scans"]] <- bind_cols(x_list[["ldct_referrals"]][1],
                                        map_df(x_list[["ldct_referrals"]][-1], ~ {
                                          sample(1:42, sum(.x)*rnorm(1, 0.9, 0.02), T, dgamma(1:42, 8, 0.3)) %>% tabulate(nbins = 42)
                                          
                                        }))
set.seed(1)
x_list[["positive_scans_firstscan"]] <- bind_cols(x_list[["ldct_scans"]][1],
                                    map_df(x_list[["ldct_referrals"]][-1], ~ {
                                      sample(1:42, sum(.x)*rnorm(1, 0.05, 0.02), T, dgamma(1:42, 8, 0.3)) %>% tabulate(nbins = 42)
                                      
                                    }))

x_list[["positive_scans_rate100"]] <-
  bind_cols(x_list[["positive_scans_firstscan"]][1],
            (x_list[["positive_scans_firstscan"]][, -1] /
               x_list[["ldct_scans"]][, -1]) * 100)
x_list[["positive_scans_rate100"]][is.na(x_list[["positive_scans_rate100"]])] <- 0

x_list[["offered_smokecess"]] <- bind_cols(x_list[["invited"]][1],
                                    map_df(x_list[["attended"]][-1], ~ {
                                      sample(1:42, round(sum(.x)*rnorm(1, 0.55, 0.08), 0), T, dgamma(1:42, 7.5, 0.35)) %>% tabulate(nbins = 42)
                                      
                                    }))

x_list[["takeup_smokecess"]] <- bind_cols(x_list[["invited"]][1],
                                           map_df(x_list[["offered_smokecess"]][-1], ~ {
                                             sample(1:42, round(sum(.x)*rnorm(1, 0.93, 0.02), 0), T, dgamma(1:42, 8, 0.35)) %>% tabulate(nbins = 42)
                                             
                                           }))

x_list[["completed_smokecess"]] <- bind_cols(x_list[["invited"]][1],
                                          map_df(x_list[["takeup_smokecess"]][-1], ~ {
                                            sample(1:42, round(sum(.x)*rnorm(1, 0.9, 0.02), 0), T, dgamma(1:42, 10, 0.35)) %>% tabulate(nbins = 42)
                                            
                                          }))

# x_list[["invited"]] %>% pivot_longer(cols = `Mansfield and Ashfield with Corby`:`Tameside and Glossop`, names_to = "CCG", values_to = "Invited") %>% ggplot(aes(Month, Invited)) + geom_bar(stat = "identity") + facet_wrap(facets = "CCG") + labs(title = "Monthly Invited Patients in the TLHC programme, all CCGs")

x_list[["collate"]] <- imap_dfr(x_list, ~ cbind(.x, metric = .y))
x_list[["collate"]][["Month"]] %<>% zoo::as.yearmon()
# x_list[["collate"]][["Month"]] %<>% as_factor()

x_list[["indiv_ccg"]] <- as.list(monthly$CCG)
names(x_list[["indiv_ccg"]]) <- monthly$CCG
  
x_list[["indiv_ccg"]] %<>% lapply(function(z) {

  bind_cols(Month = x$Month, 
            imap_dfc(x_list[1:3], function(xx, y) {
              foobar <- xx[, c(z)]
              names(foobar) <- paste0(y)
              return(foobar)
            })
  )
}
)

x_list[["indiv_ccg"]] %<>% map(~ {
          work <- .x
          work %<>% mutate(acceptedOinv = round((accepted/invited)*100, 1),
                           attendedOinv = round((attended/invited)*100, 1),
                           attendedOacc = round((attended/accepted)*100, 1)
                                 )
          return(work)
}
)

x_list[["indiv_ccg"]] %<>% map(~ {
  work <- .x
  work %<>% mutate(acceptedOinv_3ml = frollmean(acceptedOinv, 3),
                   attendedOinv_3ml = frollmean(attendedOinv, 3),
                   attendedOacc_3ml = frollmean(attendedOacc, 3)
  )
  return(work)
}
)


x_list[["collate_pivot"]] <- x_list[["collate"]] %>% tidyr::pivot_longer(`Mansfield and Ashfield with Corby`:`Tameside and Glossop`) %>% mutate_at("metric", as_factor)

## Cum sum ####

x_list[["invited_cum"]] <- x_list[["invited"]] %>% mutate_at(vars(-Month), cumsum)
x_list[["accepted_cum"]] <- x_list[["accepted"]] %>% mutate_at(vars(-Month), cumsum)
x_list[["attended_cum"]] <- x_list[["attended"]] %>% mutate_at(vars(-Month), cumsum)

x_list[["indiv_ccg_cum"]] <- as.list(monthly$CCG)
names(x_list[["indiv_ccg_cum"]]) <- monthly$CCG

x_list[["indiv_ccg_cum"]] %<>% lapply(function(z) {
  
  bind_cols(Month = x$Month, 
            imap_dfc(x_list[14:16], function(xx, y) {
              foobar <- xx[, c(z)]
              names(foobar) <- paste0(y)
              return(foobar)
            })
  )
}
)

x_list[["indiv_ccg_cum"]] %<>% map(~ {
  work <- .x
  work %<>% mutate(acceptedOinv = round((accepted_cum/invited_cum)*100, 1),
                   attendedOinv = round((attended_cum/invited_cum)*100, 1),
                   attendedOacc = round((attended_cum/accepted_cum)*100, 1)
  )
  return(work)
}
)

x_list[["indiv_ccg"]] %<>% map(~ {
  work <- .x
  work %<>% mutate(acceptedOinv_3ml = frollmean(acceptedOinv, 3),
                   attendedOinv_3ml = frollmean(attendedOinv, 3),
                   attendedOacc_3ml = frollmean(attendedOacc, 3)
  )
  return(work)
}
)


## Graph testing ####

# x_list[["indiv_ccg"]][["Mansfield and Ashfield with Corby"]] %>% pivot_longer(acceptedOinv:attendedOacc) %>%
#   ggplot(aes(Month, value, colour = name)) +
#   geom_line() +
#   geom_point() +
#   scale_x_yearmon()

# x_list[["collate_pivot"]] %>% filter(name == "Mansfield and Ashfield with Corby") %>% ggplot(aes(Month, value, fill = metric)) + geom_bar(stat = "identity", position = "dodge") + facet_wrap("metric", scales = "free")
# (x_list[["invited"]] %>% ggplot(aes(Month, `Mansfield and Ashfield with Corby`)) + 
#   geom_bar(stat = "identity", alpha = 0.78, fill = "black")) +
# (x_list[["accepted"]] %>% ggplot(aes(Month, `Mansfield and Ashfield with Corby`)) + 
#   geom_bar(stat = "identity", alpha = 0.78, fill = "black")) +
# (x_list[["attended"]] %>% ggplot(aes(Month, `Mansfield and Ashfield with Corby`)) + 
#   geom_bar(stat = "identity", alpha = 0.78, fill = "black"))

## Projections ####

projections <- bind_cols(Month = x$Month,
          `Mansfield and Ashfield with Corby` = c(20, 60, 392, rep(935, 14), 864) %>% c(rep(0, 42-length(.)))
) %>% mutate_at(vars(contains("Corby")), .funs = list(projection = function(x) cumsum(x)))

## Source other 

source("vital_info.R")
source("RESTORED-tlhc_avg.R")
source("")

## Save env ####

save.image("workspace.RData")
