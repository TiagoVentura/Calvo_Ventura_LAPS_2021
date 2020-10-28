# ------------------------------------------------------------------------------- #
# Paper: Covid and Social Media 
# Authors: Calvo and Ventura
# Last update: October 25
# ------------------------------------------------------------------------------- #
library(here)
## Instructions

# This code reproduces the difference-in-difference analysis  of the paper "Will I Get Covid:
# Partisanship, Social Media Frames, and Perceptions of Health Risk in Brazil". 

# Specifically, the code replicates table 1 (main paper) and figures 8 and 9 (SIF)

# Download the data -------------------------------------------------------
load(here("data", "CV_data.Rdata"))

source(here("codes", "utils.R"))

# Levels for Covid --------------------------------------------------------
levels_covid <- c( "Somewhat Likely"="Algo provável", 
                   "Very Likely"= "Muito provável" , 
                   "Somewhat unlikely" = "Pouco provável", 
                   "Very unlikely" = "Nada provável", 
                   "Somewhat Appropriate" = "Algo adequada",
                   "Very Appropriate" = "Muito adequada", 
                   "Somewhat Unappropriate" = "Pouco adequada",
                   "Not Appropriate" = "Nada adequada")

# Packages ----------------------------------------------------------------
library(tidyverse)
library(summarytools)
library(lubridate)
library(scales)
library(conflicted)
library(rebus)
library(patchwork)
library(extrafont)
library(broom)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

levels(d$age)

# Clean data --------------------------------------------------------------
d_time <- d %>% 
  mutate(bolsonaro_speech = as.Date("2020-03-24"), 
         diff_rd = enddate-bolsonaro_speech, 
         diff_bin = ifelse(diff_rd<=0, 0, 1)) %>% 
  filter(diff_rd < 3)

d_time <- d_time %>% 
  mutate_at(vars(contains("covid")), ~ 
              fct_recode(.x, !!!levels_covid)) %>% 
  mutate(covid_government=fct_relevel(covid_government, 
                                      c("Not Appropriate","Somewhat Unappropriate", 
                                        "Somewhat Appropriate","Very Appropriate")), 
         covid_job = fct_relevel(covid_job, 
                                 c("Very unlikely","Somewhat unlikely", 
                                   "Somewhat Likely", "Very Likely")), 
         covid_health = fct_relevel(covid_health, 
                                    c("Very unlikely","Somewhat unlikely", 
                                      "Somewhat Likely", "Very Likely")), 
         income=as.numeric(income))


d_time$id<- ifelse(d_time$runoff_bolsonaro=="On","Bolsonaro",
                   ifelse(d_time$runoff_haddad=="On","Haddad","Independent"))

d_time <-  d_time %>% 
  mutate(id = fct_relevel(id, "Bolsonaro"))


# Simple Models -----------------------------------------------------------


modjob_no = lm(as.numeric(covid_job) ~ diff_bin + id + 
              diff_bin*id, data=d_time)

modhealth_no = lm(as.numeric(covid_health) ~ diff_bin + id + 
                 diff_bin*id, data=d_time)

modgov_no = lm(as.numeric(covid_government) ~ diff_bin + id + 
              diff_bin*id, data=d_time)


# Models With Controls ----------------------------------------------------


modjob = lm(as.numeric(covid_job) ~ diff_bin + id + 
              diff_bin*id + age + income + gender + work, data=d_time)

modhealth = lm(as.numeric(covid_health) ~ diff_bin + id + 
                 diff_bin*id +
                 age + income + gender + work, data=d_time)


modgov = lm(as.numeric(covid_government) ~ diff_bin + id + 
              diff_bin*id + 
              age + income + gender + work, data=d_time)


# Table 1 -----------------------------------------------------------------

exclude = c(names(coef(modjob))[5:13])

library(stargazer)
summary(modjob)

stargazer(modjob_no, modhealth_no, modgov_no, modjob, modhealth, modgov, 
          omit = exclude, 
          intercept.bottom = FALSE, 
          add.lines = list(c("Controls", "No", "No", "No", "Yes", "Yes", "Yes")), 
          dep.var.labels.include = FALSE,
          column.labels = c("Job Risk", "Health Risk","Government Assessment",
                            "Job Risk", "Health Risk","Government Assessment"),
          omit.stat = c("rsq", "f", "ser"), 
          covariate.labels = c("Intercept", 
                               "Post-March 23", 
                               "Haddad Voters", 
                               "Independent Voters", 
                               "Post-March 23 x Haddad Voters", 
                               "Post-March 23 x Independent Voters"), 
          out=here("outputs", "table_1.tex"))

stargazer(modjob_no, modhealth_no, modgov_no, modjob, modhealth, modgov, 
          omit = exclude, 
          type="html",
          intercept.bottom = FALSE, 
          add.lines = list(c("Controls", "No", "No", "No", "Yes", "Yes", "Yes")), 
          dep.var.labels.include = FALSE,
          column.labels = c("Job Risk", "Health Risk","Government Assessment",
                            "Job Risk", "Health Risk","Government Assessment"),
          omit.stat = c("rsq", "f", "ser"), 
          covariate.labels = c("Intercept", 
                               "Post-March 23", 
                               "Haddad Voters", 
                               "Independent Voters", 
                               "Post-March 23 x Haddad Voters", 
                               "Post-March 23 x Independent Voters"), 
          out=here("outputs", "table_1.doc"))


# Placebo: Figure 8 -----------------------------------------------------------------

d_list <- map(c(0:45), ~ d  %>% 
                    mutate(bolsonaro_speech = as.Date("2020-03-24"),
                   place = bolsonaro_speech + .x,                   
                   diff_rd = enddate-place, 
                   diff_bin = ifelse(diff_rd<=0, 0, 1), 
                   id = ifelse(runoff_bolsonaro=="On","Bolsonaro",
                               ifelse(runoff_haddad=="On","Haddad","Independent")), 
                   id = fct_relevel(id, "Bolsonaro")) %>% 
      filter(diff_rd < 3 & diff_rd > -2)
)


# Check


# Run the models

models_job  <- map_df(d_list, function(data) lm(as.numeric(covid_job) ~ diff_bin + id + 
                  diff_bin*id + age + income + gender + work, data=data) %>% 
                    tidy() %>% 
                    filter(str_detect(term, "diff_bin:idHaddad")) %>% 
                    mutate(placebo=unique(data$place), 
                           up=estimate + 1.64*std.error, 
                           lb=estimate - 1.64*std.error, 
                           pres_speech = ifelse(placebo == as.Date("2020-03-24") |
                                                placebo== as.Date("2020-03-31")|
                                                placebo== as.Date("2020-04-08"),
                                                "Presidential Official Prounoucements on TV", NA))) %>% 
                    filter(!(estimate==up)) 

ggplot(models_job, aes(y=estimate, x=placebo,
                       ymin=lb, ymax=up, fill=factor(pres_speech))) +
  geom_pointrange(shape=21, size=1.5) +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
      scale_x_date(date_breaks = "2 day", 
                   date_labels = "%b %d")  +
  labs(title="Perceptions of Job Risk",
       subtitle="Difference-in-Difference Model", 
       y=" Effect comparing Opposition versus Bolsonaro's Supporters", 
       x="Days")  +
  scale_fill_discrete(name="", limits=c("Presidential Official Prounoucements on TV"))+
  theme(axis.text.x = element_text(size = 10), 
        legend.position = "bottom")

ggsave(filename=here("outputs", "figure8_up.png"), 
       width = 14, height = 8, units = "in", pointsize = 12, bg = "white")


models_health  <- map_df(d_list, function(data) lm(as.numeric(covid_health) ~ diff_bin + id + 
                                                  diff_bin*id + age + income + gender + work, data=data) %>% 
                        tidy() %>% 
                        filter(str_detect(term, "diff_bin:idHaddad")) %>% 
                        mutate(placebo=unique(data$place), 
                               up=estimate + 1.64*std.error, 
                               lb=estimate - 1.64*std.error, 
                               pres_speech = ifelse(placebo == as.Date("2020-03-24") |
                                                      placebo== as.Date("2020-03-31")|
                                                      placebo== as.Date("2020-04-08"),
                                                    "Presidential Official Prounoucements on TV", NA))) %>% 
                                filter(!(estimate==up))


ggplot(models_health, aes(y=estimate, x=placebo,
                       ymin=lb, ymax=up, fill=pres_speech)) +
  geom_pointrange(shape=21, size=1.5) +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%b %d")  +
  labs(title="Perceptions of Health Risk",
       subtitle="Difference-in-Difference Model", 
       y=" Effect comparing Opposition versus Bolsonaro's Supporters", 
       x="Days")  +
  scale_fill_discrete(name="", limits=c("Presidential Official Prounoucements on TV"))+
  theme(axis.text.x = element_text(size = 10), 
        legend.position = "bottom")

ggsave(filename=here("outputs", "figure8_bottom.png"), 
       width = 14, height = 8, units = "in", pointsize = 12, bg = "white")



# Randomization Inference: Figure 9 ----------------------------------
library(DeclareDesign)
library(ri2)

summary(lm(diff_bin ~ id + age + income + gender + work + education + 
     as.factor(negative_partisanship) + as.numeric(ideo_place_self) , data = d_time))$f[1]

N <- nrow(d_time)

declaration <- declare_ra(N = N)

balance_fun <- function(data) {
  summary(lm(diff_bin ~ id + age + income + gender + work + education + 
                as.factor(negative_partisanship) + as.numeric(ideo_place_self) , data = data))$f[1]
}

# Confirm it works!
balance_fun(d_time)


# Conduct Ri

ri2_out <-
  conduct_ri(
    test_function = balance_fun,
    declaration = declaration,
    assignment = "diff_bin",
    sharp_hypothesis = 0,
    data = d_time, 
    sims=5000
  )

# Adjust the graph

ri2_out$sims_df <- ri2_out$sims_df %>% 
  mutate(term=fct_recode(term, "F-statistics"="Custom Test Statistic"))

graph <- plot(ri2_out) 

mexico_ri <- graph +
  theme_minimal(base_size = 22)+
  labs(title = "", 
       y="Count")

summary(ri2_out)

ggsave(here("outputs","ri_diff.png"), 
       width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

