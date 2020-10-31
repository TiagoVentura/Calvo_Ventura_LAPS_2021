# ------------------------------------------------------------------------------- #
# Paper: Covid and Social Media 
# Authors: Calvo and Ventura
# Last update: October 25
# ------------------------------------------------------------------------------- #

## Instructions

# This code reproduces the first descriptive sections of the paper "Will I Get Covid:
# Partisanship, Social Media Frames, and Perceptions of Health Risk in Brazil". 

# Specifically, the code replicates figures 1, 2 of the main paper, and figure 6 and table 6 in the SIF. 


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
library(tidyr)
library(here)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")


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

# Figure 1  -------------------------------------------------------------------

covid <- d %>% select(runoff_bolsonaro, runoff_haddad, runoff_nulo, ideo_place_self, 
                      positive_partisanship, negative_partisanship, 
                      contains("covid")) 

vote <- covid %>% select(runoff_bolsonaro, runoff_haddad, contains("covid")) %>% 
  pivot_longer(cols=-c(contains("covid")), 
               names_to = "runoff", 
               values_to = "vote", 
               names_prefix = "runoff_") %>% 
  filter(vote=="On") %>% select(-vote) %>%
  group_by(runoff) %>% mutate(n=n()) %>% 
  ungroup() %>% 
  pivot_longer(cols=-c(n,runoff), 
               names_to = "covid", 
               values_to = "value")


vote_res <- vote %>% 
  count(runoff, covid, value, total=n) %>% 
  drop_na() %>% 
  mutate(prop=n/total, 
         factor_value=fct_recode(value, !!!levels_covid), 
         factor_value=fct_relevel(factor_value, 
                                  c("Very unlikely","Somewhat unlikely", 
                                    "Somewhat Likely", "Very Likely", 
                                    "Not Appropriate","Somewhat Unappropriate", 
                                    "Somewhat Appropriate",
                                    "Very Appropriate")),  
         covid_text= ifelse(covid=="covid_job", 
                            "How likely is it that you \n could lose your job?",
                            ifelse(covid=="covid_health", 
                                   "How likely will your health \n be affected by COVID-19?", 
                                   "Has the government response \n been appropriate ?")), 
         runoff=fct_rev(str_to_title(runoff)))  


pal <- RColorBrewer::brewer.pal(9, "Spectral")

plot_health <- vote_res  %>% filter(covid %in% c("covid_health"), factor_value !="Don't Know") %>%
  ggplot(aes(y=prop, x=factor_value, fill=runoff)) +
  geom_col(position = "dodge2", color="black", width=.7) +
  scale_fill_manual(values=c("Bolsonaro"=pal[9], "Haddad"=pal[1]), 
                    name="Who would you vote for?") +
  facet_wrap(~ covid_text)  + coord_flip() +
  ylab("") + 
  xlab("")  +
  guides(fill=FALSE)

plot_job <- vote_res %>% filter(covid %in% c("covid_job"), factor_value !="Don't Know") %>%
  ggplot(aes(y=prop, x=factor_value, fill=runoff)) +
  geom_col(position = "dodge2", color="black", width=.7) + 
  scale_fill_manual(values=c("Haddad"=pal[1], "Bolsonaro"=pal[9]), 
                    name="Who would you vote for?") +
  facet_wrap(~ covid_text) + coord_flip() +
  ylab("Proportion of the Votes") + xlab("") +
  guides(fill=FALSE)

plot_gov <- vote_res %>% filter(covid %in% c("covid_government"), factor_value !="Don't Know") %>%
  ggplot(aes(y=prop, x=factor_value, fill=runoff)) +
  geom_col(position = "dodge2", color="black", width=.7) + 
  scale_fill_manual(values=c("Bolsonaro"=pal[9], "Haddad"=pal[1]), 
                    name="Who would you vote for?") +
  facet_wrap(~ covid_text) + coord_flip() + 
  ylab("Proportion of the Votes") + xlab("") +
  guides(fill=guide_legend(reverse = TRUE)) ## coord flip change the order of the bars


graph_covid <- ((plot_health / plot_job) | plot_gov) + 
  plot_annotation(title="\n Partisanship, Risk Perceptions and Government Responses to Covid in Brazil")

ggsave(plot=graph_covid, filename=here("outputs", "figure1.png"), 
       width = 14, height = 8, units = "in", pointsize = 12, bg = "white")
# 
# ggsave(plot=graph_covid, filename=here("outputs", "figure1.tiff"), 
#        width = 14, height = 8, units = "in", pointsize = 12, dpi=300, bg = "white")


# Figure 6  ---------------------------------------------------------------

covid_negpartisanship <- covid %>% 
                          select(negative_partisanship, positive_partisanship, contains("covid")) %>%
                          mutate(petistas=ifelse(str_detect(positive_partisanship, "(PT)"), "Petistas", 0),  
                                 anti_petistas=ifelse(str_detect(positive_partisanship, "Nenhum|Prefiro")
                                                        & str_detect(negative_partisanship, "(PT)"), 
                                                "Anti-Petistas", 0))
  
vote <- covid_negpartisanship %>% 
  select(petistas, anti_petistas, contains("covid")) %>% 
  pivot_longer(cols=-c(contains("covid")), 
               names_to = "runoff", 
               values_to = "vote", 
               names_prefix = "runoff_") %>% 
  filter(vote!=0) %>%
  select(-vote) %>%
  group_by(runoff) %>% mutate(n=n()) %>% 
  ungroup() %>% 
  pivot_longer(cols=-c(n,runoff), 
               names_to = "covid", 
               values_to = "value")


vote_res <- vote %>% 
  count(runoff, covid, value, total=n) %>% 
  drop_na() %>% 
  mutate(prop=n/total, 
         factor_value=fct_recode(value, !!!levels_covid), 
         factor_value=fct_relevel(factor_value, 
                                  c("Very unlikely","Somewhat unlikely", 
                                    "Somewhat Likely", "Very Likely", 
                                    "Not Appropriate","Somewhat Unappropriate", 
                                    "Somewhat Appropriate",
                                    "Very Appropriate")),  
         covid_text= ifelse(covid=="covid_job", 
                            "How likely is it that you \n could lose your job?",
                            ifelse(covid=="covid_health", 
                                   "How likely will your health \n be affected by COVID-19?", 
                                   "Has the government response \n been appropriate ?")), 
         runoff=fct_recode(runoff, !!!list("Petistas"="petistas", 
                                        "Anti-Petistas"="anti_petistas")))  



plot_health <- vote_res  %>% filter(covid %in% c("covid_health"), factor_value !="Don't Know") %>%
  ggplot(aes(y=prop, x=factor_value, fill=runoff)) +
  geom_col(position = "dodge2", color="black", width=.7) +
  scale_fill_manual(values=c("Anti-Petistas"=pal[9], "Petistas"=pal[1]), 
                    name="Who would you vote for?") +
  facet_wrap(~ covid_text)  + coord_flip() +
  ylab("") + 
  xlab("")  +
  guides(fill=FALSE)

plot_job <- vote_res %>% filter(covid %in% c("covid_job"), factor_value !="Don't Know") %>%
  ggplot(aes(y=prop, x=factor_value, fill=runoff)) +
  geom_col(position = "dodge2", color="black", width=.7) + 
  scale_fill_manual(values=c("Petistas"=pal[1], "Anti-Petistas"=pal[9]), 
                    name="Who would you vote for?") +
  facet_wrap(~ covid_text) + coord_flip() +
  ylab("Proportion of the Votes") + xlab("") +
  guides(fill=FALSE)

plot_gov <- vote_res %>% filter(covid %in% c("covid_government"), factor_value !="Don't Know") %>%
  ggplot(aes(y=prop, x=factor_value, fill=runoff)) +
  geom_col(position = "dodge2", color="black", width=.7) + 
  scale_fill_manual(values=c("Anti-Petistas"=pal[9], "Petistas"=pal[1]), 
                    name="Negative Partisanship") +
  facet_wrap(~ covid_text) + coord_flip() +
  ylab("Proportion of the Votes") + xlab("") +
  guides(fill=guide_legend(reverse = TRUE)) ## coord flip change the order of the bars

graph_covid <- ((plot_health / plot_job) | plot_gov) + 
  plot_annotation(title="\n Partisanship, Risk Perceptions and Government Responses to Covid in Brazil")

ggsave(plot=graph_covid, filename=here("outputs", "figure_6.png"), 
       width = 14, height = 8, units = "in", pointsize = 12, bg = "white")

# 
# ggsave(plot=graph_covid, filename=here("outputs", "figure_6.tiff"), 
#        width = 14, height = 8, units = "in", pointsize = 12, dpi=300, bg = "white")


# Figure 2 -------------------------------------------------------------



outcomes_haddad <- map_df(c("covid_government", "covid_health", "covid_job"), ~ 
                            lm(as.numeric(get(.x)) ~ runoff_haddad + 
                                 income + gender + work + 
                                 as.numeric(education) + age , data=d) %>% 
                            tidy(.) %>% 
                            mutate(outcome=.x, 
                                   lb=estimate - 1.96*std.error, 
                                   up= estimate + 1.96*std.error)) %>% 
  filter(!(term %in% c("(Intercept)"))) %>% 
  mutate(label=paste0("Haddad   "))


outcomes_bolsonaro <- map_df(c("covid_government", "covid_health", "covid_job"), ~ 
                               lm(as.numeric(get(.x)) ~ runoff_bolsonaro + 
                                    income + gender + work + 
                                    as.numeric(education) + age, data=d) %>% 
                               tidy(.) %>% 
                               mutate(outcome=.x, 
                                      lb=estimate - 1.96*std.error, 
                                      up= estimate + 1.96*std.error)) %>% 
  filter(!(term %in% c("(Intercept)"))) %>% 
  mutate(label=paste0("Bolsonaro"))





outcomes_ind <- map_df(c("covid_government", "covid_health", "covid_job"), ~ 
                         lm(as.numeric(get(.x)) ~ runoff_nulo + 
                              income + gender + work +
                              as.numeric(education) + age, data=d) %>% 
                         tidy(.) %>% 
                         mutate(outcome=.x, 
                                lb=estimate - 1.96*std.error, 
                                up= estimate + 1.96*std.error)) %>% 
  filter(!(term %in% c("(Intercept)"))) %>% 
  mutate(label=paste0("Independents"))


outcomes <- rbind(outcomes_haddad, outcomes_bolsonaro, outcomes_ind) %>% 
  mutate(outcome= ifelse(outcome=="covid_job", 
                         "How likely is it that you \n could lose your job? ",
                         ifelse(outcome=="covid_health", 
                                "How likely will your health \n be affected by COVID-19?", 
                                "Has the government response \n been appropriate ?"))) 

outcomes_reduced <- outcomes %>% filter(str_detect(term, "runoff"))

# Graph

ggplot(outcomes_reduced, aes(y=estimate, x=label, 
                             ymin=up, ymax=lb, color=label)) +
  geom_pointrange(shape=21, fill="white", size=2) +
  labs(x="", y="Point Estimates", 
       title = "\nPartisanship, Risk Perceptions and Government Responses to Covid in Brazil", 
       subtitle = "Regression Estimates with Controls by Income, Gender, Age, Education, and Occupation.", 
       caption ="Note: Positive point-estimates when respondents show greater support \n for the government, or higher risk of losing their jobs or being infected by COVID-19") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_color_manual(values=c("Bolsonaro"=pal[9], "Haddad   "=pal[1], 
                              "Independents"=pal[4]), 
                     name="Who would you vote for?") +
  facet_wrap(~outcome) +
  theme(plot.caption = element_text(size=12, family=my_font))

ggsave(filename=here("outputs", "figure2_up.png"), 
        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

ggsave(filename=here("outputs", "figure2_up.tiff"), 
       width = 12, height = 8, units = "in", pointsize = 12, dpi=300, bg = "white")

# Complete model

models_complete <- map(c( "covid_job", "covid_health", "covid_government"), ~ 
                              lm(as.numeric(get(.x)) ~ runoff_haddad + runoff_nulo +
                                   income + gender + work + 
                                   as.numeric(education) + as.numeric(age) , data=d))


#Regression:Negative Partisansip ----------------------------------------------------


d <- d %>% 
  mutate(anti=ifelse(str_detect(negative_partisanship, "Trabalhadores") & 
         str_detect(positive_partisanship, "Prefiro|Nenhum"),
         1, 0), 
         petistas = ifelse(str_detect(positive_partisanship, "Trabalhadores"), 
                          1, 0), 
         nonpartisans= ifelse(!str_detect(positive_partisanship, "Trabalhadores") &
                              !str_detect(negative_partisanship, "Trabalhadores"), 
                          1,0))




# Working

outcomes_pt <- map_df(c("covid_government", "covid_health", "covid_job"), ~ 
                            lm(as.numeric(get(.x)) ~ petistas + 
                                 income + gender + work + education + age, data=d) %>% 
                            tidy(.) %>% 
                            mutate(outcome=.x, 
                                   lb=estimate - 1.96*std.error, 
                                   up= estimate + 1.96*std.error)) %>% 
  filter(!(term %in% c("(Intercept)"))) %>% 
  mutate(label=paste0("Petistas"))


outcomes_anti <- map_df(c("covid_government", "covid_health", "covid_job"), ~ 
                               lm(as.numeric(get(.x)) ~ anti + 
                                    income + gender + work + 
                                    education + age, data=d) %>% 
                               tidy(.) %>% 
                               mutate(outcome=.x, 
                                      lb=estimate - 1.96*std.error, 
                                      up= estimate + 1.96*std.error)) %>% 
  filter(!(term %in% c("(Intercept)"))) %>% 
  mutate(label=paste0("Anti-Petistas"))




outcomes_non <- map_df(c("covid_government", "covid_health", "covid_job"), ~ 
                         lm(as.numeric(get(.x)) ~ nonpartisans + 
                              income + gender + work +
                              education + age, data=d) %>% 
                         tidy(.) %>% 
                         mutate(outcome=.x, 
                                lb=estimate - 1.96*std.error, 
                                up= estimate + 1.96*std.error)) %>% 
  filter(!(term %in% c("(Intercept)"))) %>% 
  mutate(label=paste0("Other"))


outcomes <- rbind(outcomes_pt, outcomes_anti, outcomes_non) %>% 
  mutate(outcome= ifelse(outcome=="covid_job", 
                         "How likely is it that you \n could lose your job? ",
                         ifelse(outcome=="covid_health", 
                                "How likely will your health \n be affected by COVID-19?", 
                                "Has the government response \n been appropriate ?"))) 

outcomes_reduced <- outcomes %>% 
                    filter(str_detect(term, "anti|nonpartisans|petistas")) %>% 
                    mutate(label=fct_relevel(label, "Other", "Petistas"))

# Complete Model

ggplot(outcomes_reduced, aes(y=estimate, x=fct_rev(label), 
                             ymin=up, ymax=lb, color=fct_rev(label))) +
  geom_pointrange(shape=21, fill="white", size=2) +
  labs(x="", y="Point Estimates", 
       title = "\nNegative Partisanship and Risk Perceptions to Covid-19", 
       subtitle = "Regression Estimates with Controls by Income, Gender, Age, Education, and Occupation.", 
       caption ="Note: Positive point-estimates when respondents show greater support \n for the government, or higher risk of losing their jobs or being infected by COVID-19") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_color_manual(values=c("Anti-Petistas"=pal[9], "Petistas"=pal[1], 
                              "Other"=pal[4]), 
                     name="Partisanship") +
  facet_wrap(~outcome) +
  theme(plot.caption = element_text(size=12, family=my_font))

ggsave(filename=here("outputs", "figure2_bottom.png"), 
       width = 12, height = 8, units = "in", pointsize = 12, bg = "white")
# 
# ggsave(filename=here("outputs", "figure2_bottom.tiff"), 
#        width = 12, height = 8, units = "in", pointsize = 12, dpi=300, bg = "white")


# Table 6 -----------------------------------------------------------------

models_complete_ng <- map(c( "covid_job", "covid_health","covid_government"), ~ 
                         lm(as.numeric(get(.x)) ~ petistas + nonpartisans +
                              income + gender + work + 
                              as.numeric(education) + as.numeric(age) , data=d))

library(stargazer)
stargazer(models_complete[[1]],
          models_complete[[2]], 
          models_complete[[3]], 
          models_complete_ng[[1]], 
          models_complete_ng[[2]], 
          models_complete_ng[[3]], 
          intercept.bottom = FALSE, 
          dep.var.labels.include = FALSE,
          column.labels = c("Job Risk", "Health Risk","Government Assessment",
                            "Job Risk", "Health Risk","Government Assessment"),
          omit.stat = c("rsq", "f", "ser"), 
          covariate.labels = c("Intercept", "Voters Haddad", 
                               "Voters Independents", 
                               "Petistas", 
                               "Others (Non-Partisans)", 
                               "Income", 
                               "Gender:Male", 
                               "Employed", 
                               "Education", 
                               "Age"), 
          out=here("outputs", "table_6.tex"))
# 
# stargazer(models_complete[[1]],
#           models_complete[[2]], 
#           models_complete[[3]], 
#           models_complete_ng[[1]], 
#           models_complete_ng[[2]], 
#           models_complete_ng[[3]], 
#           intercept.bottom = FALSE, 
#           dep.var.labels.include = FALSE,
#           column.labels = c("Job Risk", "Health Risk","Government Assessment",
#                             "Job Risk", "Health Risk","Government Assessment"),
#           omit.stat = c("rsq", "f", "ser"), 
#           covariate.labels = c("Intercept", "Voters Haddad", 
#                                "Voters Independents", 
#                                "Petistas", 
#                                "Others (Non-Partisans)", 
#                                "Income", 
#                                "Gender:Male", 
#                                "Employed", 
#                                "Education", 
#                                "Age"), 
#           type="html",
#           out=here("outputs", "table_6.doc"))
# 
# 
