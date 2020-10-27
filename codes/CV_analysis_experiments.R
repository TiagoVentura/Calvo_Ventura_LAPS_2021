# ------------------------------------------------------------------------------- #
# Paper: Covid and Social Media 
# Authors: Calvo and Ventura
# Last update: October 25
# Clean the survey: Brazil
# ------------------------------------------------------------------------------- #

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

# Models ------------------------------------------------------------------

# Split Samples -----------------------------------------------------------

d_haddad <- treatment_framing %>% 
  filter(runoff_haddad=="On")
d_ind <- treatment_framing %>% 
  filter(runoff_nulo=="On")
d_bolso <- treatment_framing %>% 
  filter(runoff_bolsonaro=="On")


# All the treatments together ---------------------------------------------

tidy_model_all_together <- function(model, outcome, subsample) {
  model %>% 
    tidy(.) %>% 
    mutate(outcome=outcome, 
           lb=estimate - 1.96*std.error, 
           up= estimate + 1.96*std.error, 
           lb.90=estimate - 1.64*std.error,
           up.90=estimate + 1.64*std.error) %>% 
    filter(!(term %in% c("(Intercept)"))) %>% 
    mutate(term=str_to_title(str_replace_all(term, "_", " ")))   %>% 
    mutate(outcome_label= ifelse(outcome=="covid_job", 
                                 "How likely is it that you \n could lose your job? ",
                                 ifelse(outcome=="covid_health", 
                                        "How likely will your health  \n be affected by COVID-19?", 
                                        "Has the government response \n been appropriate ?")), 
           subsample=subsample) 
  
  
}

treatment_framing$id<- ifelse(treatment_framing$runoff_bolsonaro=="On",1,
                              ifelse(treatment_framing$runoff_haddad=="On",2,3))

## Job Security

mod_job_haddad <- lm(as.numeric(covid_job) - mean(as.numeric(covid_job), na.rm = TRUE) ~ -1 + negative_bolsonaro + 
                       negative_haddad + 
                       neutral_bolsonaro +
                       neutral_haddad,  data=d_haddad)


mod_job_bolsonaro <- lm(as.numeric(covid_job) - mean(as.numeric(covid_job), na.rm=TRUE) ~ -1 + negative_bolsonaro + 
                negative_haddad + 
                neutral_bolsonaro +
                neutral_haddad,  data=d_bolso)

mod_job_ind <- lm(as.numeric(covid_job)- mean(as.numeric(covid_job), na.rm=TRUE) ~ -1 + negative_bolsonaro + 
                          negative_haddad + 
                          neutral_bolsonaro +
                          neutral_haddad,  data=d_ind)

mod_job_all <- lm(as.numeric(covid_job)- mean(as.numeric(covid_job), na.rm=TRUE) ~ -1 + negative_bolsonaro + 
                    negative_haddad + 
                    neutral_bolsonaro +
                    neutral_haddad,  data=treatment_framing)

list_models <- list( mod_job_bolsonaro, mod_job_haddad,mod_job_ind, mod_job_all)
list_subsamples <- list("Bolsonaro Voters", "Haddad Voters", "Independent Voters", "All")

res_job <- map2_df(list_models, list_subsamples, 
        ~ tidy_model_all_together(.x, "covid_job", subsample=.y))

## Health

mod_health_haddad <- lm(as.numeric(covid_health)-mean(as.numeric(covid_health), na.rm=TRUE) ~ -1 + negative_bolsonaro + 
                       negative_haddad + 
                       neutral_bolsonaro +
                       neutral_haddad,  data=d_haddad)

mod_health_bolsonaro <- lm(as.numeric(covid_health) -mean(as.numeric(covid_health), na.rm = TRUE)~ -1 + negative_bolsonaro + 
                          negative_haddad + 
                          neutral_bolsonaro +
                          neutral_haddad,  data=d_bolso)

mod_health_ind <- lm(as.numeric(covid_health) -mean(as.numeric(covid_health), na.rm = TRUE) ~ -1 + negative_bolsonaro + 
                    negative_haddad + 
                    neutral_bolsonaro +
                    neutral_haddad,  data=d_ind)


mod_health_all <- lm(as.numeric(covid_health) -mean(as.numeric(covid_health), na.rm = TRUE) ~ -1 + negative_bolsonaro + 
                       negative_haddad + 
                       neutral_bolsonaro +
                       neutral_haddad,  data=treatment_framing)



list_models <- list(mod_health_bolsonaro, mod_health_haddad, mod_health_ind, mod_health_all)
list_subsamples <- list("Bolsonaro Voters", "Haddad Voters", "Independent Voters", "All")

res_health <- map2_df(list_models, list_subsamples, 
                   ~ tidy_model_all_together(.x, "covid_health", subsample=.y))


## government support


mod_gov_haddad <- lm(as.numeric(covid_government) - mean(as.numeric(covid_government), na.rm=TRUE )~ -1 + negative_bolsonaro + 
                          negative_haddad + 
                          neutral_bolsonaro +
                          neutral_haddad,  data=d_haddad)
mod_gov_bolsonaro <- lm(as.numeric(covid_government) - mean(as.numeric(covid_government),  na.rm=TRUE)~ -1 + negative_bolsonaro + 
                             negative_haddad + 
                             neutral_bolsonaro +
                             neutral_haddad,  data=d_bolso)

mod_gov_ind <- lm(as.numeric(covid_government) - mean(as.numeric(covid_government),  na.rm=TRUE) ~ -1 + 
                    negative_bolsonaro + 
                       negative_haddad + 
                       neutral_bolsonaro +
                       neutral_haddad,  data=d_ind)


mod_gov_all <- lm(as.numeric(covid_government) - mean(as.numeric(covid_government),  na.rm=TRUE) ~ -1 + 
                    negative_bolsonaro + 
                    negative_haddad + 
                    neutral_bolsonaro +
                    neutral_haddad,  data=treatment_framing)

list_models <- list(mod_gov_bolsonaro, mod_gov_haddad, mod_gov_ind, mod_gov_all)
list_subsamples <- list("Bolsonaro Voters", "Haddad Voters", "Independent Voters", "All")

res_gov <- map2_df(list_models, list_subsamples, 
                      ~ tidy_model_all_together(.x, "covid_gov", subsample=.y))


# Putting results together ------------------------------------------------

results_bind <- bind_rows(res_job, res_health, res_gov) %>%
                    mutate(term = str_replace(term, "Neutral", "Positive"))

ggplot(results_bind, 
       aes(y=estimate, x=term, fill=subsample, 
           ymin=up, ymax=lb)) +
  geom_pointrange(size=1, shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  geom_pointrange(aes(ymin=lb.90, y=estimate, ymax=up.90), shape=21, 
                  size=1.5, color="grey5") +
  scale_fill_manual(values=c(pal[7], pal[9], pal[1], pal[4]), name="") +
  labs(x="Treatment Conditons", y="Treatment Effects", 
       title = "Partisan Responses and Risk Perceptions \n about the Covid-19 in Brazil\n", 
       caption ="Note: Positive point estimates imply respondents show greater support for the government response") +
  facet_grid(subsample~outcome_label ) +
  coord_flip() + 
  ylim(-.4, .4)

ggsave(filename=here("outputs", "figure_3.png"), 
       width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

# Extract p-values --------------------------------------------------------

library(gtools)
library(car)
var1 <- c("negative_bolsonaro", "negative_haddad", "neutral_bolsonaro", "neutral_haddad")
var2 <-c("negative_bolsonaro", "negative_haddad", "neutral_bolsonaro", "neutral_haddad")
df <- as.data.frame(combinations(n = 4, r = 2, repeats.allowed = F, v = var1)) %>% 
  mutate_all(as.character)

variables <-as.data.frame(t(df))
extract_p_values <- function(model, variables, dv, subsample){
  
  
  map(variables, ~ linearHypothesis(model, paste0(.x[1], "=", .x[2]))$`Pr(>F)`[2]) %>% 
    unlist() %>% tibble(pvalue=., dv, subsample )
  
  
}

# job
modelsjob <- list(mod_job_all, mod_job_bolsonaro, mod_job_haddad, mod_job_ind) 
list_subsamples <- list("All", "Bolsonaro Voters", "Haddad Voters", "Independents")

job_results <- map2_dfr(modelsjob, list_subsamples, ~ extract_p_values(.x, variables, .y, "Job Risks") %>% 
                          bind_cols(df, .))


#health
modelshealth <- list(mod_health_all, mod_health_bolsonaro, mod_health_haddad, mod_health_ind) 
list_subsamples <- list("All", "Bolsonaro Voters", "Haddad Voters", "Independents")
health_results <- map2_dfr(modelshealth, list_subsamples, ~ extract_p_values(.x, variables, .y, "Health Risks") %>% 
                             bind_cols(df, .))
#gov
modelsgov <- list(mod_gov_all, mod_gov_bolsonaro, mod_gov_haddad, mod_gov_ind) 
list_subsamples <- list("All", "Bolsonaro Voters", "Haddad Voters", "Independents")
gov_results <- map2_dfr(modelsgov, list_subsamples, ~ extract_p_values(.x, variables, .y, "Government Support") %>% 
                          bind_cols(df, .))


res_all <- bind_rows(gov_results, job_results, health_results) %>% 
  mutate(subsample=fct_relevel(subsample, "Government Support", "Job Risks"),
         pvalueid = ifelse(pvalue <=0.05, "p-value < 0.05", 
                           ifelse(pvalue<=0.10, "p-value <0.10", 
                                  "p-vale > 0.10")),
         pvalueid =fct_relevel(pvalueid, "p-value < 0.05", 
                               "p-value <0.10"),
         var1=str_to_title(str_replace(str_replace(V1, "_", " "), "neutral", "Positive")), 
         var2=str_to_title(str_replace(str_replace(V2, "_", " "), "neutral", "Positive")))


# further modified ggplot
ggplot(res_all,aes(x=var1,y=fct_rev(var2),fill=pvalueid))+
  geom_tile(colour="gray95",size=0.5, alpha=.8)+
  guides(fill=guide_legend(title="P-value"))+
  labs(x="",y="",
       title="Partisan Responses and Risk Perceptions: \n",
       subtitle="P-values for the Treatment Comparisons")+
  scale_fill_manual(values=c("tomato2", "pink", "white")) +
  geom_text(aes(label = round(pvalue, 3))) +
  facet_grid(dv~subsample ) +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(angle=45, hjust=1)) 

ggsave(filename=here("outputs", "figure_3_pvalue.png"), 
       width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

# Negative Partisanship ----------------------------------------------------------------

# H1
### Split Sample
d_anti <- treatment_framing %>% 
  filter(str_detect(negative_partisanship, "Trabalhadores"), 
         str_detect(positive_partisanship, "Prefiro|Nenhum"))

d_pet <- treatment_framing %>% 
  filter(str_detect(positive_partisanship, "Trabalhadores"))

d_non_partisans <- treatment_framing %>% 
                    filter(!str_detect(positive_partisanship, "Trabalhadores") &
                           !str_detect(negative_partisanship, "Trabalhadores"))

## Job Security

mod_job_pet <- lm(as.numeric(covid_job) - mean(as.numeric(covid_job), na.rm = TRUE) ~ -1 + negative_bolsonaro + 
                       negative_haddad + 
                       neutral_bolsonaro +
                       neutral_haddad,  data=d_pet)

mod_job_anti <- lm(as.numeric(covid_job) - mean(as.numeric(covid_job), na.rm=TRUE) ~ -1 + negative_bolsonaro + 
                          negative_haddad + 
                          neutral_bolsonaro +
                          neutral_haddad,  data=d_anti)

mod_job_pet <- lm(as.numeric(covid_job) - mean(as.numeric(covid_job), na.rm = TRUE) ~ -1 + negative_bolsonaro + 
                    negative_haddad + 
                    neutral_bolsonaro +
                    neutral_haddad,  data=d_pet)

mod_job_np <- lm(as.numeric(covid_job) - mean(as.numeric(covid_job), na.rm=TRUE) ~ -1 + negative_bolsonaro + 
                     negative_haddad + 
                     neutral_bolsonaro +
                     neutral_haddad,  data=d_non_partisans)

list_models <- list( mod_job_anti, mod_job_pet)
list_subsamples <- list("Anti-Petistas", "Petistas")

res_job_ng <- map2_df(list_models, list_subsamples, 
                   ~ tidy_model_all_together(.x, "covid_job", subsample=.y))

# health

mod_health_pet <- lm(as.numeric(covid_health)-mean(as.numeric(covid_health), na.rm=TRUE) ~ -1 + negative_bolsonaro + 
                          negative_haddad + 
                          neutral_bolsonaro +
                          neutral_haddad,  data=d_pet)

mod_health_anti <- lm(as.numeric(covid_health) -mean(as.numeric(covid_health), na.rm = TRUE)~ -1 + negative_bolsonaro + 
                             negative_haddad + 
                             neutral_bolsonaro +
                             neutral_haddad,  data=d_anti)

mod_health_np<- lm(as.numeric(covid_health) -mean(as.numeric(covid_health), na.rm = TRUE)~ -1 + negative_bolsonaro + 
                        negative_haddad + 
                        neutral_bolsonaro +
                        neutral_haddad,  data=d_non_partisans)


list_models <- list(mod_health_anti, mod_health_pet)
list_subsamples <- list("Anti-Petistas", "Petistas")

res_health_ng <- map2_df(list_models, list_subsamples, 
                      ~ tidy_model_all_together(.x, "covid_health", subsample=.y))

## Gov

mod_gov_pet <- lm(as.numeric(covid_government) - mean(as.numeric(covid_government), na.rm=TRUE )~ -1 + negative_bolsonaro + 
                       negative_haddad + 
                       neutral_bolsonaro +
                       neutral_haddad,  data=d_pet)
mod_gov_anti <- lm(as.numeric(covid_government) - mean(as.numeric(covid_government),  na.rm=TRUE)~ -1 + negative_bolsonaro + 
                          negative_haddad + 
                          neutral_bolsonaro +
                          neutral_haddad,  data=d_anti)

mod_gov_np <- lm(as.numeric(covid_government) - mean(as.numeric(covid_government),  na.rm=TRUE)~ -1 + negative_bolsonaro + 
                     negative_haddad + 
                     neutral_bolsonaro +
                     neutral_haddad,  data=d_non_partisans)

list_models <- list(mod_gov_anti, mod_gov_pet )
list_subsamples <- list("Anti-Petistas", "Petistas")

res_gov_ng <- map2_df(list_models, list_subsamples, 
                   ~ tidy_model_all_together(.x, "covid_gov", subsample=.y))


# Combine
results_bind <- bind_rows(res_job_ng, res_health_ng, res_gov_ng) %>%
                  mutate(subsample=fct_relevel(subsample, "Anti-Petistas", "Petistas")) %>%
                  mutate(term = str_replace(term, "Neutral", "Positive"))


ggplot(results_bind, 
       aes(y=estimate, x=term, fill=subsample, 
           ymin=up, ymax=lb)) +
  geom_pointrange(size=1, shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  geom_pointrange(aes(ymin=lb.90, y=estimate, ymax=up.90), shape=21, 
                  size=1.5, color="grey5") +  scale_fill_manual(values=c(pal[9], pal[1], pal[4]), name="") +
  labs(x="Treatment Conditons", y="Treatment Effects", 
       title = "Negative Partisanship and Risk Perceptions  \n about the Covid-19 in Brazil\n", 
       caption ="Note: Positive point estimates imply respondents show greater support for the government response") +
  facet_grid(subsample~outcome_label ) +
  coord_flip() + 
  ylim(-.5, .5)

ggsave(filename=here("outputs", "figure_4.png"), 
       width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

#### P - Value

# job
modelsjob <- list(mod_job_pet, mod_job_anti) 
list_subsamples <- list("Petistas", "Anti-Petistas")
job_results <- map2_dfr(modelsjob, list_subsamples, ~ extract_p_values(.x, variables, .y, "Job Risks") %>% 
                          bind_cols(df, .))

#health
modelshealth <- list(mod_health_pet, mod_health_anti) 
list_subsamples <- list("Petistas", "Anti-Petistas")
health_results <- map2_dfr(modelshealth, list_subsamples, ~ extract_p_values(.x, variables, .y, "Health Risks") %>% 
                             bind_cols(df, .))
#gov
modelsgov <- list(mod_gov_pet, mod_gov_anti) 
gov_results <- map2_dfr(modelsgov, list_subsamples, ~ extract_p_values(.x, variables, .y, "Government Support") %>% 
                          bind_cols(df, .))


res_all <- bind_rows(gov_results, job_results, health_results) %>% 
  mutate(subsample=fct_relevel(subsample, "Government Support", "Job Risks"),
         pvalueid = ifelse(pvalue <=0.05, "p-value < 0.05", 
                           ifelse(pvalue<=0.10, "p-value <0.10", 
                                  "p-vale > 0.10")),
         pvalueid =fct_relevel(pvalueid, "p-value < 0.05", 
                               "p-value <0.10"),
         var1=str_to_title(str_replace(str_replace(V1, "_", " "), "neutral", "Positive")), 
         var2=str_to_title(str_replace(str_replace(V2, "_", " "), "neutral", "Positive")))


# further modified ggplot
ggplot(res_all,aes(x=var1,y=fct_rev(var2),fill=pvalueid))+
  geom_tile(colour="gray95",size=0.5, alpha=.8)+
  guides(fill=guide_legend(title="P-value"))+
  labs(x="",y="",
       title="Negative Partisanship and Risk Perceptions: \n",
       subtitle="P-values for the Treatment Comparisons")+
  scale_fill_manual(values=c("tomato2", "pink", "white")) +
  geom_text(aes(label = round(pvalue, 3))) +
  facet_grid(dv~subsample ) +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(angle=45, hjust=1)) 

ggsave(filename=here("outputs", "figure_4_pvalue.png"), 
       width = 12, height = 8, units = "in", pointsize = 12, bg = "white")



# Why Null Findings? ----------------------------------------------------

reactions <- treatment_framing %>% rowid_to_column() %>%
  select(rowid, treat_reaction) %>%
  separate(treat_reaction, sep=",", paste0("split_reactions", 1:8)) %>%
  pivot_longer(cols=contains("split_reactions"),
               values_to = "reactions",
               names_to = "reactions_order") %>%
  mutate_all(~ifelse(.x=="-999", NA, .x)) %>%
  filter(!is.na(reactions)) %>%
  mutate(reactions=str_remove_all(reactions, "[[:punct:]]")) %>%
  mutate(reactions=fct_relevel(reactions, "Ignorar"), 
         reactions_num=ifelse(reactions=="Ignorar", "Ignored", "Reacted")) 
    
emotions <- treatment_framing %>% rowid_to_column() %>%
  select(rowid, treat_emotions) %>%
  separate(treat_emotions, sep=",", paste0("split_emotions", 1:8)) %>%
  pivot_longer(cols=contains("split_emotions"),
               values_to = "emotions",
               names_to = "emotions_order") %>%
  mutate_all(~ifelse(.x=="-999", NA, .x)) %>%
  filter(!is.na(emotions), 
         emotions=="Irritado")

# merge back all

d_all <- treatment_framing %>% 
  rowid_to_column() %>% 
  left_join(emotions) %>%
  mutate(emotions=ifelse(is.na(emotions), "no-anger", emotions)) %>%
  mutate(vote=ifelse(runoff_bolsonaro=="On", "Bolsonaro Voters", 
                     ifelse(runoff_haddad=="On", "Haddad Voters", 
                            ifelse(runoff_nulo=="On", "Independents", NA))), 
         treatment= ifelse(negative_bolsonaro==1, "Negative Bolsonaro", 
                           ifelse(negative_haddad==1, "Negative Haddad", 
                                  ifelse(neutral_bolsonaro==1, "Positive Bolsonaro", 
                                         ifelse(neutral_haddad==1, "Positive Haddad", NA)))), 
         treatment=fct_relevel(treatment, "Positive Bolsonaro", "Negative Bolsonaro", 
                               "Negative Haddad", "Positive Haddad"),
         vote=fct_relevel(vote, "Independents", "Haddad Voters", "Bolsonaro Voters"),
         anger=fct_rev(ifelse(emotions=="Irritado", "Anger", "No-Anger")), 
         latency=log(treat_reaction_page_submit + 1))

# merge back 

d_reaction <- treatment_framing %>% 
  rowid_to_column() %>% 
  left_join(reactions) %>% 
  nest_by(reactions_num) %>%
  drop_na() 

d_r_h <- treatment_framing %>% 
  rowid_to_column() %>% 
  left_join(reactions) %>% 
  filter(runoff_haddad=="On") %>%
  nest_by(reactions_num) %>%
  drop_na() 

d_r_b <- treatment_framing %>% 
  rowid_to_column() %>% 
  left_join(reactions) %>% 
  filter(runoff_bolsonaro=="On") %>%
  nest_by(reactions_num) %>%
  drop_na() 

d_r_n <- treatment_framing %>% 
  rowid_to_column() %>% 
  left_join(reactions) %>% 
  filter(runoff_nulo=="On") %>%
  nest_by(reactions_num) %>%
  drop_na() 


# Autopsy -----------------------------------------------------------------

modh_1 <- lm(as.numeric(covid_health) ~ anger + treatment + vote + latency  , data=d_all)
modh_2 <-lm(as.numeric(covid_health) ~ anger + treatment + latency, data=d_all)
modh_3 <- lm(as.numeric(covid_health) ~ anger*vote + treatment + latency  , data=d_all)

modg_1 <- lm(as.numeric(covid_government) ~ anger + treatment + vote + latency  , data=d_all)
modg_2 <- lm(as.numeric(covid_government) ~ anger + treatment + latency, data=d_all)
modg_3 <- lm(as.numeric(covid_government) ~ anger*vote + treatment + latency  , data=d_all)


modj_1 <-lm(as.numeric(covid_job) ~ anger + treatment + vote + latency  , data=d_all)
modj_2 <- lm(as.numeric(covid_job) ~ anger + treatment + latency, data=d_all)
modj_3 <- lm(as.numeric(covid_job) ~ anger*vote + treatment + latency  , data=d_all)



anger <- lm(as.numeric(anger) ~ treatment, data=d_all)
anger_h <- lm(as.numeric(anger) ~ treatment, data=d_all%>% filter(vote=="Haddad Voters"))
anger_b <- lm(as.numeric(anger) ~ treatment, data=d_all%>% filter(vote=="Bolsonaro Voters"))


# Tables ------------------------------------------------------------------

names_order <- c("(Intercept)", 
                "angerAnger",
                "latency", 
                "treatmentNegative Bolsonaro", 
                "treatmentNegative Haddad", 
                "treatmentPositive Haddad", 
                "voteHaddad Voters", 
                "voteBolsonaro Voters")


             
var_tw<- c("Anger",
           "Latency", 
           "Negative Bolsonaro", 
           "Negative Haddad", 
           "Positive Haddad", 
           "Haddad Voters", 
           "Bolsonaro Voters")

# 
# "Anger x  Negative Bolsonaro", 
# "Anger x Negative Haddad", 
# "Anger x Positive Haddad",
# "Anger x  Haddad Voters", 
# "Anger x Bolsonaro Voters"



# Table 2 -----------------------------------------------------------------



stargazer(modh_2, modh_1,  
                modj_2, modj_1, 
                  modg_2, modg_1, 
                  order=paste0("^", names_order, "$"),
                  intercept.bottom = FALSE, 
                  dep.var.caption = "",
                  dep.var.labels.include = FALSE,
                  column.labels   = c("Health Risks", "Job Risks", "Support for the Government"),
                  column.separate = c(2, 2, 2), 
                  omit.stat = c("rsq", "f", "ser"), 
                  type = "latex", 
                  title = "Regression Models: Effects of Anger on Risk and Support for the Government", 
                  label="autopsy",  
                  covariate.labels = c(var_tw),
                  out=here("outputs","table_2.tex"),  
                  notes.align = "l")



# Table 8
stargazer(anger, 
          anger_b, 
          anger_h, 
          intercept.bottom = FALSE, 
          dep.var.caption = "",
          dep.var.labels.include = FALSE,
          column.labels   = c("All Sample", "Bolsonaro Voters", "Haddad Voters"),
          column.separate = c(1, 1, 1), 
          omit.stat = c("rsq", "f", "ser"), 
          type = "latex", 
          title = "Regression Models: Effects of Anger on Risk and Support for the Government", 
          label="autopsy",  
          covariate.labels = c("Constant","Negative Bolsonaro", "Negative Haddad", "Positive Haddad"), 
          out=here("outputs", "table_8.tex"),  
          notes.align = "l")






