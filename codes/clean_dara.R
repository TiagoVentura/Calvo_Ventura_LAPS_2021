# clean the data


load("./data/brazil_cleaned_data.Rdata")


# Levels for Covid --------------------------------------------------------
levels_covid <- c( "Somewhat Likely"="Algo provável", 
                   "Very Likely"= "Muito provável" , 
                   "Somewhat unlikely" = "Pouco provável", 
                   "Very unlikely" = "Nada provável", 
                   "Somewhat Appropriate" = "Algo adequada",
                   "Very Appropriate" = "Muito adequada", 
                   "Somewhat Unappropriate" = "Pouco adequada",
                   "Not Appropriate" = "Nada adequada")



# Correct Data ------------------------------------------------------------

d <- d %>% 
  mutate_at(vars(covid_job, covid_health, covid_government, runoff_bolsonaro, 
                 runoff_nulo, runoff_haddad, 
                 negative_partisanship, positive_partisanship, age, income, work, education, gender), 
            ~ifelse(.x=="-999", NA, .x))


covid <- d %>% select(trust_exp, runoff_bolsonaro, runoff_haddad, vote_haddad,
                      vote_bolsonaro, 
                      runoff_nulo, ideo_place_self, 
                      positive_partisanship, negative_partisanship, contains("covid"), 
                      contains("treat"), contains("control"),
                      -contains("click"), -contains("page_submit"), 
                      income, age, gender, work, education, 
                      startdate, enddate, treat_reaction_page_submit) 


# Capture the treatment
treatment_framing <- covid %>% 
  mutate_at(vars(c(contains("treat"), contains("control"))), ~
              ifelse(is.na(.x), 0, .x)) %>% 
  mutate(treat_negative = treat_negative_bolsonaro+treat_negative_haddad + 
           control_negative_bolsonaro + control_negative_haddad, 
         treatment_haddad= treat_negative_haddad+ treat_neutral_haddad+ 
           control_negative_haddad+ control_neutral_haddad, 
         treatment_bolsonaro = treat_negative_bolsonaro + treat_neutral_bolsonaro+ 
           control_negative_bolsonaro+ control_neutral_bolsonaro, 
         negative_bolsonaro = treat_negative_bolsonaro + control_negative_bolsonaro, 
         neutral_bolsonaro= treat_neutral_bolsonaro + control_neutral_bolsonaro, 
         negative_haddad = treat_negative_haddad + control_negative_haddad, 
         neutral_haddad= treat_neutral_haddad + control_neutral_haddad)

treatment_framing <- treatment_framing %>% select(-treat_negative_bolsonaro, -treat_negative_haddad, 
                                                    -control_negative_bolsonaro, -control_negative_haddad, 
                                                  -treat_neutral_bolsonaro, -treat_neutral_haddad , 
                                                    -control_neutral_bolsonaro ,  -control_neutral_haddad)

# fix the dv
treatment_framing <- treatment_framing %>% 
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

# Adjust Covariates

list_educ <- c( "Sem instrução",
                "Primário Completo",
                "Segundo Grau/Ensino Médio Incompleto",
                "Segundo Grau/Ensino Médio Completo"  ,
                "Ensino Superior Incompleto",
                "Ensino Superior Completo",
                "Pós-graduação")

# relevel

treatment_framing <- treatment_framing %>%
  mutate(education=fct_relevel(education, list_educ), 
         gender=fct_recode(gender, "Man"="Masculino"), 
         work=fct_recode(work, "Employed"="Sim", "Unemployed"="Não", 
                         "Unemployed"="Não sei"), 
         age=as.factor(age))   

table(treatment_framing$work)
levels(treatment_framing$education)
levels(treatment_framing$gender)
levels(treatment_framing$work)
levels(treatment_framing$education)
levels(as.factor(treatment_framing$age))
as.numeric((treatment_framing$age))

d <-treatment_framing
save(d, file=here("data/CV_data.Rdata"))
colnames(d)
