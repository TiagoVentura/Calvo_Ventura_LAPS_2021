
## Replication Materials

Replication materials for "Will I Get Covid? Partisanship, Social Media Frames, and Perceptions of Health Risk in Brazil" Ernesto Calvo and Tiago Ventura. Forthcoming in the Latin American Politics and Society. 

> __Abstract:__
> In these polarized and challenging times, not even perceptions of personal risk are immune to partisanship. This paper introduces results from a new survey with an embedded social media experiment conducted during the first months of the COVID-19 pandemic in Brazil. Descriptive results show that pro-government and opposition partisans report very
different expectations of health and job risks. Job and health policy have become wedge issues that elicit partisan responses. We exploit random variation in the survey recruitment to show the effects of the President’s first speech on national TV on perceived risk and the moderating effect of partisanship. We present a framing experiment that models key cognitive mechanisms driving partisan differences in perceptions of health risks and job security during the COVID-19 crisis.

The latest pre-print can be found [here](Covid_social_media_LAPS.pdf). The published version is [here]()

## Tutorial 

This README file provides an overview of the replications materials for the article. The R codes used in the article can be found under the folder Codes. The survey data, which we fully describe below, is under the folder data. Results are exported to the folder output. 

## Codes

- `CV_analysis_descriptive.R`: Implements the first descriptive analysis of the paper. The code replicates figures 1 and 2 of the paper, and figure 6 and table 6 in the SIF. 

- `CV_analysis_diff_in_diff.R`: Replicates the diff-in-diff results for the Speech of Bolsonaro on risk perceptions. (table 1, figures 7, and 8).

- `CV_analysis_experiments.R`: Replicates the results for the social framing experiment, including the section unpacking the null findings. 

## Data

Our paper presents analyses based on observation, quasi-experimental, and a framing experiment using novel data from a national online survey fielded by Netquest-Vanderbilt. The survey uses probabilistic samples drawn by the LAPOP team in Vanderbilt implemented with the panel of users registered with Netquest.  The entire survey and the embedded framing experiment received the approval of the University of Maryland Institutional Board Review 1552091-3. 

The parts of the survey data used in the paper are available under `data/CV_data.Rdata`. We direct the reader to tables 3 and 4 in the appendix for a in-depth description of each variable.  The dataset contains de following variables:

- **runoff_bolsonaro**: Likely to vote for Bolsonaro (runoff)          
- **runoff_haddad**: Likely to vote for Haddad (runoff)
- **vote_haddad**: Likely to Vote for Haddad (First Round)               
- **vote_bolsonaro**: Likely to Vote for Bolsonaro (First Round)        
- **runoff_nulo** : Likely to Vote Null/Blank/Others (Runoff)              
- **ideo_place_self**: Ideological Self-Placement    
- **positive_partisanship**: Which Party do you Like more?     
- **negative_partisanship**:  Which Party do you dislike more? 
- **covid_job**: Job risk assesment due to COVID-19                 
- **covid_health**: Health risk assessment due to COVID-19         
- **covid_government**: Support for how the government is reacting during the pandemic          
- **treat_reaction**: Behavioral Reaction to the Treatment      
- **treat_emotions**: Emotional Reaction to the Treatment            
- **income**: Subjective income measure 
- **age**: Respondents' Age                       
- **gender**: Respondents' Gender   
- **work**: Respondents' Occupational Status
- **education**: Respondents' Education  
- **startdate**: Start date of the Survey Response                 
- **enddate**: End Date of the Survey Response
- **treat_reaction_page_submit**: Lantency Measure to the Treatment
- **treat_negative**: Treatment Negative Tweet  
- **treatment_haddad**: Treatment Haddad Tweet 
- **treatment_bolsonaro**: Treatment Eduardo Bolsonaro Tweet     
- **negative_bolsonaro**: Treatment Negative Edurdo Bolsonaro     
- **neutral_bolsonaro**: Treatment Positive Eduardo Bolsonaro    
- **negative_haddad**: Treatment Negative Haddad           
- **neutral_haddad**: Treatment Positive Haddad




