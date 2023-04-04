#Script to automate the local sensitivity analysis for the collapse model

# Simulation --------------------------------------------------------------


## Simulation Libraries ----------------------------------------------------------------
#Simple data manipulation
library(dplyr)
library(stringr) #String tidying for setup file reading

#Netlogo interface and futures for running in parallel
library(nlrx)
library(future)
library(tictoc)

## NL setup ----------------------------------------------------------------
#Where netlogo exe is stored...
netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")

#The model that I am running... 
modelpath <- file.path("D:/colony_dynamics/collapse.nlogo")

#Where my results from the nrlx run will be stored...
outpath <- file.path("D:/colony_dynamics/output/local_sensitivity_analysis")

# Setup nl object - this initialises an instance of NetLogo
collapse_nl <- nl(nlversion = "6.2.2",
                  nlpath = netlogopath,
                  modelpath = modelpath,
                  jvmmem = 5000)

## Variable creation ----------------------------------------------------
# Creating a list of variables defaults to feed into the experiment one line at a time.
default_ls <- list(
  #Actual testing variables
  #Island setup controls
  #"initialisation-data" = "\"./data/consistency_analysis/two_isl_baseline.csv\"",
  
  #System controls
  "isl-att-curve" = "\"beta2\"",
  "nhb-rad" = 4,
  
  #Island parameters
  "clust-radius" = 10,
  "prop-suitable" = 0.5,
  "habitat-aggregation" = 0.5,
  "low-lambda" = 50,
  "high-lambda" = 150,
  "diffusion-prop" = 0.5,
  
  #Habitat controls
  "burrow-attrition-rate" = 0.1,
  "patch-burrow-minimum" = 5,
  
  "time-to-prospect" = 2,
  "patch-burrow-limit" = 150,
  
  "collapse-half-way" = 200,
  "collapse-perc" = 0.20,
  "collapse-perc-sd" = 0.05,
  
  #Mortality variables
  "predator-arrival" = 50,
  
  "chick-mortality" = 0.30,
  "chick-mortality-sd" = 0.05,
  "chick-predation" = 0.5,
  
  "juvenile-mortality" = 0.55,
  "juvenile-mortality-sd" = 0.1,
  
  "adult-mortality" = 0.05,
  "adult-mortality-sd" = 0.01,
  "adult-predation" = 0.05,
  
  "enso-breed-impact" = "\"[0.2 0.1 0 0.1 0.2]\"",
  "enso-adult-mort" = "\"[0.02 0.01 0 0.01 0.02]\"",
  
  "max-age" = 20,
  "old-mortality" = 0.5,
  
  #Breeding
  "starting-juvenile-population" = 40000,
  "starting-adult-population" = 20000,
  "age-at-first-breeding" = 4,
  "female-philopatry" = 0.95,
  "prop-returning-breeders" = 0.9,
  "max-tries" = 5,
  
  #Emigration controls
  "emigration-timer" = 5,
  "emigration-max-attempts" = 2,
  "emig-out-prob" = 0.5,
  "emigration-curve" = 0.5,
  "raft-half-way" = 250,
  
  #These are never changed....
  #Data export controls
  "behav-output-path" = "\"./output/local_sensitivity_analysis/\""
)


#LSA list - each variable +/- 10% which will be drawn from 1 at a time during the for loop


#LSA list - each variable +/- 10% which will be drawn from 1 at a time during the for loop
lsa_ls <- list(
  #Actual testing variables
  #Island setup controls
  
  #System controls
  "isl-att-curve" = list(values = rep(c("\"beta2\"", "\"beta1\"", "\"uniform\""), 6)),
  "nhb-rad" = list(values = rep(c(3, 5), 9)),
  
  #Island parameters
  "clust-radius" = list(values = rep(c(9, 11),9)),
  "prop-suitable" = list(values = rep(c(0.45, 0.55),9)),
  "habitat-aggregation" = list(values = rep(c(0.45, 0.55),9)),
  "low-lambda" = list(values = rep(c(45, 55), 9)),
  "high-lambda" = list(values = rep(c(135, 165)), 9),
  "diffusion-prop" = list(values = rep(c(0.45, 0.55),9)),
  
  #Habitat controls
  "burrow-attrition-rate" = list(values = rep(c(0.09, 0.11),9)),
  "patch-burrow-minimum" = list(values = rep(c(4, 6),9)),
  
  "time-to-prospect" = list(values = rep(c(1, 3),9)),
  "patch-burrow-limit" = list(values = rep(c(135, 165)),9),
  
  "collapse-half-way" = list(values = rep(c(180, 220),9)),
  "collapse-perc" = list(values = rep(c(0.18, 0.22),9)),
  "collapse-perc-sd" = list(values = rep(c(0.045, 0.055),9)),
  
  #Mortality variables
  "predator-arrival" = list(values = rep(c(45, 55),9)),
  
  "chick-mortality" = list(values = rep(c(0.27, 0.33),9)),
  "chick-mortality-sd" = list(values = rep(c(0.045, 0.055),9)),
  "chick-predation" = list(values = rep(c(0.45, 0.55),9)),
  
  "juvenile-mortality" = list(values =rep( c(0.495, 0.605),9)),
  "juvenile-mortality-sd" = list(values = rep(c(0.09, 0.11),9)),
  
  "adult-mortality" = list(values = rep(c(0.045, 0.055),9)),
  "adult-mortality-sd" = list(values = rep(c(0.09, 0.011),9)),
  "adult-predation" = list(values = rep(c(0.045, 0.055),9)),
  
  "enso-breed-impact" = list(values = rep(c(
    "\"[0.18 0.1 0 0.1 0.2]\"",
    "\"[0.2 0.9 0 0.1 0.2]\"",
    "\"[0.2 0.1 0 0.9 0.2]\"",
    "\"[0.2 0.1 0 0.1 0.18]\"",
    
    "\"[0.22 0.1 0 0.1 0.2]\"",
    "\"[0.2 0.11 0 0.1 0.2]\"",
    "\"[0.2 0.1 0 0.11 0.2]\"",
    "\"[0.2 0.1 0 0.1 0.22]\""),2)),
  
  "enso-adult-mort" = list(values = rep(c(
    "\"[0.018 0.01 0 0.01 0.02]\"",
    "\"[0.02 0.009 0 0.01 0.02]\"",
    "\"[0.02 0.01 0 0.009 0.02]\"",
    "\"[0.02 0.01 0 0.01 0.018]\"",
    
    "\"[0.022 0.01 0 0.01 0.02]\"",
    "\"[0.02 0.011 0 0.01 0.02]\"",
    "\"[0.02 0.01 0 0.011 0.02]\"",
    "\"[0.02 0.01 0 0.01 0.022]\""),2)),
  
  "max-age" = list(values = rep(c(18, 22),9)),
  "old-mortality" = list(values = rep(c(0.45, 0.55),9)),
  
  #Breeding controls
  
  #Breeding
  "starting-juvenile-population" = list(values = rep(c(18000, 22000),9)),                       
  "starting-adult-population" = list(values = rep(c(36000, 44000),9)),
  "age-at-first-breeding" = list(values = rep(c(3, 5),9)),
  "female-philopatry" = list(values = rep(c(0.855,1),9)),
  "prop-returning-breeders" = list(values = rep(c(0.81, 0.99),9)),
  "max-tries" = list(values = rep(c(4, 6),9)),
  
  
  #Emigration controls
  "emigration-timer" = list(values = rep(c(4, 5, 6),9)),
  "emigration-max-attempts" = list(values = rep(c(1, 3),9)),
  "emig-out-prob" = list(values = rep(c(0.45, 0.5, 0.55),9)),
  "emigration-curve" = list(values = rep(c(0.45, 0.5, 0.55), 9)),
  "raft-half-way" = list(values = rep(c(225, 275), 9))
)

lsa_ls <- list(
  #Actual testing variables
  #Island setup controls
  
  #System controls
  "isl-att-curve" = list(values = c("\"beta2\"", "\"beta1\"", "\"uniform\"")),
  "nhb-rad" = list(values = c(3, 5)),
  
  #Island parameters
  "clust-radius" = list(values = c(9, 11)),
  "prop-suitable" = list(values = c(0.45, 0.55)),
  "habitat-aggregation" = list(values = c(0.45, 0.55)),
  "low-lambda" = list(values = c(45, 55)),
  "high-lambda" = list(values = c(135, 165)),
  "diffusion-prop" = list(values = c(0.45, 0.55)),
  
  #Habitat controls
  "burrow-attrition-rate" = list(values = c(0.09, 0.11)),
  "patch-burrow-minimum" = list(values = c(4, 6)),
  
  "time-to-prospect" = list(values = c(1, 3)),
  "patch-burrow-limit" = list(values = c(135, 165)),
  
  "collapse-half-way" = list(values = c(180, 220)),
  "collapse-perc" = list(values = c(0.18, 0.22)),
  "collapse-perc-sd" = list(values = c(0.045, 0.055)),
  
  #Mortality variables
  "predator-arrival" = list(values = c(45, 55)),
  
  "chick-mortality" = list(values = c(0.27, 0.33)),
  "chick-mortality-sd" = list(values = c(0.045, 0.055)),
  "chick-predation" = list(values = c(0.45, 0.55)),
  
  "juvenile-mortality" = list(values = c(0.495, 0.605)),
  "juvenile-mortality-sd" = list(values = c(0.09, 0.11)),
  
  "adult-mortality" = list(values = c(0.045, 0.055)),
  "adult-mortality-sd" = list(values = c(0.09, 0.011)),
  "adult-predation" = list(values = c(0.045, 0.055)),
  
  "enso-breed-impact" = list(values = c(
    "\"[0.18 0.1 0 0.1 0.2]\"",
    "\"[0.2 0.9 0 0.1 0.2]\"",
    "\"[0.2 0.1 0 0.9 0.2]\"",
    "\"[0.2 0.1 0 0.1 0.18]\"",
    
    "\"[0.22 0.1 0 0.1 0.2]\"",
    "\"[0.2 0.11 0 0.1 0.2]\"",
    "\"[0.2 0.1 0 0.11 0.2]\"",
    "\"[0.2 0.1 0 0.1 0.22]\"")),
  
  "enso-adult-mort" = list(values = c(
    "\"[0.018 0.01 0 0.01 0.02]\"",
    "\"[0.02 0.009 0 0.01 0.02]\"",
    "\"[0.02 0.01 0 0.009 0.02]\"",
    "\"[0.02 0.01 0 0.01 0.018]\"",
    
    "\"[0.022 0.01 0 0.01 0.02]\"",
    "\"[0.02 0.011 0 0.01 0.02]\"",
    "\"[0.02 0.01 0 0.011 0.02]\"",
    "\"[0.02 0.01 0 0.01 0.022]\"")),
  
  "max-age" = list(values = c(18, 22)),
  "old-mortality" = list(values = c(0.45, 0.55)),
  
  #Breeding controls
  
  #Breeding
  "starting-juvenile-population" = list(values = c(18000, 22000)),                       
  "starting-adult-population" = list(values = c(36000, 44000)),
  "age-at-first-breeding" = list(values = c(3, 5)),
  "female-philopatry" = list(values = c(0.855,1)),
  "prop-returning-breeders" = list(values =  c(0.81, 0.99)),
  "max-tries" = list(values = c(4, 6)),
                                      

  #Emigration controls
                                        "emigration-timer" = list(values = c(4, 6)),
                                        "emigration-max-attempts" = list(values = c(1, 3)),
                                        "emig-out-prob" = list(values = c(0.45, 0.5, 0.55)),
                                        "emigration-curve" = list(values = c(0.45, 0.5, 0.55)),
                                        "raft-half-way" = list(values = c(225, 275))
  )
  
  
  
  #Check that these match-up!
  if(any(names(default_ls[1:(length(lsa_ls))]) != names(lsa_ls))){
    stop("!!VARIABLE LISTS DO NO MATCH!!")
  }
  
  
  ## LSA experiment setup ----------------------------------------------------
  #Creating an empty list to fill with the LSA results.
  lsa_results <- list()
  
  #A for loop to iterate over the possible combinations with one variable being moved +/- 10% each time, while all others are held at their default (i.e. best guess)
  
  #Bench marking
  tic("LSA experiment loop")
  
  for(i in seq_along(lsa_ls)){
    
    #Saving the name of the variable that is being altered
    variable_name <- names(lsa_ls)[i]
    
    # #Creating the basis for the nlrx-id to be the name of the variable that is changing to make it easier to match these with the meta-results list and find all the run names. Have to bind on the \ and " because NL can't directly read strings, unless they are simple true/false.
    # default_ls[["nlrx-id"]] <- variable_name  %>%
    #   paste0("\"", ., "\"")
    # # 
    #Finding the number of values that are being iterated over 
    param_length <- length(lsa_ls[[i]]$values)
    
    #Setting up the nlrx experiment
    collapse_nl@experiment <- experiment(
      expname = paste0("lsa_", variable_name),
      outpath = "./",
      repetition = 1,
      tickmetrics = "false",
      idsetup = c("setup"),
      idgo = "step",
      runtime = 600,
      evalticks = c(600),
      stopcond = "not any? turtles",
      variables = lsa_ls[i],
      constants = default_ls[-i],
      idfinal = "behav-csv",
      #The nlrx_id will need to be updated on the basis of the list to give them unique identifiers
      idrunnum = "nlrx-id"
    )
    
    
    ## Simulation design -------------------------------------------------------
    #Creating the simulation design - the nseeds is the number of replicates to do of each parameter set.
    collapse_nl@simdesign <- simdesign_distinct(nl = collapse_nl,
                                                nseeds = 100)
    
    #Pre-flight checks...
    #print(collapse_nl)
    #eval_variables_constants(collapse_nl)
    #Nothing should be defined for either output-file-name (this is a manual trigger when using the NetLogo Interface) or nlrx-id - this widget get's filled by nlrx while experimenting
    
    ## Simulation run -----------------------------------------------------------
    
    #Setting up parallelisation
    plan(multisession)
    
    #Put in the number of cores that it is possible to use (in this case my machine have 12 cores and I leave two open for other processing)
    ncores <- max(which((param_length %% (1:18)) == 0))
    
    #Setting up the progress bar to keep track of how far the run is
    progressr::handlers("progress")
    lsa_results[[i]] <- progressr::with_progress(
      run_nl_all(collapse_nl,
                 split = ncores)
    )
    
    #Written progress of how many variables have been completed
    cat(paste("Finished analysis of", variable_name, "-", i, "/", length(lsa_ls)))
    
  }
  
  toc()
  
  #(115 hrs)
  
  names(lsa_results) <- names(lsa_ls)
  
  #Saving the list of model results as an RDS
  saveRDS(lsa_results, "./output/local_sensitivity_analysis/lsa_meta_results.rds")

  #Checking the number of unique parameter settings
  length(unlist(lsa_ls))
  #121 unique parameters - 100 seeds, so 10100 runs
  


# LSA Analysis ------------------------------------------------------------

## Analysis Libraries ------------------------------------------------------
#Data manipulations
library(readr) #reading in all files
library(vroom)
library(janitor) #Name amendments
library(dplyr) #Data manipulations
library(tidyr) #Pivoting
library(stringr) #Parsing data
library(zoo) #Calculating cumulative statistics
library(forcats)

#Graphing
library(ggplot2) #Plots
library(viridis) #Colour scheme 

library(tictoc)

#Custom functions
#https://stackoverflow.com/questions/52459711/how-to-find-cumulative-variance-or-standard-deviation-in-r
source("./scripts/functions/cumvar.r")

## Reading in data ---------------------------------------------------------

# Reading in files
# tic("Reading individual files")
# 
# lsa_res_vr <- list.files("../output/local_sensitivity_analysis/",
#                         pattern = ".csv",
#                         full.names = TRUE) %>%
#   vroom() %>%
# 
#   # #tidying initialisation names for ease of handling
#   # mutate(initialisation_data = str_remove(
#   #   str_remove(initialisation_data,
#   #              "./data/local_sensitivity_analysis/"),
#   #   ".csv")) %>%
#   #
#   #Creating a new column to keep track of the tick the run was up to
#   group_by(run_id) %>%
# 
#   #Adding in ticks to sheet
#   mutate(ticks = 1:n()) %>%
# 
#   ungroup()
# 
# toc()
# #Reading in individual files took 183.5 seconds
# 
# # #Writing a single large file with some minor manipulations to make it quicker to read in and access
# vroom_write(lsa_res_vr,
#           "./output/local_sensitivity_analysis/lsa_allrun_data.csv",
#           col_names = TRUE)

#Meta information 
lsa_meta_ls <- readRDS("./output/local_sensitivity_analysis/lsa_meta_results_named.rds")

#Merging to a single df
lsa_meta_df <- bind_rows(lsa_meta_ls, .id = "varying_param") %>% 
  select(-c(`[run number]`, `[step]`, siminputrow)) 

#Changing names to for ease of typing
names(lsa_meta_df) <- str_replace_all(names(lsa_meta_df), "[-]", "_")

#Removing the file paths from the file names to reduce it to the variable that is changed and tidying parameter names for consistency
lsa_meta_df <- lsa_meta_df %>% 
  mutate(varying_param = str_replace_all(varying_param, "-", "_"),
         clust_radius = pi * clust_radius^2, 
         input_change = NA,
         state = "") %>% 
  as.data.frame()


#Creating a vector indicating whether the varying parameter is higher or lower than the default
#Creating vectors of each type of enso response (up or down... for the case_when as these are problematic characters)


source("./scripts/enso_baselines.r")

#Creating a variable indicating whether the varying parameter has gone up or down by 10%

#Creating 
var_nl_name <- names(default_ls)
var_r_name <- str_replace_all(names(default_ls), "-", "_")

for(i in 2:(length(var_r_name) - 1)){
  
  #Pulling out each variable in the dataframe that varied via sliding parameter(does not work for the intitialisation parameters)
  meta_var <- lsa_meta_df[lsa_meta_df$varying_param == var_r_name[i],] %>% 
    select(var_r_name[i])
  
  if(is.numeric(meta_var[1,])){
    #Checking if it is higher or lower than the default for numeric elements
    var_state <- ifelse(meta_var > default_ls[[var_nl_name[[i]]]], "Up", 
                         ifelse(meta_var < default_ls[[i]], "Down", "Default"))
    
    if(var_r_name[i] != "clust_radius"){
      
      input_change <- abs(meta_var[,1] - default_ls[[var_nl_name[[i]]]]) / default_ls[[var_nl_name[[i]]]]
    }
    
    if(var_r_name[i] == "clust_radius"){
      input_change <- abs(meta_var[,1] - (pi*10^2)) / (pi*10^2) 
    }

    
    #Overwriting the status for that parameter
    lsa_meta_df[lsa_meta_df$varying_param == var_r_name[i],]$state <- var_state
    
    #Overwriting the sensitivity index for that parameter
    lsa_meta_df[lsa_meta_df$varying_param == var_r_name[i],]$input_change <- input_change
    
  }
  
  #The ENSO variables - these are stored as strings but are actually numbers underneath the hood
  if(var_r_name[i] == "enso_adult_mort"){
    var_state <- ifelse(meta_var[,1] %in% enso_ad_up, "Up", 
                        ifelse(meta_var[,1] %in% enso_ad_down, "Down", "Default"))
    #Overwriting the status for that parameter
    lsa_meta_df[lsa_meta_df$varying_param == var_r_name[i],]$state <- var_state
  }
  
  if(var_r_name[i] == "enso_breed_impact"){
    var_state <- ifelse(meta_var[,1] %in% enso_chick_up, "Up", 
                        ifelse(meta_var[,1] %in% enso_chick_down, "Down", "Default"))
    
    #Overwriting the status for that parameter
    lsa_meta_df[lsa_meta_df$varying_param == var_r_name[i],]$state <- var_state
  }
  
  if(var_r_name[i] == "isl_att_curve"){
    
    #Overwriting the status for that parameter
    lsa_meta_df[lsa_meta_df$varying_param == var_r_name[i],]$state 
  }
}  


#Creating a vector of variable names for the variables that were changed by 10% BUT either coded as character strings or read in via csv making them problematic to manipulate. This vector is used in the following mutate to specify the variable_ratio.
ten_perc_vars <- c("enso_adult_mort", "enso_breed_impact")

#Correcting the file name values
lsa_meta_df <- lsa_meta_df %>% 
  mutate(input_change = replace(input_change, varying_param %in% ten_perc_vars & state != "Default", 0.1),
         input_change = replace(input_change, varying_param %in% ten_perc_vars & state == "Default", 0))

#Reading in combined files
lsa_50ysum_df <- vroom("./output/local_sensitivity_analysis/lsa_allrun_data.csv") %>% 
  #Reducing this to just the last 50 years of data to summarise over.
  filter(ticks >= 550) %>% 
  #Joining on the meta data
  left_join(lsa_meta_df, 
            by = c("run_id" = "nlrx_id")) %>% 
  pivot_longer(cols = starts_with("settled_"),
               names_to = "island_id",
               values_to = "adult_count") %>% 
  group_by(run_id, island_id, varying_param, input_change, state) %>% 
  summarise(adult_mean = mean(adult_count),
            adult_sd = sd(adult_count),
            .groups = "keep") %>% 
  #Calculating the proportional change in each variable
  mutate(baseline_abund = ifelse(island_id == "settled_ct_isl_1", 135, 39322),
         output_change = abs(adult_mean - baseline_abund) / baseline_abund,
         proportional_change = output_change / input_change)

# #Plotting the proportional change
# ggplot(filter(lsa_50ysum_df, state != "Default" & !is.na(state)), 
#               aes(y = proportional_change, x = state)) +
#   
#   geom_boxplot() + 
#   
#   geom_point() +  
#   
#   geom_hline(yintercept = 1, linetype = "dashed", colour = "blue") + 
#   geom_hline(yintercept = 1.5, linetype = "dotdash", colour = "red") + 
#   facet_wrap(~varying_param) + 
#   
#  labs(y = "Proportional Change", x = "Variable State") + 
#   
#   
#   
#   theme(axis.text = element_text(size = 14),
#         axis.title = element_text(size = 16),
#         
#         legend.title = element_text(size = 16),
#         legend.text = element_text(size = 14))
#   
#   
  #Plotting the proportional change
lsa_50ymedian_df <- lsa_50ysum_df %>% 
    
    #Reducing it to just up/down
    filter(state != "Default" & !is.na(state)) %>% 
    
    #Recoding for tidyness
    mutate(state_tidy = ifelse(state == "Up", 
                               "Increased", 
                               "Decreased")) %>% 
    #Calculating mean change
    group_by(state_tidy, varying_param) %>% 
    
    summarise(median_prop_change = median(proportional_change, na.rm = TRUE)) %>%
    
    ungroup() %>% 
    
    mutate(parameter = fct_recode(varying_param,
                                  "Initial Adult Population" = "starting_adult_population",
                                  "Initial Juvenile Population" = "starting_juvenile_population",
                     "Adult Mortality (mean)" = "adult_mortality",
                     "Adult Mortality (s.d.)" = "adult_mortality_sd",
                     "Adult Predation (mean)" = "adult_predation",
                     "Juvenile Mortality Predation (mean)" = "juvenile_mortality",      
                     "Juvenile Mortality Predation (s.d.)" = "juvenile_mortality_sd",  
                     
                     "Breeding Age" = "age_at_first_breeding",
                     "Burrow Attrition Rate" = "burrow_attrition_rate",
                     "Chick Mortality (mean)" = "chick_mortality",
                     "Chick Mortality (s.d.)" = "chick_mortality_sd",
                     "Chick Predation (mean)" = "chick_predation",
                     "Island Radius" = "clust_radius",
                     "Collapse 50% Threshold" = "collapse_half_way",
                     "Percent Burrows Collapse (mean)" = "collapse_perc",
                     "Percent Burrows Collapse (s.d.)" =  "collapse_perc_sd",
                     "Initial Burrow Diffusion (%)" = "diffusion_prop",          
                     "Emigrate out of System (%)" = "emig_out_prob",
                     "Emigration Curve Steepness" = "emigration_curve",
                     "Max Intra-system Emigration Attempts" = "emigration_max_attempts",
                     "Failed Breeding Attempts before Emigration" = "emigration_timer",
                     "Adult ENSO Mortality (mean)" = "enso_adult_mort",
                     "Chick ENSO Mortality (mean)" = "enso_breed_impact",
                     "Natal Philopatry Probability (mean)" = "female_philopatry",
                     "Initial Aggregation of Quality habitat" = "habitat_aggregation",
                    "Initial Burrow Count of High Quality Habitat" = "high_lambda",
                    
                    "Initial Burrow Count of Low Quality Habitat" = "low_lambda",
                    "Age at Senecense" = "max_age",
                    "Maximum Courtship Attempts" = "max_tries",
                    "Neighbourhood Radius" = "nhb_rad",
                    "Mortality at Senecence" = "old_mortality",
                    "Patch Burrow Upper Limit" =   "patch_burrow_limit", 
                    "Patch Burrow Upper Limit" = "patch_burrow_minimum",
                    "Proportion of Returning Breeders" = "prop_returning_breeders",
                    "Proportion of Suitable Habitat on Island" = "prop_suitable",
                    "Individual count for 50% Emigration Chance" = "raft_half_way",
                    "Burrow Prospecting Time" = "time_to_prospect"),
           parameter = fct_reorder(.f = parameter,
                                   .x = median_prop_change,
                                   .fun = mean,
                                   .desc = TRUE)) 
  
    
    
#Plotting the sensitivity of parameters
ggplot(lsa_50ymedian_df, 
       aes(y = median_prop_change, 
           x = parameter)) +
    
    geom_point(aes(colour = state_tidy),
               size = 2) +  
    
    scale_colour_manual(values = c("#f5840e",
                                   "#007F5C"),
                        name = "Parameter State") +

    geom_hline(yintercept = 1, 
               linetype = "solid", 
               colour = "black") + 
  
    geom_hline(yintercept = 1.5, 
               linetype = "dotdash", 
               colour = "red") + 
    
    labs(y = "Median Proportional Change", 
         x = "Parameter") +
  
    coord_flip() +
    
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))
  
#ggsave("./graphs/local_sensitivity_analysis/median_prop_change.png")

#Summary of proportional change by variable
prop_change_summ <- lsa_50ysum_df %>% 
  filter(state != "Default") %>% 
  group_by(varying_param, state) %>% 
  summarise(mean_prop_change = round(mean(proportional_change, na.rm = TRUE), digits = 2),
            sd_prop_change = round(sd(proportional_change, na.rm = TRUE), digits = 2),
            median_prop_change = round(median(proportional_change, na.rm = TRUE), digits = 2),
            upper_prop_quartile = round(quantile(proportional_change, probs = 0.75, na.rm = TRUE), digits = 2),
            lower_prop_quartile = round(quantile(proportional_change, probs = 0.25, na.rm = TRUE), digits = 2))
  
prop_change_summ
View(prop_change_summ)

# #Predator free island
# lsa_50ysum_df %>%
#   filter(island_id == "settled_ct_isl_2" & varying_param != "baseline") %>%
# ggplot(aes(x = state, y = adult_mean,
#            colour = state)) +
#   geom_violin() +
#   geom_point() +
#   scale_colour_viridis_d() +
#   facet_wrap(~varying_param) +
#   ggtitle("Predator Free Island") +
#   theme(
#     #axis.text.x = element_text(angle = 90)
#     )
# #Predator Island
# lsa_50ysum_df %>% 
#   filter(island_id == "settled_ct_isl_1" & varying_param != "baseline") %>% 
#   ggplot(aes(x = state, y = adult_mean,
#              colour = state)) + 
#   geom_violin() +
#   geom_point() +
#   scale_colour_viridis_d() +
#   facet_wrap(~varying_param) +
#   ggtitle("PredatorIsland") +
#   theme(
#     #axis.text.x = element_text(angle = 90)
#   )

#Merging in consistency data
consist_50y_sum <- structure(list(island_id = c("settled_ct_isl_1", "settled_ct_isl_2"
), adult_mean = c(71, 23576), adult_sd = c(96, 4877)), 
class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA,-2L))


lsa_50ysum_df %>% 
  filter(island_id == "settled_ct_isl_2" & varying_param != "baseline") %>% 
  mutate(adult_mean = adult_mean - consist_50y_sum[2,]$adult_mean) %>% 
  ggplot(aes(x = state, y = adult_mean,
             colour = state)) + 
  
  geom_violin() +
  geom_boxplot(width = 0.25) + 
  geom_point() +
  
  #Multiplying the sd by two to give ~ 95% confidence limits on the range
  geom_hline(yintercept = (consist_50y_sum[2,]$adult_sd * 2),
             linetype = "dotdash") +
  geom_hline(yintercept = (consist_50y_sum[2,]$adult_sd *2) * -1,
             linetype = "dotdash") +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "red") +
  
  scale_colour_viridis_d() +
  
  facet_wrap(~varying_param, scales = "free") +
  ggtitle("Predator Free Island") +
  theme()


lsa_50ysum_df %>% 
  filter(island_id == "settled_ct_isl_1" & varying_param != "baseline") %>% 
  mutate(adult_mean = adult_mean - consist_50y_sum[1,]$adult_mean) %>% 
  ggplot(aes(x = state, y = adult_mean,
             colour = state)) + 
  
  geom_violin() +
  geom_boxplot(width = 0.25) + 
  geom_point() +
  
  #Multiplying the sd by two to give ~ 95% confidence limits on the range
  geom_hline(yintercept = (consist_50y_sum[1,]$adult_sd * 2),
             linetype = "dotdash") +
  geom_hline(yintercept = (consist_50y_sum[1,]$adult_sd *2) * -1,
             linetype = "dotdash") +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "red") +
  
  scale_colour_viridis_d() +
  
  facet_wrap(~varying_param, scales = "free") +
  ggtitle("Predator Island") +
  theme(
    #axis.text.x = element_text(angle = 90)
  )



## Island attractiveness ---------------------------------------------------
#Reading in combined files
isl_att_df <- vroom("./output/local_sensitivity_analysis/lsa_allrun_data.csv") %>% 
  #Reducing this to just the last 50 years of data to summarise over.
  filter(ticks == 450) %>% 
  #Joining on the meta data
  left_join(lsa_meta_df, 
            by = c("run_id" = "nlrx_id")) %>% 
  pivot_longer(cols = starts_with("settled_"),
               names_to = "island_id",
               values_to = "adult_count") %>% 
  filter(varying_param == "isl_att_curve") %>% 
  mutate(state = isl_att_curve) %>% 
  group_by(run_id, island_id, isl_att_curve) %>% 
  summarise(adult_mean = mean(adult_count),
            adult_sd = sd(adult_count),
            .groups = "keep")

#Predator free island
isl_att_df %>% 
  filter(island_id == "settled_ct_isl_2") %>% 
  ggplot(aes(x = isl_att_curve, y = adult_mean - consist_50y_sum[2,]$adult_mean,
             colour = isl_att_curve)) + 
  geom_violin() +
  geom_boxplot(width = 0.25) + 
  geom_point() +
  
  #Multiplying the sd by two to give ~ 95% confidence limits on the range
  geom_hline(yintercept = (consist_50y_sum[2,]$adult_sd * 2),
             linetype = "dotdash") +
  geom_hline(yintercept = (consist_50y_sum[2,]$adult_sd *2) * -1,
             linetype = "dotdash") +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "red") +
  
  scale_colour_viridis_d() +
  
  ggtitle("Predator Free Island") +
  theme()


#Predator free island
isl_att_df %>% 
  filter(island_id == "settled_ct_isl_1") %>% 
  ggplot(aes(x = isl_att_curve, y = adult_mean - consist_50y_sum[1,]$adult_mean,
             colour = isl_att_curve)) + 
  geom_violin() +
  geom_boxplot(width = 0.25) + 
  geom_point() +
  
  #Multiplying the sd by two to give ~ 95% confidence limits on the range
  geom_hline(yintercept = (consist_50y_sum[1,]$adult_sd * 2),
             linetype = "dotdash") +
  geom_hline(yintercept = (consist_50y_sum[1,]$adult_sd *2) * -1,
             linetype = "dotdash") +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "red") +
  
  scale_colour_viridis_d() +
  
  ggtitle("Predator Island") +
  theme()



## ENSO response -----------------------------------------------------------

## ENSO breeding impact ----------------------------------------------------
#Reading in combined files
enso_breed_df <- vroom("./output/local_sensitivity_analysis/lsa_allrun_data.csv") %>% 
  #Reducing this to just the last 50 years of data to summarise over.
  filter(ticks == 450) %>% 
  #Joining on the meta data
  left_join(lsa_meta_df, 
            by = c("run_id" = "nlrx_id")) %>% 
  filter(varying_param == "enso_breed_impact") %>% 
  pivot_longer(cols = starts_with("settled_"),
               names_to = "island_id",
               values_to = "adult_count") %>% 
  
  mutate(enso_state = case_when(
    enso_breed_impact == "[0.45 0.2 0 0.2 0.5]" ~ "La Niña",
    enso_breed_impact == "[0.5 0.18 0 0.2 0.5]" ~ "La Niña Leaning",
    enso_breed_impact == "[0.5 0.2 0 0.18 0.5]" ~ "El Niño Leaning",
    enso_breed_impact == "[0.5 0.2 0 0.2 0.45]" ~ "El Niño",
    
    enso_breed_impact == "[0.5 0.2 0 0.2 0.5]" ~ "Neutral",
    
    enso_breed_impact == "[0.55 0.2 0 0.2 0.5]" ~ "La Niña",
    enso_breed_impact == "[0.5 0.22 0 0.2 0.5]" ~ "La Niña Leaning",
    enso_breed_impact == "[0.5 0.2 0 0.22 0.5]" ~ "El Niño Leaning",
    enso_breed_impact == "[0.5 0.2 0 0.2 0.55]" ~ "El Niño"),
    
  state = case_when(
    enso_breed_impact %in% enso_chick_down ~ "Down",
    enso_breed_impact %in% enso_chick_baseline ~ "Default",
    enso_breed_impact %in% enso_chick_up ~ "Up"),
  
  enso_state = factor(enso_state, levels = c("La Niña", "La Niña Leaning", "Neutral", "El Niño Leaning", "El Niño")),
  state = factor(state, levels = c("Up", "Default", "Down"))) %>% 
  group_by(run_id, island_id, enso_state, state) %>% 
  summarise(adult_mean = mean(adult_count),
            adult_sd = sd(adult_count),
            .groups = "keep")

#Graphing
enso_breed_df %>% 
  filter(island_id == "settled_ct_isl_2") %>% 
  ggplot(aes(x = enso_state, 
             y = adult_mean - consist_50y_sum[2,]$adult_mean,
             colour = state)) + 
  #geom_violin() +
  geom_boxplot(width = 0.25) + 
  geom_point() +
  
  #Multiplying the sd by two to give ~ 95% confidence limits on the range
  geom_hline(yintercept = (consist_50y_sum[2,]$adult_sd * 2),
             linetype = "dotdash") +
  geom_hline(yintercept = (consist_50y_sum[2,]$adult_sd *2) * -1,
             linetype = "dotdash") +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "red") +
  
  scale_colour_viridis_d() +
  
  facet_wrap(~state) + 
  
  ggtitle("Predator Free Island")

#Predator island
enso_breed_df %>% 
  filter(island_id == "settled_ct_isl_1") %>% 
  ggplot(aes(x = enso_state, 
             y = adult_mean - consist_50y_sum[1,]$adult_mean,
             colour = state)) + 
  #geom_violin() +
  geom_boxplot(width = 0.25) + 
  geom_point() +
  
  #Multiplying the sd by two to give ~ 95% confidence limits on the range
  geom_hline(yintercept = (consist_50y_sum[1,]$adult_sd * 2),
             linetype = "dotdash") +
  geom_hline(yintercept = (consist_50y_sum[1,]$adult_sd *2) * -1,
             linetype = "dotdash") +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "red") +
  
  scale_colour_viridis_d() +
  
  facet_wrap(~state) + 
  
  ggtitle("Predator Island")



## ENSO adult mortality ----------------------------------------------------
#Reading in combined files
enso_adult_df <- vroom("./output/local_sensitivity_analysis/lsa_allrun_data.csv") %>% 
  #Reducing this to just the last 50 years of data to summarise over.
  filter(ticks == 450) %>% 
  #Joining on the meta data
  left_join(lsa_meta_df, 
            by = c("run_id" = "nlrx_id")) %>% 
  filter(varying_param == "enso_adult_mort") %>% 
  pivot_longer(cols = starts_with("settled_"),
               names_to = "island_id",
               values_to = "adult_count") %>% 
  mutate(enso_state = case_when(
    enso_adult_mort == "[0.225 0.1 0 0.1 0.25]" ~ "La Niña",
    enso_adult_mort == "[0.25 0.09 0 0.1 0.25]" ~ "La Niña Leaning",
    enso_adult_mort == "[0.25 0.1 0 0.09 0.25]" ~ "El Niño Leaning",
    enso_adult_mort == "[0.25 0.1 0 0.1 0.225]" ~ "El Niño",
    
    enso_adult_mort == "[0.25 0.1 0 0.1 0.25]" ~ "Neutral",
    
    enso_adult_mort == "[0.275 0.1 0 0.1 0.25]" ~ "La Niña",
    enso_adult_mort == "[0.25 0.11 0 0.1 0.25]" ~ "La Niña Leaning",
    enso_adult_mort == "[0.25 0.1 0 0.11 0.25]" ~ "El Niño Leaning",
    enso_adult_mort == "[0.25 0.1 0 0.1 0.275]" ~ "El Niño"),
    
    state = case_when(
      enso_adult_mort %in% enso_ad_down ~ "Down",
      enso_adult_mort %in% enso_ad_baseline ~ "Default",
      enso_adult_mort %in% enso_ad_up ~ "Up"),
    
    enso_state = factor(enso_state, levels = c("La Niña", "La Niña Leaning", "Neutral", "El Niño Leaning", "El Niño")),
    state = factor(state, levels = c("Up", "Default", "Down"))) %>% 
  
  group_by(run_id, island_id, enso_state, state) %>% 
  
  summarise(adult_mean = mean(adult_count),
            adult_sd = sd(adult_count),
            .groups = "keep")

#Graphing
enso_adult_df %>% 
  filter(island_id == "settled_ct_isl_2") %>% 
  ggplot(aes(x = enso_state, 
             y = adult_mean - consist_50y_sum[2,]$adult_mean,
             colour = state)) + 
  #geom_violin() +
  geom_boxplot(width = 0.25) + 
  geom_point() +
  
  #Multiplying the sd by two to give ~ 95% confidence limits on the range
  geom_hline(yintercept = (consist_50y_sum[2,]$adult_sd * 2),
             linetype = "dotdash") +
  geom_hline(yintercept = (consist_50y_sum[2,]$adult_sd *2) * -1,
             linetype = "dotdash") +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "red") +
  
  scale_colour_viridis_d() +
  
  facet_wrap(~state) + 
  
  ggtitle("Predator Free Island")

#Predator island
enso_adult_df %>% 
  filter(island_id == "settled_ct_isl_1") %>% 
  ggplot(aes(x = enso_state, 
             y = adult_mean - consist_50y_sum[1,]$adult_mean,
             colour = state)) + 
  #geom_violin() +
  geom_boxplot(width = 0.25) + 
  geom_point() +
  
  #Multiplying the sd by two to give ~ 95% confidence limits on the range
  geom_hline(yintercept = (consist_50y_sum[1,]$adult_sd * 2),
             linetype = "dotdash") +
  geom_hline(yintercept = (consist_50y_sum[1,]$adult_sd *2) * -1,
             linetype = "dotdash") +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "red") +
  
  scale_colour_viridis_d() +
  
  facet_wrap(~state) + 
  
  ggtitle("Predator Island")



ggplot(lsa_50ysum_df, aes(y = prop_change, x = state)) +
## Calculating variable-response change ------------------------------------





