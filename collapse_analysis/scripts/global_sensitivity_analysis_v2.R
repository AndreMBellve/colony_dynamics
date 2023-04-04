#Script to automate the global sensitivity analysis for the collapse model

# Simulations -------------------------------------------------------------
## Simulation Libraries ----------------------------------------------------------------
#Simple data manipulation
library(dplyr)
library(stringr) #String tidying for setup file reading
library(tictoc)

#Netlogo interface and futures for running in parallel
library(nlrx)
library(future)

## NL setup ----------------------------------------------------------------
#Where netlogo exe is stored...
netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")

#The model that I am running... 
modelpath <- file.path("D:/colony_dynamics/collapse_gsa2.nlogo")

#Where my results from the nrlx run will be stored...
outpath <- file.path("D:/colony_dynamics/output/global_sensitivity_analysis")

# Setup nl object - this initialises an instance of NetLogo
collapse_gsa_nl <- nl(
  nlversion = "6.2.2",
  nlpath = netlogopath,
  modelpath = modelpath,
  jvmmem = 5000
)

## Variable creation ----------------------------------------------------
# Creating a list of variables defaults to feed into the experiment one line at a time.
gsa_default_ls <- list(
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
  
  "collapse-perc" = 0.20,
  "collapse-perc-sd" = 0.05,
  
  #Mortality variables
  "predator-arrival" = 50,
  
  "chick-mortality-sd" = 0.05,
  "chick-predation" = 0.5,
  
  "juvenile-mortality" = 0.55,
  "juvenile-mortality-sd" = 0.1,
  
  "adult-mortality" = 0.05,
  "adult-mortality-sd" = 0.01,
  "adult-predation" = 0.05,
  
  "enso-adult-mort" = "\"[0.02 0.01 0 0.01 0.02]\"",

  
  "max-age" = 20,
  "old-mortality" = 0.5,
  
  #Breeding
  "starting-juvenile-population" = 10000,
  "starting-adult-population" = 20000,
  "age-at-first-breeding" = 4,
  
  #Emigration controls
  "emigration-curve" = 0.5,
  "raft-half-way" = 250,
  "emigration-max-attempts" = 3,
  
  #These are never changed....
  #Data export controls
  "behav-output-path" = "\"./output/global_sensitivity_analysis/\""
)

#gsa list - each variable +/- 10% which will be drawn from 1 at a time during the for loop
gsa_ls <- list(
  
  #Emigration
  "emigration-timer" = list(min = 1, 
                            max = 6, 
                            step = 0.01, 
                            qfun = "qunif"),
  "emig-out-prob" = list(min = 0.20,
                         max = 0.80, 
                         step = 0.01, 
                         qfun = "qunif"),
  "female-philopatry" = list(min = 0.5, 
                             max = 0.99,
                             step = 0.01, 
                             qfun = "qunif"),
  
  #Habitat
  "collapse-half-way" = list(min = 100, 
                             max = 300, 
                             step = 0.01, 
                             qfun = "qunif"),
  
  #Breeding
  "prop-returning-breeders" = list(min = 0.8, 
                                   max = 1, 
                                   step = 0.01, 
                                   qfun = "qunif"),
  "max-tries" = list(min = 2, 
                     max = 8, 
                     step = 0.01, 
                     qfun = "qunif"),

  #Mortality
  "chick-mortality" = list(min = 0.2, 
                           max = 0.6, 
                           step = 0.01, 
                           qfun = "qunif"),
  "LN-chick" = list(min = 0.05,
                    max = 0.5,
                    step = 0.01,
                    qfun = "qunif"),
  "LNL-chick" = list(min = 0.01,
                     max = 0.3,
                     step = 0.01,
                     qfun = "qunif"),
  "ENL-chick" = list(min = 0.01,
                     max = 0.3,
                     step = 0.01,
                     qfun = "qunif"),
  "EN-chick" = list(min = 0.05,
                    max = 0.5,
                    step = 0.01,
                    qfun = "qunif")
  )


## GSA experiment setup ----------------------------------------------------

#Bench marking
tic("GSA experiment")
  
  
#Setting up the nlrx experiment
  collapse_gsa_nl@experiment <- experiment(
    expname = "gsa",
    outpath = outpath,
    repetition = 1,
    tickmetrics = "false",
    idsetup = "setup",
    idgo = "step",
    runtime = 600,
    evalticks = c(600),
    stopcond = "not any? turtles",
    variables = gsa_ls,
    constants = gsa_default_ls,
    idfinal = "behav-csv",
    #The nlrx_id will need to be updated on the basis of the list to give them unique identifiers
    idrunnum = "nlrx-id"
  )
  
  
## Simulation design -------------------------------------------------------
  #Creating the simulation design - the nseeds is the number of replicates to do of each parameter set.
  collapse_gsa_nl@simdesign <- simdesign_lhs(nl = collapse_gsa_nl,
                                             samples = 4000,
                                             precision = 2,
                                             nseeds = 1)
  
  
  #gsa list - each variable +/- 10% which will be drawn from 1 at a time during the for loop
  int_param <- c("collapse-half-way", "emigration-timer", "max-tries")
  
  for(i in 1:ncol(collapse_gsa_nl@simdesign@siminput)){
    if(names(collapse_gsa_nl@simdesign@siminput[,i]) %in% int_param){
      collapse_gsa_nl@simdesign@siminput[,i] <- round(collapse_gsa_nl@simdesign@siminput[,i], digits = 0 )
    }
  }
  
  #View(collapse_gsa_nl@simdesign@siminput)
  

  
  
  #Pre-flight checks...
  print(collapse_gsa_nl)
  eval_variables_constants(collapse_gsa_nl)
  #Nothing should be defined for either output-file-name (this is a manual trigger when using the NetLogo Interface) or nlrx-id - this widget get's filled by nlrx while experimenting
  
## Simulation run -----------------------------------------------------------
  
  #Setting up parallelisation
  plan(multisession)
  
  #Setting up the progress bar to keep track of how far the run is
  progressr::handlers("progress")
  gsa_results_2 <- progressr::with_progress(
    run_nl_all(collapse_gsa_nl,
               split = 16)
  )

toc()

#(115 hrs)
saveRDS(gsa_results_2, "../output/global_sensitivity_analysis/gsa_results_2.rds")

names(gsa_results_2) <- names(gsa_ls)
saveRDS(gsa_results_2, "../output/global_sensitivity_analysis/gsa_results_2_named.rds")

#Checking the number of unique parameter settings
#length(unlist(gsa_ls))
#121 unique parameters - 100 seeds, so 10100 runs



# Analysis ----------------------------------------------------------------


## Analysis Libraries ------------------------------------------------------
#Data manipulations
library(readr) #reading in all files
library(vroom)
library(janitor) #Name amendments
library(dplyr) #Data manipulations
library(tidyr) #Pivoting
library(stringr) #Parsing data
library(zoo) #Calculating cumulative statistics

#Graphing
library(ggplot2) #Plots
library(viridis) #Colour scheme 
library(lemon)

#Analysis
library(caret)
library(pdp)
library(gbm)

library(tictoc)

#Custom functions
#https://stackoverflow.com/questions/52459711/how-to-find-cumulative-variance-or-standard-deviation-in-r
source("./scripts/functions/cumvar.r")

## Reading in data ---------------------------------------------------------

#Reading in files
# tic("Reading individual files")
# 
# gsa_res_vr <- list.files("../output/global_sensitivity_analysis/",
#                         pattern = ".csv",
#                         full.names = TRUE) %>%
#   vroom() %>%
# 
#   # #tidying initialisation names for ease of handling
#   # mutate(initialisation_data = str_remove(
#   #   str_remove(initialisation_data,
#   #              "./data/global_sensitivity_analysis/"),
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
# vroom_write(gsa_res_vr,
#           "./output/global_sensitivity_analysis/gsa_allrun_data_2.csv",
#           col_names = TRUE)


#reading in meta-data of the runs
gsa_meta_df <- readRDS("../output/global_sensitivity_analysis/gsa_results_2.rds")[,-1]


colnames(gsa_meta_df) <- c(names(gsa_ls), names(gsa_default_ls), "nlrx_id", "seed", "final_tick", "final_turtle_count", "run_number")

#Changing names to for ease of typing
names(gsa_meta_df) <- str_replace_all(names(gsa_meta_df), "[-]", "_")

#Reading in combined files
gsa_df <- vroom("./output/global_sensitivity_analysis/gsa_allrun_data_2.csv") %>% 
  #Joining on the meta data
  left_join(gsa_meta_df, 
            by = c("run_id" = "nlrx_id")) %>% 
  pivot_longer(cols = starts_with("settled_"),
               names_to = "island_id",
               values_to = "adult_count") 


#Setting a reproducible seed
set.seed(42)

## Time to extinction analysis-------------------------------------------------------------------------
#Subsetting dataframe to just the covariates (changing input params) and the response (time to extinction)
extinct_time_df <- gsa_df %>% 
  group_by(run_id) %>% 
  slice_tail() %>% 
  ungroup() %>% 
  filter(final_turtle_count == 0) %>% 
  #removing the first one as it does not change
  dplyr::select(c(ticks, str_replace_all(names(gsa_ls), "-", "_"))) %>% 
  mutate(ticks = ticks - 50) %>% 
  rename("time" = "ticks")


#Setting up the tuning parameter controls
gsa_et_tc <- trainControl(method = "cv",
                          number = 5,
                          search = "grid",
                          returnResamp = "all")


tic("extinction time gbm")
# #Fitting gradient boosting machine (linear)
# ext_time_gbml <- train(time ~ .,
#                        method = "xgbLinear",
#                        tuneLength = 12,
#                        trControl = gsa_et_tc,
#                        data = extinct_time_df)

ext_time_gbm_2 <- train(time ~ .,
                       method = "gbm",
                       tuneLength = 12,
                       trControl = gsa_et_tc,
                       data = extinct_time_df)


# ext_time_rf <- train(time ~ .,
#                       method = "rf",
#                       tuneLength = 12,
#                       trControl = gsa_et_tc,
#                       data = extinct_time_df)


toc()


#<4-5 hr cook time

#Saving model for later
saveRDS(ext_time_gbm_2, "./output/global_sensitivity_analysis/ext_time_gbm_2.rds")

#Chick predation most important!
#varImp(ext_time_rf)

varImp(ext_time_gbm_2)

# Creating partial dependence curves for the pop size classes
extTime_pdp_plots <- vector(mode = "list", length = length(ext_time_gbm_2$coefnames))

for (i in seq_along(ext_time_gbm_2$coefnames)) {
     #

  
  print(i)
  
  df_i <- ext_time_gbm_2 %>%
    pdp::partial(pred.var = ext_time_gbm_2$coefnames[i],
                 plot.engine = "ggplot2",
                 plot = FALSE,
                 type = "regression")
  
  # tidy
  df_i <- data.frame(var = ext_time_gbm_2$coefnames[i], df_i)
  colnames(df_i) <- c("var", "x", "y")
  extTime_pdp_plots[[i]] <- df_i
  
}

# clean up output
extTime_pdp_plots_clean <- bind_rows(extTime_pdp_plots)

extTime_pdp_var_imp <- varImp(ext_time_gbm_2)[[1]] %>% 
  arrange(desc(Overall))
extTime_pdp_var_imp$var <- row.names(extTime_pdp_var_imp)


extTime_pdp_plots_clean


#rename model params for plotting
extTime_pdp_plots_clean <- extTime_pdp_plots_clean %>%
  mutate(var = recode(var,
                      "chick_mortality" = "Chick Mortality (mean)",
                      "LNL_chick" = "La Nina Leaning Mortality (chick)",
                      "LN_chick" = "La Nina Mortality (chick)",
                      "ENL_chick" = "El Nino Leaning Mortality (chick)",
                      "EN_chick" = "El Nino Mortality (chick)",
                      "max_tries" = "Maximum Courtship Attempts",
                      "collapse_half_way" = "Collapse 50% Threshold",
                      "emig_out_prob" = "Emigrate out of System (%)",
                      "emigration_timer" =  "Failed Courtships before Emigration Attempt" ,
                      "female_philopatry" = "Female Philopatry",
                      
                      "prop_returning_breeders" = "Proportion of Breeders Returning"))


extTime_pdp_var_imp <- extTime_pdp_var_imp %>%
  mutate(var = recode(var,
                      "chick_mortality" = "Chick Mortality (mean)",
                      "LNL_chick" = "La Nina Leaning Mortality (chick)",
                      "LN_chick" = "La Nina Mortality (chick)",
                      "ENL_chick" = "El Nino Leaning Mortality (chick)",
                      "EN_chick" = "El Nino Mortality (chick)",
                      "max_tries" = "Maximum Courtship Attempts",
                      "collapse_half_way" = "Collapse 50% Threshold",
                      "emig_out_prob" = "Emigrate out of System (%)",
                      "emigration_timer" =  "Failed Courtships before Emigration Attempt" ,
                      "female_philopatry" = "Female Philopatry",
                      
                      "prop_returning_breeders" = "Proportion of Breeders Returning"))


# fix var order for plotting
extTime_pdp_plots_clean$var <-  factor(extTime_pdp_plots_clean$var,
                                       levels = extTime_pdp_var_imp$var)

extTime_pdp_var_imp$var <- as.factor(extTime_pdp_var_imp$var)

extTime_pdp_var_imp2 <- extTime_pdp_var_imp %>%
  filter(Overall > 20)

extTime_pdp_plots_clean2 <- extTime_pdp_plots_clean %>%
  filter(var %in% extTime_pdp_var_imp2$var)


# add vertical line for default model setting
df_line <- data.frame(var = extTime_pdp_var_imp2$var,
                      z = c(5, 0.3, 5, 0.9))
                        # c(5, 0.5, 0.9, 200, 0.9, 5, 0.3, 0.2, 0.1, 0.1, 0.2))


# plot
ggplot(extTime_pdp_plots_clean2, aes(x, y)) +
  geom_line(colour = "orange",
            linewidth = 1) +
  #geom_smooth() +
  geom_text(
    data = extTime_pdp_var_imp2,
    mapping = aes(x = -Inf, y = -Inf, label = round(Overall)),
    hjust = -0.5,
    vjust = -18) +
  facet_rep_wrap(~var, scales = "free_x", 
                 strip.position = "bottom", 
                 nrow = 1) +
  geom_vline(data = df_line, 
             aes(xintercept = z), 
             linetype = 2, 
             colour = "red") +
  labs(x = "",
       y = "Time to Extinction") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.placement = "outside")

#Saving
ggsave("./graphs/global_sensitivity_analysis/gsa_extinction_time_plot.png", width = 15.5, height = 6.5)

## Population Size (50 y Average) ---------------------------------------------------------

#calculating the number of birds for the run where there was no extinction for the last 50 years
gsa_50y_ps_df <- gsa_df %>% 
  filter(ticks > 501) %>% 
  group_by(run_id, island_id) %>% 
  summarise(adult_mean = mean(adult_count),
            .groups = "keep") %>% 
  ungroup() %>% 
  left_join(gsa_meta_df, by = c("run_id" = "nlrx_id")) %>% 
  select(adult_mean, island_id, str_replace_all(names(gsa_ls), "-", "_"))


#Setting up the tuning parameter controls
gsa_ps_tc <- trainControl(method = "cv",
                          number = 5,
                          search = "grid",
                          returnResamp = "all")

gsa_50y_ps_1_df <- gsa_50y_ps_df %>% 
  filter(island_id == "settled_ct_isl_1") %>% 
  select(-island_id)


tic("pop size gbm - 1")
pop_size_1_gbm <- train(adult_mean ~ .,
                        method = "gbm",
                        tuneLength = 12,
                        trControl = gsa_et_tc,
                        data = gsa_50y_ps_1_df)

toc()

varImp(pop_size_1_gbm)

# Creating partial dependence curves for the pop size classes
popsize_pdp_plots <- vector(mode = "list", length = length(pop_size_1_gbm$coefnames))

for (i in seq_along(pop_size_1_gbm$coefnames)) {
  
  print(i)
  
  df_i <- pop_size_1_gbm %>%
    pdp::partial(pred.var = pop_size_1_gbm$coefnames[i],
                 plot.engine = "ggplot2",
                 plot = FALSE,
                 type = "regression")
  
  # tidy
  df_i <- data.frame(var = pop_size_1_gbm$coefnames[i], df_i)
  colnames(df_i) <- c("var", "x", "y")
  popsize_pdp_plots[[i]] <- df_i
  
}

# clean up output
popsize_pdp_plots_clean <- bind_rows(popsize_pdp_plots)

popsize_pdp_var_imp <- varImp(pop_size_1_gbm)[[1]] %>% 
  arrange(desc(Overall))
popsize_pdp_var_imp$var <- row.names(popsize_pdp_var_imp)

#rename model params for plotting
popsize_pdp_plots_clean <- popsize_pdp_plots_clean %>%
  mutate(var = recode(var,
                      "chick_mortality" = "Chick Mortality (mean)",
                      "LNL_chick" = "La Nina Leaning Mortality (chick)",
                      "LN_chick" = "La Nina Mortality (chick)",
                      "ENL_chick" = "El Nino Leaning Mortality (chick)",
                      "EN_chick" = "El Nino Mortality (chick)",
                      "max_tries" = "Maximum Courtship Attempts",
                      "collapse_half_way" = "Collapse 50% Threshold",
                      "emig_out_prob" = "Emigrate out of System (%)",
                      "emigration_timer" =  "Failed Courtships before Emigration Attempt" ,
                      "female_philopatry" = "Female Philopatry",
                      
                      "prop_returning_breeders" = "Proportion of Breeders Returning"))


popsize_pdp_var_imp <- popsize_pdp_var_imp %>%
  mutate(var = recode(var,
                      "chick_mortality" = "Chick Mortality (mean)",
                      "LNL_chick" = "La Nina Leaning Mortality (chick)",
                      "LN_chick" = "La Nina Mortality (chick)",
                      "ENL_chick" = "El Nino Leaning Mortality (chick)",
                      "EN_chick" = "El Nino Mortality (chick)",
                      "max_tries" = "Maximum Courtship Attempts",
                      "collapse_half_way" = "Collapse 50% Threshold",
                      "emig_out_prob" = "Emigrate out of System (%)",
                      "emigration_timer" =  "Failed Courtships before Emigration Attempt" ,
                      "female_philopatry" = "Female Philopatry",
                      
                      "prop_returning_breeders" = "Proportion of Breeders Returning"))


# fix var order for plotting
popsize_pdp_plots_clean$var <-  factor(popsize_pdp_plots_clean$var,
                                        levels = popsize_pdp_var_imp$var)

popsize_pdp_var_imp$var <- as.factor(popsize_pdp_var_imp$var)

popsize_pdp_var_imp2 <- popsize_pdp_var_imp %>%
  filter(Overall >= 20)

popsize_pdp_plots_clean2 <- popsize_pdp_plots_clean %>%
  filter(var %in% popsize_pdp_var_imp2$var)


# add vertical line for default model setting
df_line <- data.frame(var = popsize_pdp_var_imp2$var,
                      z = c(5, 0.5, 0.3, 200))



#THIS PLOT IS BROKEN - RESULTS NEED TO BE BACK-TRANSFORMED?
# plot
ggplot(popsize_pdp_plots_clean2, aes(x, y)) +
  geom_line(colour = "orange",
            linewidth = 1) +
  #geom_smooth() +
  geom_text(
    data = popsize_pdp_var_imp2,
    mapping = aes(x = -Inf, y = -Inf, label = round(Overall)),
    hjust = -0.5,
    vjust = -18) +
  facet_rep_wrap(~var, scales = "free_x", strip.position = "bottom", nrow = 1) +
  geom_vline(data = df_line, aes(xintercept = z), linetype = 2, colour = "red") +
  labs(x = "",
       y = "Population size") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.placement = "outside")

ggsave("./graphs/global_sensitivity_analysis/pop_size_isl_1_gsa.png", width = 15.5, height = 6.5)

#Island 2 model
gsa_50y_ps_2_df <- gsa_50y_ps_df %>% 
  filter(island_id == "settled_ct_isl_2") %>% 
  dplyr::select(-island_id)

tic("pop size gbm- 2")
pop_size_2_gbm <- train(adult_mean ~ .,
                        method = "gbm",
                        tuneLength = 12,
                        trControl = gsa_et_tc,
                        data = gsa_50y_ps_2_df) 
toc()
varImp(pop_size_2_gbm)


# Creating partial dependence curves for the pop size classes
popsize_pdp_plots <- vector(mode = "list", length = length(pop_size_2_gbm$coefnames))

for (i in seq_along(pop_size_2_gbm$coefnames)) {
  
  print(i)
  
  df_i <- pop_size_2_gbm %>%
    pdp::partial(pred.var = pop_size_2_gbm$coefnames[i],
                 plot.engine = "ggplot2",
                 plot = FALSE,
                 type = "regression")
  
  # tidy
  df_i <- data.frame(var = pop_size_2_gbm$coefnames[i], df_i)
  colnames(df_i) <- c("var", "x", "y")
  popsize_pdp_plots[[i]] <- df_i
  
}

# clean up output
popsize_pdp_plots_clean <- bind_rows(popsize_pdp_plots)

popsize_pdp_var_imp <- varImp(pop_size_2_gbm)[[1]] %>% 
  arrange(desc(Overall))
popsize_pdp_var_imp$var <- row.names(popsize_pdp_var_imp)

#rename model params for plotting
popsize_pdp_plots_clean <- popsize_pdp_plots_clean %>%
  mutate(var = recode(var,
                      "chick_mortality" = "Chick Mortality (mean)",
                      "LNL_chick" = "La Nina Leaning Mortality (chick)",
                      "LN_chick" = "La Nina Mortality (chick)",
                      "ENL_chick" = "El Nino Leaning Mortality (chick)",
                      "EN_chick" = "El Nino Mortality (chick)",
                      "max_tries" = "Maximum Courtship Attempts",
                      "collapse_half_way" = "Collapse 50% Threshold",
                      "emig_out_prob" = "Emigrate out of System (%)",
                      "emigration_timer" =  "Failed Courtships before Emigration Attempt" ,
                      "female_philopatry" = "Female Philopatry",
                      "prop_returning_breeders" = "Proportion of Breeders Returning"))


popsize_pdp_var_imp <- popsize_pdp_var_imp %>%
  mutate(var = recode(var,
                      "chick_mortality" = "Chick Mortality (mean)",
                      "LNL_chick" = "La Nina Leaning Mortality (chick)",
                      "LN_chick" = "La Nina Mortality (chick)",
                      "ENL_chick" = "El Nino Leaning Mortality (chick)",
                      "EN_chick" = "El Nino Mortality (chick)",
                      "max_tries" = "Maximum Courtship Attempts",
                      "collapse_half_way" = "Collapse 50% Threshold",
                      "emig_out_prob" = "Emigrate out of System (%)",
                      "emigration_timer" =  "Failed Courtships before Emigration Attempt" ,
                      "female_philopatry" = "Female Philopatry",
                      
                      "prop_returning_breeders" = "Proportion of Breeders Returning"))


# fix var order for plotting
popsize_pdp_plots_clean$var <-  factor(popsize_pdp_plots_clean$var,
                                       levels = popsize_pdp_var_imp$var)

popsize_pdp_var_imp$var <- as.factor(popsize_pdp_var_imp$var)

popsize_pdp_var_imp2 <- popsize_pdp_var_imp %>%
  filter(Overall >= 20)

popsize_pdp_plots_clean2 <- popsize_pdp_plots_clean %>%
  filter(var %in% popsize_pdp_var_imp2$var)


# add vertical line for default model setting
df_line <- data.frame(var = popsize_pdp_var_imp2$var,
                      z = c(0.3, 5, 200, 5, 0.9))


#THIS PLOT IS BROKEN - RESULTS NEED TO BE BACK-TRANSFORMED?
# plot
ggplot(popsize_pdp_plots_clean2, aes(x, y)) +
  geom_line(colour = "orange",
            linewidth = 1) +
  #geom_smooth() +
  geom_text(
    data = popsize_pdp_var_imp2,
    mapping = aes(x = -Inf, y = -Inf, label = round(Overall)),
    hjust = -0.5,
    vjust = -18) +
  facet_rep_wrap(~var, scales = "free_x", strip.position = "bottom", nrow = 1) +
  geom_vline(data = df_line, aes(xintercept = z), linetype = 2, colour = "red") +
  labs(x = "",
       y = "Population size") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.placement = "outside")

ggsave("./graphs/global_sensitivity_analysis/pop_size_isl_2_gsa.png", width = 15.5, height = 6.5)


## Model interactions -----------------------------------------------

#Extinction time interaction graph
ext_time_gbm_2[["bestTune"]]

ext_time_gbm_int <- gbm.step(data= as.data.frame(extinct_time_df),
                             gbm.x = 2:12, gbm.y = 1,family = "gaussian",
                             n.trees = 100, tree.complexity = 5,
                             shrinkage = 0.1, n.minobsinnode = 10)

gbm.interactions(ext_time_gbm_int)

ext_int_pdp <- ext_time_gbm_2 %>%
  pdp::partial(pred.var = c("chick_mortality", "max_tries"),
               plot.engine = "ggplot2",
               plot = TRUE,
               type = "regression")

ext_pdp_gg <- ext_int_pdp + 
  labs(y = "Maximum Courtship Attempts", 
       x = "Chick Mortality (mean)") +
  guides(fill = guide_legend("Time to Extinction")) +
  theme_minimal() + 
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))


ext_pdp_gg

ggsave("./graphs/global_sensitivity_analysis/ext_time_interaction.png", width = 10, height = 7)


#Population size (isl 1) interaction graph
pop_size_1_gbm[["bestTune"]]

pop_size_1_gbm_int <- gbm.step(data= as.data.frame(gsa_50y_ps_1_df),
                             gbm.x = 2:12, gbm.y = 1, 
                             family = "gaussian",
                             n.trees = 450, 
                             tree.complexity = 5,
                             shrinkage = 0.1, 
                             n.minobsinnode = 10)

gbm.interactions(pop_size_1_gbm_int)

pop_1_int_pdp <- pop_size_1_gbm %>%
  pdp::partial(pred.var = c("chick_mortality",
                            "emig_out_prob"),
               plot.engine = "ggplot2",
               plot = TRUE,
               type = "regression")

pop_1_pdp_gg <- pop_1_int_pdp + 
  labs(y = "Chick Mortality (mean)", 
       x = "Emigrate out of System (%)") +
  guides(fill = guide_legend("Population Size")) +
  theme_minimal() + 
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))


pop_1_pdp_gg

ggsave("./graphs/global_sensitivity_analysis/pop_size_1_interaction.png", width = 10, height = 7)



#Population size (isl 1) interaction graph
pop_size_2_gbm[["bestTune"]]

pop_size_2_gbm_int <- gbm.step(data= as.data.frame(gsa_50y_ps_1_df),
                               gbm.x = 2:12, gbm.y = 1, 
                               family = "gaussian",
                               n.trees = 250, 
                               tree.complexity = 12,
                               shrinkage = 0.1, 
                               n.minobsinnode = 10)

gbm.interactions(pop_size_2_gbm_int)

pop_2_int_pdp <- pop_size_2_gbm %>%
  pdp::partial(pred.var = c("chick_mortality",
                            "emig_out_prob"),
               plot.engine = "ggplot2",
               plot = TRUE,
               type = "regression")

pop_2_pdp_gg <- pop_2_int_pdp + 
  labs(y = "Chick Mortality (mean)", 
       x = "Emigrate out of System (%)") +
  guides(fill = guide_legend("Population Size")) +
  theme_minimal() + 
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
pop_2_pdp_gg
ggsave("./graphs/global_sensitivity_analysis/pop_size_2_interaction.png", width = 10, height = 7)







