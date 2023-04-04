#Extirpation Analysis

#This script runs a simulation experiment through nlrx for the collapse model (base), with the key parameter varying being the amount of adult and chick predation 

# NL setup ----------------------------------------------------------------
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
outpath <- file.path("D:/colony_dynamics/output/extirap")

# Setup nl object - this initialises an instance of NetLogo
collapse_extir_nl <- nl(nlversion = "6.2.2",
                  nlpath = netlogopath,
                  modelpath = modelpath,
                  jvmmem = 4098)

## Variable creation ----------------------------------------------------
# Creating a list of variables defaults to feed into the experiment one line at a time.
extir_default_ls <- list(
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
  
  "chick-mortality" = 0.3,
  "chick-mortality-sd" = 0.05,
  #"chick-predation" = 0.5,
  
  "juvenile-mortality" = 0.55,
  "juvenile-mortality-sd" = 0.1,
  
  "adult-mortality" = 0.05,
  "adult-mortality-sd" = 0.01,
  #"adult-predation" = 0.05,
  
  "enso-breed-impact" = "\"[0.2 0.1 0 0.1 0.2]\"",
  "enso-adult-mort" = "\"[0.02 0.01 0 0.01 0.02]\"",
  
  "max-age" = 20,
  "old-mortality" = 0.5,
  
  #Breeding
  "starting-juvenile-population" = 10000,
  "starting-adult-population" = 20000,
  "age-at-first-breeding" = 4,
  "female-philopatry" = 0.95,
  "prop-returning-breeders" = 0.9,
  "max-tries" = 5,
  
  #Emigration controls
  "emigration-timer" = 5,
  "emigration-max-attempts" = 3,
  "emig-out-prob" = 0.5,
  "emigration-curve" = 0.5,
  "raft-half-way" = 250,
  
  #These are never changed....
  #Data export controls
  "behav-output-path" = "\"./output/predation_analysis/\""
)



extir_ls <- list(
  "chick-predation" = list(values = c(0, 0.1, 0.2, 0.3, 0.4, 0.5)),
  "adult-predation" = list(values = c(0, 0.01, 0.02, 0.03, 0.04, 0.05))
  )

# Extirpation experiment setup ----------------------------------------------------

#Bench marking
tic("Extirpation experiment")

#Setting up the nlrx experiment
collapse_extir_nl@experiment <- experiment(
  expname = "extir",
  outpath = outpath,
  repetition = 1,
  tickmetrics = "false",
  idsetup = "setup",
  idgo = "step",
  runtime = 600,
  evalticks = (600),
  stopcond = "not any? turtles",
  variables =  extir_ls,
  constants = extir_default_ls,
  idfinal = "behav-csv",
  #The nlrx_id will need to be updated on the basis of the list to give them unique identifiers
  idrunnum = "nlrx-id"
)

# Simulation design -------------------------------------------------------
#Creating the simulation design - the nseeds is the number of replicates to do of each parameter set.
collapse_extir_nl@simdesign <- simdesign_ff(nl = collapse_extir_nl,
                                                nseeds = 100)

#Pre-flight checks...
print(collapse_extir_nl)
eval_variables_constants(collapse_extir_nl)
#Nothing should be defined for either output-file-name (this is a manual trigger when using the NetLogo Interface) or nlrx-id - this widget get's filled by nlrx while experimenting

# Simulation run -----------------------------------------------------------

#Setting up parallelisation
plan(multisession)

#Setting up the progress bar to keep track of how far the run is
progressr::handlers("progress")
extir_results_2 <- progressr::with_progress(
  run_nl_all(collapse_extir_nl,
             split = 18)
)

toc()

saveRDS(extir_results_2, "../output/extirpation_simulation/extir_results.rds")
#(110 hrs)
names(extir_results_2) <- str_replace_all(c(names(extir_ls), names(extir_default_ls)), "-", "_")


saveRDS(extir_results_2, "../output/extirpation_simulation/extir_results_named.rds")




#Checking the number of unique parameter settings
#length(unlist(gsa_ls))
#121 unique parameters - 100 seeds, so 10100 runs


# Extirpation analysis ----------------------------------------------------

#Reading in large data
library(vroom)

#Data manipulations
library(janitor) #Name amendments
library(dplyr) #Data manipulations
library(tidyr) #Pivoting
library(stringr) #Parsing data

#Graphing
library(ggplot2) #Plots
library(viridis) #Colour scheme 


# #Reading in files
# extirp_res_vr <- list.files("../output/predation_analysis/",
#                         pattern = ".csv",
#                         full.names = TRUE) %>%
#   vroom() %>%
# 
# 
#   #Creating a new column to keep track of the tick the run was up to
#   group_by(run_id) %>%
# 
#   #Adding in ticks to sheet
#   mutate(ticks = 1:n()) %>%
# 
#   ungroup()
# # 
# #  #Writing a single large file with some minor manipulations to make it quicker to read in and access
# vroom_write(extirp_res_vr,
#           "../output/predation_analysis/extirp_allrun_data.csv",
#           col_names = TRUE)

#Meta information for each run 
extirp_meta_df <- readRDS("../output/extirpation_simulation/extir_results.rds")[,1:42]

names(extirp_meta_df) <- str_replace_all(c("tbd", 
                                           names(extir_ls), 
                                           names(extir_default_ls),
                                           "nlrx_id"), "-", "_")

#Reading in the run data
extirp_df <- vroom("../output/predation_analysis/extirp_allrun_data.csv") %>% 
  #Reducing this to just the last 50 years of data to summarise over.
  #filter(ticks >= 450) %>% 
  #Joining on the meta data
  left_join(extirp_meta_df, 
            by = c("run_id" = "nlrx_id")) %>% 
  pivot_longer(cols = starts_with("settled_"),
               names_to = "island_id",
               values_to = "adult_count") %>%  
  #Creating some convience columns 
  mutate(island_id = str_sub(island_id, 
                             start = 16L, end = 16L),
         predators = ifelse(island_id == "1", "Present", "Absent"),
         run_number = as.factor(run_id))



# Graphing ----------------------------------------------------------------

# seaPal <- wesanderson::wes_palette("FantasticFox1")[c(4,3, 1)] 
# #c("#440154FF",  "#FDE725FF")
# #viridis(n = 2, begin = 0.2)
# 
# ggplot(extirp_df,
#   aes(x = ticks, y = adult_count,
#     colour = predators,
#     group = interaction(run_number,  island_id))) +
#   
#   geom_line(linewidth = 0.8) +
#   scale_colour_manual(values = seaPal, 
#                       name = "Predation") +
#   
#   labs(y = "Adults (Count)", x = "Years") +
#   #xlim(c(5, 100)) +
#   
#  
#   
#  facet_grid(chick_pred_isl_1 ~ adult_pred_isl_1) +
# theme_minimal() +
# theme(axis.text = element_text(size = 12, colour = "white"),
#       axis.title = element_text(size = 14, colour = "white"),
#       strip.text = element_text(size = 12, colour = "white"),
#       legend.text = element_text(size = 12, colour = "white"),
#       legend.title = element_text(size = 14, colour = "white"),
#       panel.grid = element_blank())
# 

#Saving
# ggsave("./graphs/persistance.png",
#        width = 9.9, height = 5.5)


#Calculate the time to the predator invaded island reaching (min time, max time, mean time for each scenario)

#End of run mean meta-population sums
end_counts <- extirp_df %>% 
  filter(ticks == 600) %>% 
  group_by(chick_predation, adult_predation, predators) %>% 
  summarise(end_mean = round(mean(adult_count),
                             digits = 0),
            end_sd = round(sd(adult_count), 
                           digits = 0)) %>% 
  ungroup() %>% 
  group_by(chick_predation, adult_predation) %>% 
  summarise(end_mean_sum = sum(end_mean),
            end_mean_sd = round(mean(end_sd), 0)) %>% 
  mutate(end_est = paste(end_mean_sum, 
                         end_mean_sd, 
                         sep = " ± "))
end_counts

end_counts %>% 
  dplyr::select(chick_predation, adult_predation, end_est) %>% 
  rename("Chick Predation" = "chick_predation",
         "Adult_Predation" = "adult_predation",
         "Population (Mean + S.D)" = "end_est") %>% 
  write.csv("./output/extirpation_simulation/end_pop_summary.csv")

extir_point <- extirp_df %>% 
  filter(predators == "Present") %>% 
  mutate(extirp = ifelse(adult_count < 50, 
                          TRUE, 
                          FALSE),
         pseudo_ext = ifelse(adult_count < 500 & adult_count > 50, 
                             TRUE, 
                             FALSE)) %>%
  filter(extirp) %>% 
  group_by(chick_predation, adult_predation, run_id) %>% 
  arrange(ticks, .by_group = TRUE) %>% 
  slice_head() %>% 
  ungroup() %>% 
  group_by(chick_predation, adult_predation) %>% 
  summarise(time_to_ext_5 = round(quantile(ticks, 0.05), 
                                  digits = 1),
            time_to_ext_95 = round(quantile(ticks, 0.95), 
                                   digits = 1),
            median_time_to_ext = median(ticks), 
            prop_ext = n()) 
extir_point


psuedo_ext_point <- extirp_df %>% 
  filter(predators == "Present") %>% 
  mutate(extirp = ifelse(adult_count < 50, 
                         TRUE, 
                         FALSE),
         pseudo_ext = ifelse(adult_count < 500 & adult_count > 50, 
                             TRUE, 
                             FALSE)) %>%
  filter(pseudo_ext) %>% 
  group_by(chick_predation, adult_predation, run_id) %>% 
  arrange(ticks, .by_group = TRUE) %>% 
  slice_head() %>% 
  ungroup() %>% 
  group_by(chick_predation, adult_predation) %>% 
  summarise(time_to_psuedo_5 = round(quantile(ticks, 0.05), 
                                  digits = 1),
            time_to_psuedo_95 = round(quantile(ticks, 0.95), 
                                   digits = 1),
            median_time_to_psuedo = median(ticks),
            prop_psuedo_ext = n()) 

psuedo_ext_point


extirpation_summary <- end_counts %>% 
  left_join(extir_point) %>% 
  #left_join(psuedo_ext_point) %>% 
  mutate(time_prop = paste0(median_time_to_ext - 50,"y", " | ", prop_ext,"%"),
         time_prop = replace(time_prop, is.na(prop_ext), ""))

write.csv(extirpation_summary, "./output/extirpation_simulation/extirpation_summary.csv")
# %>% 
#   mutate(end_est = paste(end_mean_sum, 
#                          end_mean_sd, 
#                          sep = " ± "))

#
extirp_summ <- extirp_df %>% 
  group_by(chick_predation, adult_predation, island_id, ticks, predators) %>% 
  summarise(adult_mean = mean(adult_count),
            adult_sd = sd(adult_count),
            adult_lwr = adult_mean - adult_sd,
            adult_lwr = replace(adult_lwr, adult_lwr < 0, 0),
            adult_upr = adult_mean + adult_sd)


#Calculating an average for the null no predation scenario to compare other scenarios too

null_scenario <- extirp_summ %>% 
  filter(chick_predation == 0 & adult_predation == 0) %>% 
  filter(ticks > 400) %>% 
  ungroup() %>% 
  #group_by(island_id) %>% 
  summarise(mean_count = mean(adult_mean),
            sd_count = sd(adult_mean))


#Adult pred only effect

extirp_summ %>% 
  filter(chick_predation == 0) %>% 
  filter(ticks > 400) %>% 
  ungroup() %>% 
  group_by(adult_predation) %>% 
  summarise(mean_count = mean(adult_mean),
            sd_count = sd(adult_mean)) %>% 
  mutate(pop_change = 1 - (mean_count / 74230))

extirp_summ %>% 
  filter(adult_predation == 0) %>% 
  filter(ticks > 400) %>% 
  ungroup() %>% 
  group_by(chick_predation) %>% 
  summarise(mean_count = mean(adult_mean),
            sd_count = sd(adult_mean)) %>% 
  mutate(pop_change = 1 - (mean_count / 74230))

baseline_conditions <- extirp_summ %>% 
  filter(chick_predation == 0.5 & adult_predation == 0.05) %>%
  filter(ticks > 400) %>% 
  group_by(island_id) %>% 
  summarise(mean_count = mean(adult_mean),
            sd_count = sd(adult_mean))

ggplot(extirp_summ,
       aes(x = ticks, y = adult_mean,
           colour = predators,
           group = interaction(chick_predation,
                               adult_predation,
                               island_id))) +

  
  geom_ribbon(aes(ymin = adult_lwr, 
                  ymax = adult_upr,
                  fill = predators,
                  colour = predators),
              alpha = 0.7) + 

  
  geom_line(aes(linetype = predators),
            colour = "black") +
  
  geom_hline(aes(yintercept = 74230),
             colour = "#ff960f",
             linetype = "dotdash",
             linewidth = 0.5) +
  
  geom_vline(aes(xintercept = 50),
             colour = "#850000",
             linetype = "dotted",
             linewidth = 0.5) +
  
  geom_text(data = extirpation_summary,
            mapping = aes(x = Inf, y = Inf, label = time_prop,
                          group = NULL, colour = NULL),
            hjust = 1.05,
            vjust = 1.5) +
  
  scale_colour_manual(values = c("#3B9AB2","#F21A00"), 
                      name = "Predators") +
  scale_fill_manual(values = c("#3B9AB2","#F21A00"), 
                      name = "Predators") +
  
  scale_linetype_manual(values = c("solid", "dashed"), 
                    name = "Predators") +
  
  labs(y = "Breeding Pairs", x = "Years") +
  
  scale_x_continuous(breaks = seq(0, 500, by = 100), guide = guide_axis(angle = 90)) +
  
  scale_y_continuous(breaks = seq(0, 100000, by = 25000), limits = c(0, 100000)) +
  
  
  facet_grid(chick_predation ~ adult_predation) + 
  
  theme_bw() +
  
  theme(axis.text = element_text(size = 12),
        #axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        panel.grid = element_blank())

ggsave("./graphs/extirpation_simulations/predation_effects.png",
       width = 16, height = 9)


no_pred_gg <- ggplot(filter(extirp_summ, 
                      adult_predation == 0 & chick_predation == 0),
       aes(x = ticks, y = adult_mean,
           colour = predators,
           group = interaction(chick_predation,
                               adult_predation,
                               island_id))) +
  
  
  geom_ribbon(aes(ymin = adult_lwr, 
                  ymax = adult_upr,
                  fill = predators,
                  colour = predators),
              alpha = 0.7) + 
  
  geom_text(data = filter(extirpation_summary, 
                          adult_predation == 0 & chick_predation == 0),
            mapping = aes(x = Inf, y = Inf, label = time_prop,
                          group = NULL, colour = NULL),
            hjust = 1.05,
            vjust = 1.5) +

  geom_line(aes(linetype = predators),
            colour = "black") +
  
  scale_colour_manual(values = c("#3B9AB2","#F21A00"), 
                      name = "Predators", guide = "none") +
  scale_fill_manual(values = c("#3B9AB2","#F21A00"), 
                    name = "Predators", guide = "none") +
  
  scale_linetype_manual(values = c("solid", "dashed"), 
                        name = "Predators", guide = "none") +
  
  labs(y = "Breeding Pairs", x = "") +
  
  ggtitle("No Predation") + 
  
  scale_x_continuous(breaks = NULL) +
  
  scale_y_continuous(breaks = NULL) +
  
  theme_classic() +
  
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        panel.grid = element_blank())

pred_gg <- ggplot(filter(extirp_summ, 
                         adult_predation == 0.05 & chick_predation == 0.5),
                  aes(x = ticks, y = adult_mean,
                      colour = predators)) +
  
  
  geom_ribbon(aes(ymin = adult_lwr, 
                  ymax = adult_upr,
                  fill = predators,
                  colour = predators),
              alpha = 0.7) + 
  
  
  geom_vline(aes(xintercept = 50),
             colour = "#850000",
             linetype = "dotted",
             linewidth = 1) +
  
  geom_line(aes(linetype = predators),
            colour = "black") +
  
  scale_colour_manual(values = c("#3B9AB2","#F21A00"), 
                      name = "Predators") +
  scale_fill_manual(values = c("#3B9AB2","#F21A00"), 
                    name = "Predators") +
  
  scale_linetype_manual(values = c("solid", "dashed"), 
                        name = "Predators") +
  
  labs(y = "", x = "") +
  
  ggtitle("Predation") + 
  
  scale_x_continuous(breaks = NULL) +
  
  scale_y_continuous(breaks = NULL) +
  
  theme_classic() +
  
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        panel.grid = element_blank())

library(patchwork)

no_pred_gg + pred_gg 


ggsave("./graphs/extirpation_simulations/predation_effects_intro_example.png", width = 7, height = 3.5)



# Tile plots --------------------------------------------------------------


tile_graph_df <- extirp_df %>% 
  filter(ticks == 600) %>% 
  group_by(chick_predation, adult_predation, predators) %>% 
  summarise(end_mean = round(mean(adult_count),
                             digits = 0)) %>% 
  ungroup() %>% 
  mutate(pop_reduction = 100 *(1 - round((end_mean / 74792), digits = 2)))


ggplot(tile_graph_df, 
      aes( y = as.character(chick_predation),
           x = as.character(adult_predation),
           fill = pop_reduction)) + 
  
  geom_tile() +
  
  facet_grid(~predators) +
  
  scale_fill_viridis_c(option = "A",
                       name = "Population Reduction",
                       direction = -1,
                       guide = guide_colourbar(direction = "horizontal", title.position = "top")) + 
  
  labs(y = "Chick Predation Rate",
       x = "Adult Predation Rate") +
  
  theme_minimal() + 
  
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 14))


ggsave("./graphs/extirpation_simulations/population_reduction.png",
       width = 12.5, height = 6)

