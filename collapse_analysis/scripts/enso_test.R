#Checking my ENSO cycle looks correct

# Reading in data ---------------------------------------------------------

#Data manipulations
library(readr) #reading in all files
library(janitor) #Name amendments
library(dplyr) #Data manipulations
library(tidyr) #Pivoting
library(stringr) #Parsing data
library(zoo) #Calculating cumulative statistics

#Graphing
library(ggplot2) #Plots
library(viridis) #Colour scheme 
library(wesanderson)
library(patchwork)


# Two Island --------------------------------------------------------------

#Creating a list of runs

#Reading in files
consistency_df <- list.files("../output/consistency_analysis/", 
                             pattern = ".csv",
                             full.names = TRUE) %>%  
  read_csv() %>% 
  
  #Creating a new column to keep track of the tick the run was up to
  group_by(run_id) %>% 
  mutate(ticks = 1:n())



enso_check <- consistency_df %>%
  ungroup() %>% 
  filter(run_id == "consistency_1993961617_1") %>% 
  select(c(enso_state, ticks)) %>% 
  mutate(enso_class = recode(enso_state,
                             `0` = "La Niña",           
                             `1` = "La Niña Leaning",
                             `2` = "Neutral",
                             `3` = "El Niño Leaning",
                             `4` = "El Niño"),
         enso_class = factor(enso_class, levels = c( "La Niña",           
                                                     "La Niña Leaning",
                                                     "Neutral",
                                                     "El Niño Leaning",
                                                     "El Niño")))

enso_model <- ggplot(enso_check, aes(y = enso_state, x = ticks)) + 
  geom_line() + 
  xlim(0, 100) +
  geom_point(aes(colour = enso_class)) +
  scale_colour_viridis_d(name = "ENSO Class") +
  labs(y = "ENSO State",
       x = "Tick") +
  ggtitle("Simulated World") + 
  #guides(colour = guide_legend("ENSO State")) +
  theme_bw() +
  theme(axis.text.y = element_blank())

##Enso by state rather than SOI
enso_true <- read.csv("./data/state_SOI_detrended.csv") %>% 
  mutate(enso_state = ifelse(soi_index >= 1, "La Niña",
                             ifelse(soi_index < 1 & soi_index > 0.5, "La Niña Leaning",
                                    ifelse(soi_index < -0.5 & soi_index > -1, "El Niño Leaning",
                                           ifelse(soi_index < -1, "El Niño", "Neutral"))))) %>% 
  mutate(enso_class = recode(enso_state,
                            "La Niña" =  0 ,           
                            "La Niña Leaning" = 1,
                            "Neutral" = 2,
                            "El Niño Leaning" = 3,
                            "El Niño" = 4),
         enso_state = factor(enso_state, levels = c( "La Niña",           
                                                     "La Niña Leaning",
                                                     "Neutral",
                                                     "El Niño Leaning",
                                                     "El Niño")))

enso_real <- ggplot(enso_true, aes(x = soi_year, y = enso_class)) +
  geom_line() +
  geom_point(aes(colour = enso_state)) + 
  scale_colour_viridis_d(name = "ENSO Class") +
  labs(y = "ENSO State",
       x = "Year") +
  ggtitle("Real World") +
  #guides(colour = guide_legend("ENSO State")) +
  theme_bw() +
  theme(axis.text.y = element_blank())

#Patchwork
enso_real + enso_model + plot_layout(nrow = 2, guides = 'collect')


ggsave("./graphs/supporting_material/enso_states.png",
       width = 11, height = 7)
