# Statistical models
library(tidyverse)
library(readxl)
library(viridis)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
library(brms)
library(ggeffects)
library(tidybayes)
library(bayesplot)
library(modelr)
library(gridExtra)

load("data/richness_demo_land.RData")
load("data/abundance_demo_land.RData")

# functions to make figures look cohesive
change_theme <- function(){
  theme_bw() +
    ggplot2::theme(#text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 13), 
      axis.title = element_text(size = 15),
      axis.line.x = element_line(color="black"), 
      axis.line.y = element_line(color="black"),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),                                          
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),  
      plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
      plot.title = element_text(size = 15, vjust = 1, hjust = 0),
      legend.text = element_text(size = 12),          
      legend.title = element_blank(),                              
      legend.position = c(0.9, 0.9), 
      legend.key = element_blank(),
      legend.background = element_rect(size = 2,
                                       linetype = "blank"))
}

# Species richness ----

# Lowlands
prior1 <- c(set_prior(prior = 'normal(0,6)', class='b', coef="human_pop_trend_2022"), 	# global slope,
            set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 	# global intercept
            set_prior(prior = 'cauchy(0,2)', class='sd'))	# group-level intercepts

prior2 <- c(set_prior(prior = 'normal(0,6)', class='b', coef="human_pop_trend_2023"), 	# global slope,
            set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 	# global intercept
            set_prior(prior = 'cauchy(0,2)', class='sd'))	# group-level intercepts

prior3 <- c(set_prior(prior = 'normal(0,6)', class='b', coef="human_pop_trend_2024"), 	# global slope,
            set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 	# global intercept
            set_prior(prior = 'cauchy(0,2)', class='sd'))	# group-level intercepts

#### Birds -------------------------------------------------------------------

## Bird species richness ~ human_pop_trend
bird_species_richness <- richness_demo_land %>% filter(taxa == "Birds")

# running the model
bird_richness_model <- brm(bf(sp_richness ~ human_pop_trend_2023*region +
                                 (1|village_en)),                                      # village as random (grouping factor)
                            data = bird_species_richness, family = poisson(),
                            prior = prior2, iter = 10000,
                            warmup = 2000,
                            init = '0',
                            cores = 4, chains = 4)

summary(bird_richness_model)

# saving the model
save(bird_richness_model, file="data/models/bird_richness_model.RData")

#plotting predictions
(birds_richness_human_plot <- bird_species_richness %>%
    group_by(region) %>%                                                        # as region is a predictor in the model, we group by it here
    modelr::data_grid(human_pop_trend_2023 = seq_range(human_pop_trend_2023, n = 101)) %>%
    add_epred_draws(bird_richness_model, re_formula = NULL, allow_new_levels = TRUE) %>% #adding predicted draws from the model
    ggplot(aes(x = human_pop_trend_2023, y = sp_richness, color = region,            # plotting richness~human_pop by region
               fill = region)) +
    tidybayes::stat_lineribbon(aes(y = .epred, fill = region, colour = region),           # adding confidence intervals
                    .width = c(0.95), alpha = 0.3, linewidth = 1.5) +
    tidybayes::stat_lineribbon(aes(y = .epred, fill = region, colour = region) ,
                    .width = c(0), linewidth = 1.5) +
    geom_point(data = bird_species_richness, alpha = 0.8) +                                     # adding raw data
    scale_color_manual(values = c("#df7a5f", "#8cbca4")) +
    scale_fill_manual(values = c("#df7a5f", "#8cbca4")) +
    change_theme() +
    theme(legend.position = c(0.2, 0.87),
          plot.title = element_text(face = "bold",                              # adding legend 
                                    hjust = -0.17)) +
    labs(x = "\nHuman population trend\n(slope of change per year)",            # labels
         y = "Bird species richness\n",
         title = "A\n") +
    geom_vline(xintercept = 0, linetype = "dotted") +                           # adding line to show zero-net change of population
    scale_y_continuous(limits = c(0, 24),
                       breaks = c(0, 8, 16, 24),                                # scale limits
                       labels = c("0", "8", "16", "24")) +
    scale_x_continuous(limits = c(-1.3, 0.3),
                       breaks = c(-1.2, -0.9, -0.6, -0.3, 0, 0.3),
                       labels = c("-1.2", "-0.9", "-0.6", "-0.3", "0", "0.3")))

#### Amphibia ----------------------------------------------------------------

amphibia_species_richness <- richness_demo_land %>% filter(taxa == "Amphibia" &
                                                             sp_richness > 0)

## Amphibia richness ~ human_pop_trend
amphibia_richness_model1 <- brm(bf(sp_richness ~ human_pop_trend_2023*region +       # human_pop as fixed effect, interacting with region
                                     (1|village_en)),                           # village as random (grouping factor)
                                data = amphibia_species_richness, 
                                family = poisson(),
                                prior = prior2, 
                                iter = 10000,
                                warmup = 2000,
                                init = '0',
                                cores = 4, chains = 4)

summary(amphibia_richness_model1)
save(amphibia_richness_model1, file="data/models/amphibia_richness_model1.RData" )


# plot
(amphibian_richness_human_plot <- amphibia_species_richness %>%
    group_by(region) %>%
    modelr::data_grid(human_pop_trend_2023 = seq_range(human_pop_trend_2023, n = 101)) %>%
    add_epred_draws(amphibia_richness_model1, re_formula = NULL,                # adding predicted draws from model
                    allow_new_levels = TRUE) %>%
    ggplot(aes(x = human_pop_trend_2023, y = sp_richness, color = region,            # plotting richness~human_pop by region
               fill = region)) +
    tidybayes::stat_lineribbon(aes(y = .epred, fill = region, colour = region),           # adding confidence intervals
                               .width = c(0.95), alpha = 0.3, linewidth = 1.5) +
    tidybayes::stat_lineribbon(aes(y = .epred, fill = region, colour = region) ,
                               .width = c(0), linewidth = 1.5) +
    geom_point(data = amphibia_species_richness, alpha = 0.8) +                                     # adding raw data
    scale_color_manual(values = c("#df7a5f", "#8cbca4")) +
    scale_fill_manual(values = c("#df7a5f", "#8cbca4")) +
    change_theme() +
    theme(legend.position = c(0.2, 0.87),
          plot.title = element_text(face = "bold",                              # adding legend 
                                    hjust = -0.17)) +
    labs(x = "\nHuman population trend\n(slope of change per year)",            # labels
         y = "Amphibian species richness\n",
         title = "D\n") +
    geom_vline(xintercept = 0, linetype = "dotted") +                           # adding line to show zero-net change of population
    scale_y_continuous(limits = c(0, 24),
                       breaks = c(0, 8, 16, 24),                                # scale limits
                       labels = c("0", "8", "16", "24")) +
    scale_x_continuous(limits = c(-1.3, 0.3),
                       breaks = c(-1.2, -0.9, -0.6, -0.3, 0, 0.3),
                       labels = c("-1.2", "-0.9", "-0.6", "-0.3", "0", "0.3")))

#### Odonata ----------------------------------------------------------------

# filter data frame for only dragonflies
odonata_species_richness <- richness_demo_land %>% filter(taxa == "Odonata" & sp_richness > 0)

## Odonata richness ~ human_pop_trend

# Model fit
odonata_richness_model1 <- brm(bf(sp_richness ~ human_pop_trend_2023*region +         # human_pop as fixed effect, interacting with region
                                   (1|village_en)),                             # village as random (grouping factor)
                              data = odonata_species_richness, family = poisson(),
                              prior = prior2, iter = 10000,
                              warmup = 2000,
                              init = '0',
                              cores = 4, chains = 4)

summary(odonata_richness_model1)
# save model as RData
save(odonata_richness_model1, file="data/models/odonata_richness_model1.RData" )

#plot
(odonata_richness_human_plot <- odonata_species_richness %>%
    group_by(region) %>%
    modelr::data_grid(human_pop_trend_2023 = seq_range(human_pop_trend_2023, n = 101)) %>%
    add_epred_draws(odonata_richness_model1, re_formula = NULL, allow_new_levels = TRUE) %>%
    ggplot(aes(x = human_pop_trend_2023, y = sp_richness, color = region,            # plotting richness~human_pop by region
               fill = region)) +
    tidybayes::stat_lineribbon(aes(y = .epred, fill = region, colour = region),           # adding confidence intervals
                               .width = c(0.95), alpha = 0.3, linewidth = 1.5) +
    tidybayes::stat_lineribbon(aes(y = .epred, fill = region, colour = region) ,
                               .width = c(0), linewidth = 1.5) +
    geom_point(data = odonata_species_richness, alpha = 0.8) +                                     # adding raw data
    scale_color_manual(values = c("#df7a5f", "#8cbca4")) +
    scale_fill_manual(values = c("#df7a5f", "#8cbca4")) +
    change_theme() +
    theme(legend.position = c(0.2, 0.87),
          plot.title = element_text(face = "bold",                              # adding legend 
                                    hjust = -0.17)) +
    labs(x = "\nHuman population trend\n(slope of change per year)",            # labels
         y = "Odonata species richness\n",
         title = "C\n") +
    geom_vline(xintercept = 0, linetype = "dotted") +                           # adding line to show zero-net change of population
    scale_y_continuous(limits = c(0, 24),
                       breaks = c(0, 8, 16, 24),                                # scale limits
                       labels = c("0", "8", "16", "24")) +
    scale_x_continuous(limits = c(-1.3, 0.3),
                       breaks = c(-1.2, -0.9, -0.6, -0.3, 0, 0.3),
                       labels = c("-1.2", "-0.9", "-0.6", "-0.3", "0", "0.3")))

#### Plants ------------------------------------------------------------------

#filter data frame for only plants
plant_richness <- richness_demo_land %>% filter(taxa == "Plants" &
                                             region != "NA" &
                                             year == 2024)

## Plant richness ~ human_pop_trend

# Model fit
plant_richness_model1 <- brm(bf(sp_richness ~ human_pop_trend_2024*region +           # human_pop as fixed effect, interacting with region
                                  (1|village_en)),                             # village as random (grouping factor)
                             data = plant_richness, family = poisson(),
                             prior = prior3, iter = 10000,
                             warmup = 2000,
                             init = '0',
                             cores = 4, chains = 4)
summary(plant_richness_model1)

# save model as RData
save(plant_richness_model1, file="data/models/plant_richness_model1.RData" )

# plot
(plants_richness_human_plot <- plant_richness %>%
    group_by(region) %>%
    modelr::data_grid(human_pop_trend_2024 = seq_range(human_pop_trend_2024, n = 101)) %>%
    add_epred_draws(plant_richness_model1, re_formula = NULL,                   # adding predicted draws from model
                    allow_new_levels = TRUE) %>%
    ggplot(aes(x = human_pop_trend_2024, y = sp_richness, color = region)) +         # plotting richness ~ human pop trend, colored by region      
    stat_lineribbon(aes(y = .epred, fill = region, colour = region) ,           # confidence intervals
                    .width = c(0.95), alpha = 0.3, size = 1.5) +
    stat_lineribbon(aes(y = .epred, fill = region, colour = region) ,           # estimates
                    .width = c(0), size = 1.5) +
    geom_point(data = plant_richness) +                                         # raw data
    scale_color_manual(values = c("#df7a5f", "#8cbca4")) +
    scale_fill_manual(values = c("#df7a5f", "#8cbca4")) +                       # colors based on regions
    change_theme() +                                                            # theme as specified in function
    #adding legend  
    theme(legend.position = "none",                                       # legend position
          plot.title = element_text(face = "bold",                              # title appearance 
                                    hjust = -0.17)) +
    labs(x = "\nHuman population trend\n(slope of change per year)",            # labels             
         y = "Plant species richness\n",
         title = "B\n") +
    geom_vline(xintercept = 0, linetype = "dotted") +                           # adding line to show zero-net change of population
    scale_y_continuous(limits = c(0, 240),
                       breaks = c(0, 80, 160, 240),                             # axis break specifications
                       labels = c("0", "80", "160", "240")) +
    scale_x_continuous(limits = c(-1.3, 0.3),
                       breaks = c(-1.2, -0.9, -0.6, -0.3, 0, 0.3),
                       labels = c("-1.2", "-0.9", "-0.6", "-0.3", "0", "0.3")))

# panel birds + plants
(richness_panel <- grid.arrange(birds_richness_human_plot,
                                    plants_richness_human_plot,
                                    odonata_richness_human_plot, 
                                    amphibian_richness_human_plot, nrow = 2))

ggsave(richness_panel, file = "figures/panel_sp_richness_4_taxa.png",
       height = 10,
       width = 10)

# Abundance ----

#### Birds -----
## Bird species abundance ~ human_pop_trend
bird_species_abundance <- abundance_demo_land %>% filter(taxa == "Birds")
hist(bird_species_abundance$abundance)

# running the model
bird_abundance_model <- brm(bf(abundance ~ human_pop_trend_2023*region + (1|village_en),
                               family = poisson()),
                            data = bird_species_abundance,
                           prior = prior2, iter = 10000,
                           warmup = 2000,
                           init = '0',
                           cores = 4, chains = 4)

summary(bird_abundance_model)

# saving the model
save(bird_abundance_model, file="data/models/bird_abundance_model.RData")

#plotting predictions
(birds_abundance_human_plot <- bird_species_abundance %>%
    group_by(region) %>%                                                        # as region is a predictor in the model, we group by it here
    modelr::data_grid(human_pop_trend_2023 = seq_range(human_pop_trend_2023, n = 101)) %>%
    add_epred_draws(bird_abundance_model, re_formula = NULL, allow_new_levels = TRUE) %>% #adding predicted draws from the model
    ggplot(aes(x = human_pop_trend_2023, y = abundance, color = region,            # plotting abundance~human_pop by region
               fill = region)) +
    tidybayes::stat_lineribbon(aes(y = .epred, fill = region, colour = region),           # adding confidence intervals
                               .width = c(0.95), alpha = 0.3, linewidth = 1.5) +
    tidybayes::stat_lineribbon(aes(y = .epred, fill = region, colour = region) ,
                               .width = c(0), linewidth = 1.5) +
    geom_point(data = bird_species_abundance, alpha = 0.8) +                                     # adding raw data
    scale_color_manual(values = c("#df7a5f", "#8cbca4")) +
    scale_fill_manual(values = c("#df7a5f", "#8cbca4")) +
    change_theme() +
    theme(legend.position = c(0.3, 0.87),
          plot.title = element_text(face = "bold",                              # adding legend 
                                    hjust = -0.17)) +
    labs(x = "\nHuman population trend\n(slope of change per year)",            # labels
         y = "Total bird abundance\n",
         title = "A\n") +
    geom_vline(xintercept = 0, linetype = "dotted") +                           # adding line to show zero-net change of population
    scale_y_continuous(limits = c(0, 165),
                       breaks = c(0, 55, 110, 165),                                # scale limits
                       labels = c("0", "55", "110", "165")) +
    scale_x_continuous(limits = c(-1.3, 0.3),
                       breaks = c(-1.2, -0.9, -0.6, -0.3, 0, 0.3),
                       labels = c("-1.2", "-0.9", "-0.6", "-0.3", "0", "0.3")))

#### Amphibia ----------------------------------------------------------------

amphibia_species_abundance <- abundance_demo_land %>% filter(taxa == "Amphibia")

## Amphibia abundance ~ human_pop_trend
amphibia_abundance_model1 <- brm(bf(abundance ~ human_pop_trend_2023*region +       # human_pop as fixed effect, interacting with region
                                     (1|village_en)),                           # village as random (grouping factor)
                                data = amphibia_species_abundance, 
                                family = poisson(),
                                prior = prior2, 
                                iter = 10000,
                                warmup = 2000,
                                init = '0',
                                cores = 4, chains = 4)

summary(amphibia_abundance_model1)
save(amphibia_abundance_model1, file = "data/models/amphibia_abundance_model1.RData" )

# plot
(amphibian_abundance_human_plot <- amphibia_species_abundance %>%
    group_by(region) %>%
    modelr::data_grid(human_pop_trend_2023 = seq_range(human_pop_trend_2023, n = 101)) %>%
    add_epred_draws(amphibia_abundance_model1, re_formula = NULL,                # adding predicted draws from model
                    allow_new_levels = TRUE) %>%
    ggplot(aes(x = human_pop_trend_2023, y = abundance, color = region,            # plotting abundance~human_pop by region
               fill = region)) +
    tidybayes::stat_lineribbon(aes(y = .epred, fill = region, colour = region),           # adding confidence intervals
                               .width = c(0.95), alpha = 0.3, linewidth = 1.5) +
    tidybayes::stat_lineribbon(aes(y = .epred, fill = region, colour = region) ,
                               .width = c(0), linewidth = 1.5) +
    geom_point(data = amphibia_species_abundance, alpha = 0.8) +                                     # adding raw data
    scale_color_manual(values = c("#df7a5f", "#8cbca4")) +
    scale_fill_manual(values = c("#df7a5f", "#8cbca4")) +
    change_theme() +
    theme(legend.position = c(0.3, 0.87),
          plot.title = element_text(face = "bold",                              # adding legend 
                                    hjust = -0.17)) +
    labs(x = "\nHuman population trend\n(slope of change per year)",            # labels
         y = "Total amphibian abundance\n",
         title = "C\n") +
    geom_vline(xintercept = 0, linetype = "dotted") +                           # adding line to show zero-net change of population
    scale_y_continuous(limits = c(0, 45),
                       breaks = c(0, 15, 30, 45),                                # scale limits
                       labels = c("0", "15", "30", "45")) +
    scale_x_continuous(limits = c(-1.3, 0.3),
                       breaks = c(-1.2, -0.9, -0.6, -0.3, 0, 0.3),
                       labels = c("-1.2", "-0.9", "-0.6", "-0.3", "0", "0.3")))

#### Odonata ----------------------------------------------------------------

# filter data frame for only dragonflies
odonata_species_abundance <- abundance_demo_land %>% filter(taxa == "Odonata")

# There was one false record of 31 Platycnemis pennipes in Voysil, correcting for that
# should be 54 - 31 = 23
odonata_species_abundance[111 ,22] <- 23

## Odonata abundance ~ human_pop_trend

# Model fit
odonata_abundance_model1 <- brm(bf(abundance ~ human_pop_trend_2023*region +         # human_pop as fixed effect, interacting with region
                                    (1|village_en)),                             # village as random (grouping factor)
                               data = odonata_species_abundance, family = poisson(),
                               prior = prior2, iter = 10000,
                               warmup = 2000,
                               init = '0',
                               cores = 4, chains = 4)

summary(odonata_abundance_model1)
# save model as RData
save(odonata_abundance_model1, file = "data/models/odonata_abundance_model1.RData" )

#plot
(odonata_abundance_human_plot <- odonata_species_abundance %>%
    group_by(region) %>%
    modelr::data_grid(human_pop_trend_2023 = seq_range(human_pop_trend_2023, n = 101)) %>%
    add_epred_draws(odonata_abundance_model1, re_formula = NULL, allow_new_levels = TRUE) %>%
    ggplot(aes(x = human_pop_trend_2023, y = abundance, color = region,            # plotting abundance~human_pop by region
               fill = region)) +
    tidybayes::stat_lineribbon(aes(y = .epred, fill = region, colour = region),           # adding confidence intervals
                               .width = c(0.95), alpha = 0.3, linewidth = 1.5) +
    tidybayes::stat_lineribbon(aes(y = .epred, fill = region, colour = region) ,
                               .width = c(0), linewidth = 1.5) +
    geom_point(data = odonata_species_abundance, alpha = 0.8) +                                     # adding raw data
    scale_color_manual(values = c("#df7a5f", "#8cbca4")) +
    scale_fill_manual(values = c("#df7a5f", "#8cbca4")) +
    change_theme() +
    theme(legend.position = c(0.3, 0.87),
          plot.title = element_text(face = "bold",                              # adding legend 
                                    hjust = -0.17)) +
    labs(x = "\nHuman population trend\n(slope of change per year)",            # labels
         y = "Total dragonfly and damselfly abundance\n",
         title = "B\n") +
    geom_vline(xintercept = 0, linetype = "dotted") +                           # adding line to show zero-net change of population
    scale_y_continuous(limits = c(0, 60),
                       breaks = c(0, 20, 40, 60),                                # scale limits
                       labels = c("0", "20", "40", "60")) +
    scale_x_continuous(limits = c(-1.3, 0.3),
                       breaks = c(-1.2, -0.9, -0.6, -0.3, 0, 0.3),
                       labels = c("-1.2", "-0.9", "-0.6", "-0.3", "0", "0.3")))

# panel
abundance_panel <- grid.arrange(birds_abundance_human_plot,
                                odonata_abundance_human_plot,
                                amphibian_abundance_human_plot, ncol = 3)

ggsave(abundance_panel, filename = "figures/abundance_panel.png",
       height = 4, width = 12)


#### Plants ------------------------------------------------------------------
load("data/plants2024_cover.RData")
       
## Plant cover ~ human_pop_trend
hist(plants2024_cover$layer_cover)

plants2024_cover <-plants2024_cover %>% ungroup() %>%
  dplyr::select(-year) 

plants2024_cover$village_en[plants2024_cover$village_en == "Zeleno_darvo"] <- "Zeleno darvo"
plants2024_cover$village_en[plants2024_cover$village_en == "Kreslyuvtsi"] <- "Kresluvtsi"
plants2024_cover$village_en[plants2024_cover$village_en == "Martsekovtsi"] <- "Marutsekovtsi"
plants2024_cover$village_en[plants2024_cover$village_en == "Stanchovhan"] <- "Stanchov han"
plants2024_cover$village_en[plants2024_cover$village_en == "Torbalazi"] <- "Torbalaji"

plants2024_cover$site <- parse_number(plants2024_cover$site)
plants2024_cover$site <- paste(plants2024_cover$village_en, 
                                  plants2024_cover$site, sep = "_")


# Add the metadata
plant_richness_meta <- plant_richness %>% dplyr::select(-sp_richness)

plant_richness_meta$village_en[plant_richness_meta$village_en == "Stanchov Han"] <- "Stanchov han"

plant_richness_meta$site <- parse_number(plant_richness_meta$site)
plant_richness_meta$site <- paste(plant_richness_meta$village_en, 
                                  plant_richness_meta$site, sep = "_")

plants2024_cover<- left_join(plants2024_cover, plant_richness_meta,
                              by = c("site", "village_en"))

plants2024_cover_lowland <- plants2024_cover %>% filter(region == "Lowland")
plants2024_cover_mountain <- plants2024_cover %>% filter(region == "Mountain")

# Model fit
plant_cover_model1 <- brm(bf(layer_cover ~ human_pop_trend_2024*layer2 +           # human_pop as fixed effect, interacting with region
                                  (1|village_en)),                             # village as random (grouping factor)
                             data = plants2024_cover_lowland, family = gaussian(),
                             prior = prior3, iter = 10000,
                             warmup = 2000,
                             init = '0',
                             cores = 4, chains = 4)
summary(plant_cover_model1)

# save model as RData
save(plant_cover_model1, file="data/models/plant_cover_model1.RData" )

# plot
(plants_cover_human_plot <- plants2024_cover_lowland %>%
    group_by(layer2) %>%
    modelr::data_grid(human_pop_trend_2024 = seq_range(human_pop_trend_2024, n = 101)) %>%
    add_epred_draws(plant_cover_model1, re_formula = NULL,                   # adding predicted draws from model
                    allow_new_levels = TRUE) %>%
    ggplot(aes(x = human_pop_trend_2024, y = layer_cover, color = layer2)) +         # plotting abundance ~ human pop trend, colored by region      
    stat_lineribbon(aes(y = .epred, fill = layer2, colour = layer2) ,           # confidence intervals
                    .width = c(0.95), alpha = 0.3, size = 1.5) +
    stat_lineribbon(aes(y = .epred, fill = layer2, colour = layer2) ,           # estimates
                    .width = c(0), size = 1.5) +
    geom_point(data = plants2024_cover_lowland) +                                         # raw data
    scale_color_manual(values = c("#dc79a2", "#bf5315", "#43657e")) +
    scale_fill_manual(values = c("#dc79a2", "#bf5315", "#43657e")) +                       # colors based on regions
    change_theme() +                                                            # theme as specified in function
    #adding legend  
    theme(legend.position = "bottom",                                       # legend position
          plot.title = element_text(face = "bold",                              # title appearance 
                                    hjust = -0.17)) +
    labs(x = "\nHuman population trend\n(slope of change per year)",            # labels             
         y = "Vegetation layer\n(% cover)\n",
         title = "A   Lowlands\n") +
    geom_vline(xintercept = 0, linetype = "dotted") +                           # adding line to show zero-net change of population
    scale_y_continuous(limits = c(0, 200),
                       breaks = c(0, 50, 100, 150, 200),                             # axis break specifications
                       labels = c("0", "50", "100", "150", "200")) +
    scale_x_continuous(limits = c(-1.3, 0.3),
                       breaks = c(-1.2, -0.9, -0.6, -0.3, 0, 0.3),
                       labels = c("-1.2", "-0.9", "-0.6", "-0.3", "0", "0.3")))

# Model fit
plant_cover_model2 <- brm(bf(layer_cover ~ human_pop_trend_2024*layer2 +           # human_pop as fixed effect, interacting with region
                               (1|village_en)),                             # village as random (grouping factor)
                          data = plants2024_cover_mountain, family = gaussian(),
                          prior = prior3, iter = 10000,
                          warmup = 2000,
                          init = '0',
                          cores = 4, chains = 4)
summary(plant_cover_model2)

# save model as RData
save(plant_cover_model2, file="data/models/plant_cover_model2.RData")

# plot
(plants_cover_human_plot2 <- plants2024_cover_mountain %>%
    group_by(layer2) %>%
    modelr::data_grid(human_pop_trend_2024 = seq_range(human_pop_trend_2024, n = 101)) %>%
    add_epred_draws(plant_cover_model2, re_formula = NULL,                   # adding predicted draws from model
                    allow_new_levels = TRUE) %>%
    ggplot(aes(x = human_pop_trend_2024, y = layer_cover, color = layer2)) +         # plotting abundance ~ human pop trend, colored by region      
    stat_lineribbon(aes(y = .epred, fill = layer2, colour = layer2) ,           # confidence intervals
                    .width = c(0.95), alpha = 0.3, size = 1.5) +
    stat_lineribbon(aes(y = .epred, fill = layer2, colour = layer2) ,           # estimates
                    .width = c(0), size = 1.5) +
    geom_point(data = plants2024_cover_mountain) +                                         # raw data
    scale_color_manual(values = c("#dc79a2", "#bf5315", "#43657e")) +
    scale_fill_manual(values = c("#dc79a2", "#bf5315", "#43657e")) +                       # colors based on regions
    change_theme() +                                                            # theme as specified in function
    #adding legend  
    theme(legend.position = "bottom",                                       # legend position
          plot.title = element_text(face = "bold",                              # title appearance 
                                    hjust = -0.17)) +
    labs(x = "\nHuman population trend\n(slope of change per year)",            # labels             
         y = "Vegetation layer\n(% cover)\n",
         title = "B   Mountains\n") +
    geom_vline(xintercept = 0, linetype = "dotted") +
    scale_y_continuous(limits = c(0, 200),
                       breaks = c(0, 50, 100, 150, 200),                             # axis break specifications
                       labels = c("0", "50", "100", "150", "200")) +
    scale_x_continuous(limits = c(-1.3, 0.3),
                       breaks = c(-1.2, -0.9, -0.6, -0.3, 0, 0.3),
                       labels = c("-1.2", "-0.9", "-0.6", "-0.3", "0", "0.3")))


# Now as proportions of the total vegetation

plants2024_cover_lowland <- plants2024_cover_lowland %>%
  group_by(site) %>%
  mutate(total_veg_cover = sum(layer_cover)) %>%
  group_by(site, layer2) %>%
  mutate(prop_layer_cover = layer_cover/total_veg_cover*100)

# Model fit
plant_cover_model3 <- brm(bf(prop_layer_cover ~ human_pop_trend_2024*layer2 +           # human_pop as fixed effect, interacting with region
                               (1|village_en)),                             # village as random (grouping factor)
                          data = plants2024_cover_lowland, family = gaussian(),
                          prior = prior3, iter = 10000,
                          warmup = 2000,
                          init = '0',
                          cores = 4, chains = 4)
summary(plant_cover_model3)

# save model as RData
save(plant_cover_model3, file="data/models/plant_cover_model3.RData" )

# plot
(plants_cover_human_plot3 <- plants2024_cover_lowland %>%
    group_by(layer2) %>%
    modelr::data_grid(human_pop_trend_2024 = seq_range(human_pop_trend_2024, n = 101)) %>%
    add_epred_draws(plant_cover_model3, re_formula = NULL,                   # adding predicted draws from model
                    allow_new_levels = TRUE) %>%
    ggplot(aes(x = human_pop_trend_2024, y = prop_layer_cover, color = layer2)) +         # plotting abundance ~ human pop trend, colored by region      
    stat_lineribbon(aes(y = .epred, fill = layer2, colour = layer2) ,           # confidence intervals
                    .width = c(0.95), alpha = 0.3, size = 1.5) +
    stat_lineribbon(aes(y = .epred, fill = layer2, colour = layer2) ,           # estimates
                    .width = c(0), size = 1.5) +
    geom_point(data = plants2024_cover_lowland) +                                         # raw data
    scale_color_manual(values = c("#dc79a2", "#bf5315", "#43657e")) +
    scale_fill_manual(values = c("#dc79a2", "#bf5315", "#43657e")) +                       # colors based on regions
    change_theme() +                                                            # theme as specified in function
    #adding legend  
    theme(legend.position = "bottom",                                       # legend position
          plot.title = element_text(face = "bold",                              # title appearance 
                                    hjust = -0.17)) +
    labs(x = "\nHuman population trend\n(slope of change per year)",            # labels             
         y = "Vegetation layer\n(% cover)\n",
         title = "A   Lowlands\n") +
    geom_vline(xintercept = 0, linetype = "dotted") +
    scale_y_continuous(limits = c(0, 100),
                       breaks = c(0, 25, 50, 75, 100),                             # axis break specifications
                       labels = c("0", "25", "50", "75", "100")) +
    scale_x_continuous(limits = c(-1.3, 0.3),
                       breaks = c(-1.2, -0.9, -0.6, -0.3, 0, 0.3),
                       labels = c("-1.2", "-0.9", "-0.6", "-0.3", "0", "0.3")))

plants2024_cover_mountain <- plants2024_cover_mountain %>%
  group_by(site) %>%
  mutate(total_veg_cover = sum(layer_cover)) %>%
  group_by(site, layer2) %>%
  mutate(prop_layer_cover = layer_cover/total_veg_cover*100)

# Model fit
plant_cover_model4 <- brm(bf(prop_layer_cover ~ human_pop_trend_2024*layer2 +           # human_pop as fixed effect, interacting with region
                               (1|village_en)),                             # village as random (grouping factor)
                          data = plants2024_cover_mountain, family = gaussian(),
                          prior = prior3, iter = 10000,
                          warmup = 2000,
                          init = '0',
                          cores = 4, chains = 4)
summary(plant_cover_model4)

# save model as RData
save(plant_cover_model4, file="data/models/plant_cover_model4.RData" )

# plot
(plants_cover_human_plot4 <- plants2024_cover_mountain %>%
    group_by(layer2) %>%
    modelr::data_grid(human_pop_trend_2024 = seq_range(human_pop_trend_2024, n = 101)) %>%
    add_epred_draws(plant_cover_model4, re_formula = NULL,                   # adding predicted draws from model
                    allow_new_levels = TRUE) %>%
    ggplot(aes(x = human_pop_trend_2024, y = prop_layer_cover, color = layer2)) +         # plotting abundance ~ human pop trend, colored by region      
    stat_lineribbon(aes(y = .epred, fill = layer2, colour = layer2) ,           # confidence intervals
                    .width = c(0.95), alpha = 0.3, size = 1.5) +
    stat_lineribbon(aes(y = .epred, fill = layer2, colour = layer2) ,           # estimates
                    .width = c(0), size = 1.5) +
    geom_point(data = plants2024_cover_mountain) +                                         # raw data
    scale_color_manual(values = c("#dc79a2", "#bf5315", "#43657e")) +
    scale_fill_manual(values = c("#dc79a2", "#bf5315", "#43657e")) +                       # colors based on regions
    change_theme() +                                                            # theme as specified in function
    #adding legend  
    theme(legend.position = "bottom",                                       # legend position
          plot.title = element_text(face = "bold",                              # title appearance 
                                    hjust = -0.17)) +
    labs(x = "\nHuman population trend\n(slope of change per year)",            # labels             
         y = "Vegetation layer\n(% cover)\n",
         title = "B   Mountains\n") +
    geom_vline(xintercept = 0, linetype = "dotted") +
    scale_y_continuous(limits = c(0, 100),
                       breaks = c(0, 25, 50, 75, 100),                             # axis break specifications
                       labels = c("0", "25", "50", "75", "100")) +
    scale_x_continuous(limits = c(-1.3, 0.3),
                       breaks = c(-1.2, -0.9, -0.6, -0.3, 0, 0.3),
                       labels = c("-1.2", "-0.9", "-0.6", "-0.3", "0", "0.3")))

# Panel
cover_panel <- grid.arrange(plants_cover_human_plot3,
                            plants_cover_human_plot4, ncol = 2)

ggsave(cover_panel, filename = "figures/cover_panel.png", height = 5, width = 10)

# One model combined
plants2024_cover <- plants2024_cover %>%
  group_by(site) %>%
  mutate(total_veg_cover = sum(layer_cover)) %>%
  group_by(site, layer2) %>%
  mutate(prop_layer_cover = layer_cover/total_veg_cover*100)

# Model fit
plant_cover_model5 <- brm(bf(prop_layer_cover ~ human_pop_trend_2024*layer2 +           # human_pop as fixed effect, interacting with region
                               (1|village_en)),                             # village as random (grouping factor)
                          data = plants2024_cover, family = gaussian(),
                          prior = prior3, iter = 10000,
                          warmup = 2000,
                          init = '0',
                          cores = 4, chains = 4)
summary(plant_cover_model5)

# save model as RData
save(plant_cover_model5, file="data/models/plant_cover_model5.RData" )

# plot
(plants_cover_human_plot5 <- plants2024_cover %>%
    group_by(layer2) %>%
    modelr::data_grid(human_pop_trend_2024 = seq_range(human_pop_trend_2024, n = 101)) %>%
    add_epred_draws(plant_cover_model5, re_formula = NULL,                   # adding predicted draws from model
                    allow_new_levels = TRUE) %>%
    ggplot(aes(x = human_pop_trend_2024, y = prop_layer_cover, color = layer2)) +         # plotting abundance ~ human pop trend, colored by region      
    stat_lineribbon(aes(y = .epred, fill = layer2, colour = layer2) ,           # confidence intervals
                    .width = c(0.95), alpha = 0.3, size = 1.5) +
    stat_lineribbon(aes(y = .epred, fill = layer2, colour = layer2) ,           # estimates
                    .width = c(0), size = 1.5) +
    geom_point(data = plants2024_cover) +                                         # raw data
    scale_color_manual(values = c("#dc79a2", "#bf5315", "#43657e")) +
    scale_fill_manual(values = c("#dc79a2", "#bf5315", "#43657e")) +                       # colors based on regions
    change_theme() +                                                            # theme as specified in function
    #adding legend  
    theme(legend.position = "bottom",                                       # legend position
          plot.title = element_text(face = "bold",                              # title appearance 
                                    hjust = -0.17)) +
    labs(x = "\nHuman population trend\n(slope of change per year)",            # labels             
         y = "Vegetation layer\n(% cover)\n",
         title = "C   Both regions\n") +
    geom_vline(xintercept = 0, linetype = "dotted") +
    scale_y_continuous(limits = c(0, 100),
                       breaks = c(0, 25, 50, 75, 100),                             # axis break specifications
                       labels = c("0", "25", "50", "75", "100")) +
    scale_x_continuous(limits = c(-1.3, 0.3),
                       breaks = c(-1.2, -0.9, -0.6, -0.3, 0, 0.3),
                       labels = c("-1.2", "-0.9", "-0.6", "-0.3", "0", "0.3")))

# Panel
cover_panel <- grid.arrange(plants_cover_human_plot3,
                            plants_cover_human_plot4, plants_cover_human_plot5, ncol = 3)

ggsave(cover_panel, filename = "figures/cover_panel2.png", height = 5, width = 13)

# Community composition ----
library(vegan)

# loading data
bird_abundance <- read_excel("data/bird_abundance.xlsx")

plants2024_raw <- read_csv("data/plants2024_for_editingGD.csv")
plants2024_raw <- plants2024_raw %>% drop_na(village)

colnames(plants2024_raw)[1] <- "village_en"

plants2024_raw$village_en[plants2024_raw$village_en == "Zeleno_darvo"] <- "Zeleno darvo"
plants2024_raw$village_en[plants2024_raw$village_en == "Kreslyuvtsi"] <- "Kresluvtsi"
plants2024_raw$village_en[plants2024_raw$village_en == "Martsekovtsi"] <- "Marutsekovtsi"
plants2024_raw$village_en[plants2024_raw$village_en == "Stanchovhan"] <- "Stanchov han"
plants2024_raw$village_en[plants2024_raw$village_en == "Torbalazi"] <- "Torbalaji"

# Data preparation

#steps: creating matrix with 30 rows (30 villages) and unique (species) columns, values = % cover
# also create identifier to keep track which row belongs to which village (important later)
# run NMDS to see community differences

### Birds ----

# preparing bird dataframe
bird_abundance_1 <- bird_abundance %>% 
  #selecting only three columns and distinct values
  dplyr:: select(village_en, species, abundance) %>% distinct() %>%
  as.data.frame() #matrify() cannot handle tibbles, therefore going for dataframe

# creating the matrix

bird_matrix <- labdsv::matrify(bird_abundance_1) 
bird_matrix[is.na(bird_matrix)] <- 0

# NMDS
bird_mds <- metaMDS(bird_matrix)

# Plot
village_status_meta2023 <- read_csv("data/village_pairs_status.csv")

village_status_meta2024 <- read_csv("data/village_pairs_status2024.csv")

village_status_meta2023 <- village_status_meta2023 %>% dplyr::select(village_en, pair,
                                                             region, status)

# extract the values from the NMDS for the different villages
data_scores <- 
  # creating it as dataframe
  as.data.frame(
    # extracting scores
    vegan::scores(bird_mds, 
                  #only taking value for site (not species)
                  display = "sites",
                  #choosing tidy format for ggplot
                  tidy = T))  %>% 
  rename(village_en = label) %>% 
  left_join(village_status_meta2023) %>% 
  mutate(region_status = paste(region, status, sep = "_"))

hull_data_status <- data_scores %>%
  #defining groups as region
  split(.$region_status) %>%
  #mapping the outer polygon edges
  map_dfr(~ .x[chull(.x$NMDS1, .x$NMDS2), ], .id = "region_status")

#plot
(bird_status_plot <- ggplot() + 
    geom_polygon(data = hull_data_status, aes(x = NMDS1, y = NMDS2, 
                                              fill = region_status, group = region_status),alpha=0.30)+
    geom_point(data = data_scores,aes(x = NMDS1, y = NMDS2,
                                      shape=region_status,
                                      colour=region_status),size=3)+  
    change_theme() + 
    theme(plot.title = element_text(face = "bold",
                                    hjust = -0.10)) +
    scale_x_continuous(limits = c(-1.0, 0.7), ,
                       breaks = c(-1.0, -0.50, 0, 0.50),
                       labels = c("-1.00", "-0.50", "0", "0.50")) +    
    scale_y_continuous(limits = c(-0.4, 0.6),
                       breaks = c(-0.40, -0.20, 0, 0.20, 0.40),
                       labels = c("-0.4", "-0.2", "0", "0.2", "0.4")) +
    scale_color_manual(values = c("#8cbca4", "#df7a5f",  "#8cbca4", "#df7a5f", "#8cbca4"),
                       breaks = c("Mountain_abandoned",
                                  "Lowland_increasing",
                                  "Mountain_decreasing",
                                  "Lowland_decreasing",
                                  "Mountain_stable"),
                       labels = c("Fully abandoned mountain villages",
                                  "Lowland villages with increasing population",
                                  "Depopulating mountain villages",
                                  "Depopulating lowland villages",
                                  "Mountain villages with stable population")) +
    scale_fill_manual(values = c("#8cbca4", "#df7a5f",  "#8cbca4", "#df7a5f", "#8cbca4"),
                      breaks = c("Mountain_abandoned",
                                 "Lowland_increasing",
                                 "Mountain_decreasing",
                                 "Lowland_decreasing",
                                 "Mountain_stable"),
                      labels = c("Fully abandoned mountain villages",
                                 "Lowland villages with increasing population",
                                 "Depopulating mountain villages",
                                 "Depopulating lowland villages",
                                 "Mountain villages with stable population")) +
    scale_shape_discrete(breaks = c("Mountain_abandoned",
                                    "Lowland_increasing",
                                    "Mountain_decreasing",
                                    "Lowland_decreasing",
                                    "Mountain_stable"),
                         labels = c("Fully abandoned mountain villages",
                                    "Lowland villages with increasing population",
                                    "Depopulating mountain villages",
                                    "Depopulating lowland villages",
                                    "Mountain villages with stable population")) +
    theme(panel.grid = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 12)) +
    guides(fill = guide_legend(nrow = 3, byrow = TRUE),
           colour = guide_legend(nrow = 3, byrow = TRUE),
           shape = guide_legend(nrow = 3, byrow = TRUE)) +
    labs(title="A   Birds\n"))

ggsave("figures/birds_NMDS.png",
       height = 7.7,
       width = 7.5)

bird_mds$stress

### Plants ------
library(labdsv)

plants2024 <- plants2024_raw %>% 
  dplyr::select(village_en, species2, cover_final)

plants2024 <- as.data.frame(plants2024)

plant_matrix <- labdsv::matrify(plants2024)
plant_matrix[is.na(plant_matrix)] <- 0

#NMDS analysis
plant_mds <- metaMDS(plant_matrix)

#stress low
plant_mds$stress

# extract the values from the NMDS for the different villages
data_scores2 <- 
  # creating it as dataframe
  as.data.frame(
    # extracting scores
    vegan::scores(plant_mds, 
                  #only taking value for site (not species)
                  display = "sites",
                  #choosing tidy format for ggplot
                  tidy = T))  %>% 
  rename(village_en = label) %>% 
  left_join(village_status_meta2024) %>% 
  mutate(region_status = paste(region, status, sep = "_"))

hull_data_status2 <- data_scores2 %>%
  #defining groups as region
  split(.$region_status) %>%
  #mapping the outer polygon edges
  map_dfr(~ .x[chull(.x$NMDS1, .x$NMDS2), ], .id = "region_status")


#plot
(plant_status_plot <- ggplot() + 
    geom_polygon(data = hull_data_status2, aes(x = NMDS1, y = NMDS2, 
                                              fill = region_status, group = region_status),alpha=0.30)+
    geom_point(data = data_scores2,aes(x = NMDS1, y = NMDS2,
                                      shape=region_status,
                                      colour=region_status),size=3)+  
    change_theme() + 
    theme(plot.title = element_text(face = "bold",
                                    hjust = -0.10)) +
    scale_x_continuous(limits = c(-1.0, 0.7), ,
                       breaks = c(-1.0, -0.50, 0, 0.50),
                       labels = c("-1.00", "-0.50", "0", "0.50")) +    
    scale_y_continuous(limits = c(-0.4, 0.6),
                       breaks = c(-0.40, -0.20, 0, 0.20, 0.40),
                       labels = c("-0.4", "-0.2", "0", "0.2", "0.4")) +
    scale_color_manual(values = c("#8cbca4", "#df7a5f",  "#8cbca4", "#df7a5f", "#8cbca4"),
                       breaks = c("Mountain_abandoned",
                                  "Lowland_increasing",
                                  "Mountain_decreasing",
                                  "Lowland_decreasing",
                                  "Mountain_stable"),
                       labels = c("Fully abandoned mountain villages",
                                  "Lowland villages with increasing population",
                                  "Depopulating mountain villages",
                                  "Depopulating lowland villages",
                                  "Mountain villages with stable population")) +
    scale_fill_manual(values = c("#8cbca4", "#df7a5f",  "#8cbca4", "#df7a5f", "#8cbca4"),
                      breaks = c("Mountain_abandoned",
                                 "Lowland_increasing",
                                 "Mountain_decreasing",
                                 "Lowland_decreasing",
                                 "Mountain_stable"),
                      labels = c("Fully abandoned mountain villages",
                                 "Lowland villages with increasing population",
                                 "Depopulating mountain villages",
                                 "Depopulating lowland villages",
                                 "Mountain villages with stable population")) +
    scale_shape_discrete(breaks = c("Mountain_abandoned",
                                    "Lowland_increasing",
                                    "Mountain_decreasing",
                                    "Lowland_decreasing",
                                    "Mountain_stable"),
                         labels = c("Fully abandoned mountain villages",
                                    "Lowland villages with increasing population",
                                    "Depopulating mountain villages",
                                    "Depopulating lowland villages",
                                    "Mountain villages with stable population")) +
    theme(panel.grid = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 12)) +
    guides(fill = guide_legend(nrow = 3, byrow = TRUE),
           colour = guide_legend(nrow = 3, byrow = TRUE),
           shape = guide_legend(nrow = 3, byrow = TRUE)) +
    labs(title="B   Plants\n"))

ggsave("figures/plants_NMDS.png",
       height = 7.7,
       width = 7.5)

(nmds_panel <- grid.arrange(bird_status_plot, plant_status_plot, ncol = 2))

ggsave(nmds_panel, file = "figures/bird_plant_nmds.png",
       height = 10, width = 15)

### Odonata -----------------------------------------------------------------
DataBase_od_am_all <- read_excel("data/DataBase_od_am_all.xlsx") %>%
  dplyr::select(-taxa)

species_to_taxa_key <- read_csv("data/species_to_taxa_key.csv")

DataBase_od_am_all <- left_join(DataBase_od_am_all, species_to_taxa_key,
                                by = "species")

# creating data matrix
odonata <- DataBase_od_am_all %>% 
  filter(taxa == "Odonata",
         species != "няма регистрации",
         species != "richness 0",
         age %in% c("Ad", "ad", "ad+juv", "Sub", "juv+ad")) %>%                # only picking out odonata data points
  dplyr::select(village_en, species, abundance) %>% # only three columns for matrix
  as.data.frame() %>%                        # matrify needs a dataframe
  matrify()

#running nmds analysis 
odonata_mds <- metaMDS(odonata)

#stress is 0.12
odonata_mds$stress

# extract the values from the NMDS for the different villages
data_scores3 <- 
  # creating it as dataframe
  as.data.frame(
    # extracting scores
    vegan::scores(odonata_mds, 
                  #only taking value for site (not species)
                  display = "sites",
                  #choosing tidy format for ggplot
                  tidy = T))  %>% 
  rename(village_en = label) %>% 
  left_join(village_status_meta2023) %>% 
  mutate(region_status = paste(region, status, sep = "_"))

hull_data_status3 <- data_scores3 %>%
  #defining groups as region
  split(.$region_status) %>%
  #mapping the outer polygon edges
  map_dfr(~ .x[chull(.x$NMDS1, .x$NMDS2), ], .id = "region_status")


#plot
(odonata_status_plot <- ggplot() + 
    geom_polygon(data = hull_data_status3, aes(x = NMDS1, y = NMDS2, 
                                               fill = region_status, group = region_status),alpha=0.30)+
    geom_point(data = data_scores3, aes(x = NMDS1, y = NMDS2,
                                       shape=region_status,
                                       colour=region_status),size=3)+  
    change_theme() + 
    theme(plot.title = element_text(face = "bold",
                                    hjust = -0.10)) +
  #  scale_x_continuous(limits = c(-1.0, 1.4), ,
  #                     breaks = c(-1.0, -0.50, 0, 0.50, 1.00),
  #                     labels = c("-1.00", "-0.50", "0", "0.50", "1.00")) +    
  #  scale_y_continuous(limits = c(-1.4, 1.4),
  #                     breaks = c(-1.0, -0.50, 0, 0.50, 1.00),
  #                     labels = c("-1.0", "-0.50", "0", "0.50", "1.00")) +
    scale_color_manual(values = c("#8cbca4", "#df7a5f",  "#8cbca4", "#df7a5f", "#8cbca4"),
                       breaks = c("Mountain_abandoned",
                                  "Lowland_increasing",
                                  "Mountain_decreasing",
                                  "Lowland_decreasing",
                                  "Mountain_stable"),
                       labels = c("Fully abandoned mountain villages",
                                  "Lowland villages with increasing population",
                                  "Depopulating mountain villages",
                                  "Depopulating lowland villages",
                                  "Mountain villages with stable population")) +
    scale_fill_manual(values = c("#8cbca4", "#df7a5f",  "#8cbca4", "#df7a5f", "#8cbca4"),
                      breaks = c("Mountain_abandoned",
                                 "Lowland_increasing",
                                 "Mountain_decreasing",
                                 "Lowland_decreasing",
                                 "Mountain_stable"),
                      labels = c("Fully abandoned mountain villages",
                                 "Lowland villages with increasing population",
                                 "Depopulating mountain villages",
                                 "Depopulating lowland villages",
                                 "Mountain villages with stable population")) +
    scale_shape_discrete(breaks = c("Mountain_abandoned",
                                    "Lowland_increasing",
                                    "Mountain_decreasing",
                                    "Lowland_decreasing",
                                    "Mountain_stable"),
                         labels = c("Fully abandoned mountain villages",
                                    "Lowland villages with increasing population",
                                    "Depopulating mountain villages",
                                    "Depopulating lowland villages",
                                    "Mountain villages with stable population")) +
    theme(panel.grid = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 12)) +
    guides(fill = guide_legend(nrow = 3, byrow = TRUE),
           colour = guide_legend(nrow = 3, byrow = TRUE),
           shape = guide_legend(nrow = 3, byrow = TRUE)) +
    labs(title="C   Dragonflies and damselflies\n"))

ggsave("figures/odonata_NMDS.png",
       height = 7.7,
       width = 7.5)

## Amphibia ----------------------------------------------------------------
load("data/od_am_rept_1.RData")

#creating data matrix
amphibia <- od_am_rept_1 %>% 
  filter(taxa == "Amphibia",                #only picking out amphibia data points
         species != "няма регистрации",
         species != "richness 0",
         age %in% c("Ad", "ad", "ad+juv", "Sub", "juv+ad")) %>% 
  dplyr::select(village_en, species, abundance) %>% #only three columns for matrix
  as.data.frame() %>%                        #matrify needs a dataframe
  matrify()

#running nmds analysis 
amphibia_mds <-metaMDS(amphibia)

#stress is 0.1
amphibia_mds$stress

# extract the values from the NMDS for the different villages
data_scores4 <- 
  # creating it as dataframe
  as.data.frame(
    # extracting scores
    vegan::scores(amphibia_mds, 
                  #only taking value for site (not species)
                  display = "sites",
                  #choosing tidy format for ggplot
                  tidy = T))  %>% 
  rename(village_en = label) %>% 
  left_join(village_status_meta2023) %>% 
  mutate(region_status = paste(region, status, sep = "_"))

hull_data_status4 <- data_scores4 %>%
  #defining groups as region
  split(.$region_status) %>%
  #mapping the outer polygon edges
  map_dfr(~ .x[chull(.x$NMDS1, .x$NMDS2), ], .id = "region_status")


#plot
(amphibia_status_plot <- ggplot() + 
    geom_polygon(data = hull_data_status4, aes(x = NMDS1, y = NMDS2, 
                                               fill = region_status, group = region_status),alpha=0.30)+
    geom_point(data = data_scores4, aes(x = NMDS1, y = NMDS2,
                                        shape=region_status,
                                        colour=region_status),size=3)+  
    change_theme() + 
    theme(plot.title = element_text(face = "bold",
                                    hjust = -0.10)) +
    #  scale_x_continuous(limits = c(-1.0, 1.4), ,
    #                     breaks = c(-1.0, -0.50, 0, 0.50, 1.00),
    #                     labels = c("-1.00", "-0.50", "0", "0.50", "1.00")) +    
    #  scale_y_continuous(limits = c(-1.4, 1.4),
    #                     breaks = c(-1.0, -0.50, 0, 0.50, 1.00),
    #                     labels = c("-1.0", "-0.50", "0", "0.50", "1.00")) +
    scale_color_manual(values = c("#8cbca4", "#df7a5f",  "#8cbca4", "#df7a5f", "#8cbca4"),
                       breaks = c("Mountain_abandoned",
                                  "Lowland_increasing",
                                  "Mountain_decreasing",
                                  "Lowland_decreasing",
                                  "Mountain_stable"),
                       labels = c("Fully abandoned mountain villages",
                                  "Lowland villages with increasing population",
                                  "Depopulating mountain villages",
                                  "Depopulating lowland villages",
                                  "Mountain villages with stable population")) +
    scale_fill_manual(values = c("#8cbca4", "#df7a5f",  "#8cbca4", "#df7a5f", "#8cbca4"),
                      breaks = c("Mountain_abandoned",
                                 "Lowland_increasing",
                                 "Mountain_decreasing",
                                 "Lowland_decreasing",
                                 "Mountain_stable"),
                      labels = c("Fully abandoned mountain villages",
                                 "Lowland villages with increasing population",
                                 "Depopulating mountain villages",
                                 "Depopulating lowland villages",
                                 "Mountain villages with stable population")) +
    scale_shape_discrete(breaks = c("Mountain_abandoned",
                                    "Lowland_increasing",
                                    "Mountain_decreasing",
                                    "Lowland_decreasing",
                                    "Mountain_stable"),
                         labels = c("Fully abandoned mountain villages",
                                    "Lowland villages with increasing population",
                                    "Depopulating mountain villages",
                                    "Depopulating lowland villages",
                                    "Mountain villages with stable population")) +
    theme(panel.grid = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 12)) +
    guides(fill = guide_legend(nrow = 3, byrow = TRUE),
           colour = guide_legend(nrow = 3, byrow = TRUE),
           shape = guide_legend(nrow = 3, byrow = TRUE)) +
    labs(title = "D   Amphibians\n"))

ggsave("figures/amphibia_NMDS.png",
       height = 7.7,
       width = 7.5)

# All four together

nmds_all <- grid.arrange(bird_status_plot, plant_status_plot,
                         odonata_status_plot, amphibia_status_plot, ncol = 2)

ggsave(nmds_all, filename = "figures/nmds_all.png", height = 12, width = 12)

# Bray-Curtis
bray_pop_diff_model<-brm(bf(bray_birds ~ pop_trend_diff*region1 + (1|site1)), 
                         data = bray_birds1, family = Beta(),   #family Beta encompasses the interval (0,1)
                         iter = 6000,
                         warmup = 2000,
                         init = '0',
                         cores = 4,
                         chains = 4)

# Jaccard ----

# making everything numeric 
bird_matrix2 <- bird_matrix %>%
  mutate(across(.cols = where(is.factor), .fns = as.numeric))



#### Birds----

#calculating jaccard index for the villages
jaccard_birds <- as.data.frame(as.matrix(vegdist(bird_matrix2, method = "jaccard"))) 

# adding column with villagename
jaccard_birds$site1 <- rownames(jaccard_birds)

village_status_meta2023$site1 <- village_status_meta2023$village_en
village_status_meta2023b <- village_status_meta2023
village_status_meta2023b$site2 <- village_status_meta2023b$village_en
village_status_meta2023b$status2 <- village_status_meta2023b$status

# Transforming into long format and adding different dataframes containing information about land use and village pairs
jaccard_birds_mountain <- jaccard_birds %>%
  gather(site2, jaccard, 1:30) %>%       #long format
  left_join(village_status_meta2023, join_by("site1")) %>%         # adding status for site 1 
  #rename(site2 = site2.x) %>% 
  left_join(village_status_meta2023b, join_by("site2")) %>%  #adding status for site2
  dplyr::select(site1.x, site2, jaccard, pair.x, pair.y, 
                region.x, region.y, status.x, status2) %>%
  rename(site1 = site1.x,
         pair1 = pair.x,
         region1 = region.x, region2 = region.y,
         status1 = status.x) %>%
  mutate(test = site1 == site2,
         comparison = paste(status1, status2, sep = "_")) %>%  #creating new column that shows status comparison
  filter(test == FALSE,
         region1 == "Mountain",
         region2 == "Mountain") %>% 
  droplevels()%>%
  ungroup()

# In the mountains the comparisons are:

# stable - decreasing
# stable - stable
# stable - abandoned
# abandoned - decreasing
# abandoned - abandoned
# decreasing - decreasing

unique(jaccard_birds_mountain$comparison)

jaccard_birds_mountain <- jaccard_birds_mountain %>%
  filter(!comparison %in% c("stable_abandoned", "stable_decreasing",
                            "decreasing_abandoned"))

library(ggridges)

# creating dataframe with mean of jaccard index
mean_jaccard_birds_mountain <- jaccard_birds_mountain %>%
  group_by(comparison, region1) %>%
  summarise(mean_jaccard = mean(jaccard, na.rm = TRUE), .groups = "drop")

jaccard_birds_mountain <- jaccard_birds_mountain %>%
  group_by(comparison) %>%
  mutate(mean.jaccard = mean(jaccard))

# Plotting 
(jaccard_birds_mountain_plot <- ggplot() +
  geom_density_ridges(data = jaccard_birds_mountain,
                      aes(x = jaccard, y = comparison,
                          fill = comparison, 
                          colour = comparison), alpha = 0.5,
                      quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x)) +
  change_theme() +
  theme(legend.position = "n") +
    labs(title = "B   Birds in the mountains\n"))

#saving plot
ggsave(jaccard_birds_mountain_plot, file = "figures/bird_status_comparison_jaccard_mountain.png",
       height = 5,
       width = 7)

# Transforming into long format and adding different dataframes containing information about land use and village pairs
jaccard_birds_lowland <- jaccard_birds %>%
  gather(site2, jaccard, 1:30) %>%       #long format
  left_join(village_status_meta2023, join_by("site1")) %>%         # adding status for site 1 
  #rename(site2 = site2.x) %>% 
  left_join(village_status_meta2023b, join_by("site2")) %>%  #adding status for site2
  dplyr::select(site1.x, site2, jaccard, pair.x, pair.y, 
                region.x, region.y, status.x, status2) %>%
  rename(site1 = site1.x,
         pair1 = pair.x,
         region1 = region.x, region2 = region.y,
         status1 = status.x) %>%
  mutate(test = site1 == site2,
         comparison = paste(status1, status2, sep = "_")) %>%  #creating new column that shows status comparison
  filter(test == FALSE,
         region1 == "Lowland",
         region2 == "Lowland") %>% 
  droplevels()%>%
  ungroup()

# In the lowlands the comparisons are:

# increasing - decreasing
# increasing - increasing
# decreasing - decreasing

unique(jaccard_birds_lowland$comparison)

jaccard_birds_lowland <- jaccard_birds_lowland %>%
  filter(comparison != "increasing_decreasing")

# Plotting 
(jaccard_birds_lowland_plot <- ggplot() +
    geom_density_ridges(data = jaccard_birds_lowland,
                        aes(x = jaccard, y = comparison,
                            fill = comparison, 
                            colour = comparison), alpha = 0.5,
                        quantile_lines=TRUE,
                        quantile_fun=function(x,...)mean(x)) +
    change_theme() +
    theme(legend.position = "n") +
    labs(title = "A   Birds in the lowlands\n"))

#saving plot
ggsave(jaccard_birds_lowland_plot, file = "figures/bird_status_comparison_jaccard_lowland.png",
       height = 5,
       width = 7)

#### Plants -----

# making everything numeric 
plant_matrix2 <- plant_matrix %>%
  mutate(across(.cols = where(is.factor), .fns = as.numeric))

#calculating jaccard index for the villages
jaccard_plants <- as.data.frame(as.matrix(vegdist(plant_matrix2, method = "jaccard"))) 

# adding column with villagename
jaccard_plants$site1 <- rownames(jaccard_plants)

village_status_meta2024$site1 <- village_status_meta2024$village_en
village_status_meta2024b <- village_status_meta2024
village_status_meta2024b$site2 <- village_status_meta2024b$village_en
village_status_meta2024b$status2 <- village_status_meta2024b$status

# Transforming into long format and adding different dataframes containing information about land use and village pairs
jaccard_plants_mountain <- jaccard_plants %>%
  gather(site2, jaccard, 1:30) %>%       #long format
  left_join(village_status_meta2024, join_by("site1")) %>%         # adding status for site 1 
  #rename(site2 = site2.x) %>% 
  left_join(village_status_meta2024b, join_by("site2")) %>%  #adding status for site2
  dplyr::select(site1.x, site2, jaccard, pair.x, pair.y, 
                region.x, region.y, status.x, status2) %>%
  rename(site1 = site1.x,
         pair1 = pair.x,
         region1 = region.x, region2 = region.y,
         status1 = status.x) %>%
  mutate(test = site1 == site2,
         comparison = paste(status1, status2, sep = "_")) %>%  #creating new column that shows status comparison
  filter(test == FALSE,
         region1 == "Mountain",
         region2 == "Mountain") %>% 
  droplevels()%>%
  ungroup()

# In the mountains the comparisons are:

# stable - decreasing
# stable - stable
# stable - abandoned
# abandoned - decreasing
# abandoned - abandoned
# decreasing - decreasing

unique(jaccard_plants_mountain$comparison)

jaccard_plants_mountain <- jaccard_plants_mountain %>%
  filter(!comparison %in% c("stable_abandoned", "stable_decreasing",
                            "decreasing_abandoned"))

# Plotting 
(jaccard_plants_mountain_plot <- ggplot() +
    geom_density_ridges(data = jaccard_plants_mountain,
                        aes(x = jaccard, y = comparison,
                            fill = comparison, 
                            colour = comparison), alpha = 0.5,
                        quantile_lines=TRUE,
                        quantile_fun=function(x,...)mean(x)) +
    change_theme() +
    theme(legend.position = "n") +
    labs(title = "B   Plants in the mountains\n"))

#saving plot
ggsave(jaccard_plants_mountain_plot, file = "figures/plant_status_comparison_jaccard_mountain.png",
       height = 5,
       width = 7)

# Transforming into long format and adding different dataframes containing information about land use and village pairs
jaccard_plants_lowland <- jaccard_plants %>%
  gather(site2, jaccard, 1:30) %>%       #long format
  left_join(village_status_meta2024, join_by("site1")) %>%         # adding status for site 1 
  #rename(site2 = site2.x) %>% 
  left_join(village_status_meta2024b, join_by("site2")) %>%  #adding status for site2
  dplyr::select(site1.x, site2, jaccard, pair.x, pair.y, 
                region.x, region.y, status.x, status2) %>%
  rename(site1 = site1.x,
         pair1 = pair.x,
         region1 = region.x, region2 = region.y,
         status1 = status.x) %>%
  mutate(test = site1 == site2,
         comparison = paste(status1, status2, sep = "_")) %>%  #creating new column that shows status comparison
  filter(test == FALSE,
         region1 == "Lowland",
         region2 == "Lowland") %>% 
  droplevels()%>%
  ungroup()

# In the lowlands the comparisons are:

# increasing - decreasing
# increasing - increasing
# decreasing - decreasing

unique(jaccard_plants_lowland$comparison)

jaccard_plants_lowland <- jaccard_plants_lowland %>%
  filter(comparison != "increasing_decreasing")

# Plotting 
(jaccard_plants_lowland_plot <- ggplot() +
    geom_density_ridges(data = jaccard_plants_lowland,
                        aes(x = jaccard, y = comparison,
                            fill = comparison, 
                            colour = comparison), alpha = 0.5,
                        quantile_lines=TRUE,
                        quantile_fun=function(x,...)mean(x)) +
    change_theme() +
    theme(legend.position = "n") +
    labs(title = "A   Plants in the lowlands\n"))

#saving plot
ggsave(jaccard_plants_lowland_plot, file = "figures/bird_status_comparison_jaccard_lowland.png",
       height = 5,
       width = 7)

#### Amphibian----

#calculating jaccard index for the villages
jaccard_amphibian <- as.data.frame(as.matrix(vegdist(amphibia, method = "jaccard"))) 

# adding column with villagename
jaccard_amphibian$site1 <- rownames(jaccard_amphibian)

village_status_meta2023$site1 <- village_status_meta2023$village_en
village_status_meta2023b <- village_status_meta2023
village_status_meta2023b$site2 <- village_status_meta2023b$village_en
village_status_meta2023b$status2 <- village_status_meta2023b$status

# Transforming into long format and adding different dataframes containing information about land use and village pairs
jaccard_amphibian_mountain <- jaccard_amphibian %>%
  gather(site2, jaccard, 1:29) %>%       #long format
  left_join(village_status_meta2023, join_by("site1")) %>%         # adding status for site 1 
  #rename(site2 = site2.x) %>% 
  left_join(village_status_meta2023b, join_by("site2")) %>%  #adding status for site2
  dplyr::select(site1.x, site2, jaccard, pair.x, pair.y, 
                region.x, region.y, status.x, status2) %>%
  rename(site1 = site1.x,
         pair1 = pair.x,
         region1 = region.x, region2 = region.y,
         status1 = status.x) %>%
  mutate(test = site1 == site2,
         comparison = paste(status1, status2, sep = "_")) %>%  #creating new column that shows status comparison
  filter(test == FALSE,
         region1 == "Mountain",
         region2 == "Mountain") %>% 
  droplevels()%>%
  ungroup()

# In the mountains the comparisons are:

# stable - decreasing
# stable - stable
# stable - abandoned
# abandoned - decreasing
# abandoned - abandoned
# decreasing - decreasing

unique(jaccard_amphibian_mountain$comparison)

jaccard_amphibian_mountain <- jaccard_amphibian_mountain %>%
  filter(!comparison %in% c("stable_abandoned", "stable_decreasing",
                            "decreasing_abandoned"))

# Plotting 
(jaccard_amphibian_mountain_plot <- ggplot() +
    geom_density_ridges(data = jaccard_amphibian_mountain,
                        aes(x = jaccard, y = comparison,
                            fill = comparison, 
                            colour = comparison), alpha = 0.5,
                        quantile_lines=TRUE,
                        quantile_fun=function(x,...)mean(x)) +
    change_theme() +
    theme(legend.position = "n") +
    labs(title = "D   Amphibians in the mountains\n"))

#saving plot
ggsave(jaccard_amphibian_mountain_plot, file = "figures/amphibia_status_comparison_jaccard_mountain.png",
       height = 5,
       width = 7)

# Transforming into long format and adding different dataframes containing information about land use and village pairs
jaccard_amphibian_lowland <- jaccard_amphibian %>%
  gather(site2, jaccard, 1:29) %>%       #long format
  left_join(village_status_meta2023, join_by("site1")) %>%         # adding status for site 1 
  #rename(site2 = site2.x) %>% 
  left_join(village_status_meta2023b, join_by("site2")) %>%  #adding status for site2
  dplyr::select(site1.x, site2, jaccard, pair.x, pair.y, 
                region.x, region.y, status.x, status2) %>%
  rename(site1 = site1.x,
         pair1 = pair.x,
         region1 = region.x, region2 = region.y,
         status1 = status.x) %>%
  mutate(test = site1 == site2,
         comparison = paste(status1, status2, sep = "_")) %>%  #creating new column that shows status comparison
  filter(test == FALSE,
         region1 == "Lowland",
         region2 == "Lowland") %>% 
  droplevels()%>%
  ungroup()

# In the lowlands the comparisons are:

# increasing - decreasing
# increasing - increasing
# decreasing - decreasing

unique(jaccard_amphibian_lowland$comparison)

jaccard_amphibian_lowland <- jaccard_amphibian_lowland %>%
  filter(comparison != "increasing_decreasing")

# Plotting 
(jaccard_amphibian_lowland_plot <- ggplot() +
    geom_density_ridges(data = jaccard_amphibian_lowland,
                        aes(x = jaccard, y = comparison,
                            fill = comparison, 
                            colour = comparison), alpha = 0.5,
                        quantile_lines=TRUE,
                        quantile_fun=function(x,...)mean(x)) +
    change_theme() +
    theme(legend.position = "n") +
    labs(title = "C   Amphibians in the lowlands\n"))

#saving plot
ggsave(jaccard_amphibian_lowland_plot, file = "figures/amphibian_status_comparison_jaccard_lowland.png",
       height = 5,
       width = 7)

#### Odonata----

# calculating jaccard index for the villages
jaccard_odonata <- as.data.frame(as.matrix(vegdist(odonata, method = "jaccard"))) 

# adding column with villagename
jaccard_odonata$site1 <- rownames(jaccard_odonata)

village_status_meta2023$site1 <- village_status_meta2023$village_en
village_status_meta2023b <- village_status_meta2023
village_status_meta2023b$site2 <- village_status_meta2023b$village_en
village_status_meta2023b$status2 <- village_status_meta2023b$status

# Transforming into long format and adding different dataframes containing information about land use and village pairs
jaccard_odonata_mountain <- jaccard_odonata %>%
  gather(site2, jaccard, 1:28) %>%       #long format
  left_join(village_status_meta2023, join_by("site1")) %>%         # adding status for site 1 
  #rename(site2 = site2.x) %>% 
  left_join(village_status_meta2023b, join_by("site2")) %>%  #adding status for site2
  dplyr::select(site1.x, site2, jaccard, pair.x, pair.y, 
                region.x, region.y, status.x, status2) %>%
  rename(site1 = site1.x,
         pair1 = pair.x,
         region1 = region.x, region2 = region.y,
         status1 = status.x) %>%
  mutate(test = site1 == site2,
         comparison = paste(status1, status2, sep = "_")) %>%  #creating new column that shows status comparison
  filter(test == FALSE,
         region1 == "Mountain",
         region2 == "Mountain") %>% 
  droplevels()%>%
  ungroup()

# In the mountains the comparisons are:

# stable - decreasing
# stable - stable
# stable - abandoned
# abandoned - decreasing
# abandoned - abandoned
# decreasing - decreasing

unique(jaccard_odonata_mountain$comparison)

jaccard_odonata_mountain <- jaccard_odonata_mountain %>%
  filter(!comparison %in% c("stable_abandoned", "stable_decreasing",
                            "decreasing_abandoned"))

# Plotting 
(jaccard_odonata_mountain_plot <- ggplot() +
    geom_density_ridges(data = jaccard_odonata_mountain,
                        aes(x = jaccard, y = comparison,
                            fill = comparison, 
                            colour = comparison), alpha = 0.5,
                        quantile_lines=TRUE,
                        quantile_fun=function(x,...)mean(x)) +
    change_theme() +
    theme(legend.position = "n") +
    labs(title = "D   Odonata in the mountains\n"))

#saving plot
ggsave(jaccard_odonata_mountain_plot, file = "figures/odonata_status_comparison_jaccard_mountain.png",
       height = 5,
       width = 7)

# Transforming into long format and adding different dataframes containing information about land use and village pairs
jaccard_odonata_lowland <- jaccard_odonata %>%
  gather(site2, jaccard, 1:28) %>%       #long format
  left_join(village_status_meta2023, join_by("site1")) %>%         # adding status for site 1 
  #rename(site2 = site2.x) %>% 
  left_join(village_status_meta2023b, join_by("site2")) %>%  #adding status for site2
  dplyr::select(site1.x, site2, jaccard, pair.x, pair.y, 
                region.x, region.y, status.x, status2) %>%
  rename(site1 = site1.x,
         pair1 = pair.x,
         region1 = region.x, region2 = region.y,
         status1 = status.x) %>%
  mutate(test = site1 == site2,
         comparison = paste(status1, status2, sep = "_")) %>%  #creating new column that shows status comparison
  filter(test == FALSE,
         region1 == "Lowland",
         region2 == "Lowland") %>% 
  droplevels()%>%
  ungroup()

# In the lowlands the comparisons are:

# increasing - decreasing
# increasing - increasing
# decreasing - decreasing

unique(jaccard_odonata_lowland$comparison)

jaccard_odonata_lowland <- jaccard_odonata_lowland %>%
  filter(comparison != "increasing_decreasing")

# Plotting 
(jaccard_odonata_lowland_plot <- ggplot() +
    geom_density_ridges(data = jaccard_odonata_lowland,
                        aes(x = jaccard, y = comparison,
                            fill = comparison, 
                            colour = comparison), alpha = 0.5,
                        quantile_lines=TRUE,
                        quantile_fun=function(x,...)mean(x)) +
    change_theme() +
    theme(legend.position = "n") +
    labs(title = "D   Odonata in the lowlands\n"))

#saving plot
ggsave(jaccard_odonata_lowland_plot, file = "figures/odonata_status_comparison_jaccard_lowland.png",
       height = 5,
       width = 7)


# Time since aband + layers ----
(aband_years_layers <- ggplot(layers_simple[layers_simple$status == "abandoned",], (aes(x = years_since_aband, y = sum_prop_cover,
                                                                                        colour = layer_simple,
                                                                                        fill = layer_simple))) +
   geom_point(alpha = 0.5) +
   geom_smooth(method = "lm") +
   change_theme() +
   facet_wrap(~layer_simple) +
   scale_fill_manual(values = c("#a81353", "#e69b10", "#3f996c")) +
   scale_colour_manual(values = c("#a81353", "#e69b10", "#3f996c")) +
   guides(fill = F, colour = F) +
   labs(x = "\nYears since abandonment", y = "Vegetation cover (%)\n") +
   theme(strip.background = element_blank(),
         panel.border = element_rect(colour = "black", size = 0.5, fill = NA), 
         axis.line.x = element_blank(),
         axis.line.y = element_blank(),
         strip.text.x = element_text(size = 15,
                                     color = "grey35", face = "bold")))

ggsave(aband_years_layers, filename = "figures/years_since_aband_layers.png",
       height = 5, width = 12)

# Threatened status ----
village_pairs_status2023 <- read_csv("data/village_pairs_status.csv")

bulgaria_protection_status <- read.csv("data/bulgarian_protection_status.csv") # data available at http://e-ecodb.bas.bg/rdb/en/vol2/texts.html
bird_species_names <- read.csv("data/bird_species_bg.csv")

bird_species_names <- bird_species_names %>%
  dplyr::select(unique.bird_abundance.species., Latin_name, Common_Name) %>%
  rename(species = unique.bird_abundance.species.,
         Latin.name = Latin_name)

colnames(bird_species_names)
colnames(bird_abundance)

# Birds

bird_abundance_status <- left_join(bird_abundance, 
                                   bird_species_names, by = "species")

colnames(bulgaria_protection_status)

bird_abundance_status <- left_join(bird_abundance_status,
                                   bulgaria_protection_status, by = "Latin.name")
village_status_meta <- village_status_meta2023 %>%
  dplyr::select(village_en, status)

bird_threat_tally <- bird_abundance_status %>%
  dplyr::select(village, village_en, species, region, Latin.name, Common_Name,
                Degree.of.threat) %>% distinct() %>%
  ungroup() %>%
  left_join(village_status_meta, by = "village_en")

# Pheasants are definitely not extinct, adjust status
# classification because genetically the wild species has bred too much
# with non-wild pheasants

bird_threat_tally <- bird_threat_tally %>%
  mutate(ScientificName = case_when(Latin.name == "Dendrocopus medius" ~ "Dendrocoptes medius", .default = Latin.name),
       Degree.of.threat = case_when(Latin.name =="Dendrocoptes medius" ~ NA,
                                       Latin.name == "Phasianus colchicus" ~ NA, # manually adding those species that are not in the df, by looking them up online
                                       Latin.name == "Columba livia domestica" ~ NA, .default = Degree.of.threat))


bird_threat_tally_simple <- bird_threat_tally %>%
  group_by(region, status, Degree.of.threat) %>% tally()

# Odonata, amphibia
load("data/od_am_rept_2025.RData")

name_codes <- all_taxa_abundance %>% ungroup() %>%
  dplyr::select(village_en, village) %>% distinct()

od_am_rept2 <- left_join(od_am_rept, name_codes, by = "village")
od_am_rept2 <- od_am_rept2 %>%
  rename(village_en = village_en.y)

od_am_rept2 <- left_join(od_am_rept2, village_status_meta)
colnames(od_am_rept2)
od_am_rept2 <- od_am_rept2 %>%
  rename(Latin.name = species)

od_am_rept2 <- left_join(od_am_rept2, bulgaria_protection_status)

odonata_species_richness_tally <- od_am_rept2 %>%
  filter(taxa == "Odonata",
         Latin.name != "няма регистрации",
         Latin.name != "Calopteryx sp.",
         Latin.name != "Libellula sp.",
         Latin.name != "Orthetrum sp.",
         Latin.name != "Sympetrum sp.") %>%
  group_by(region, status, Degree.of.threat) %>% 
  summarise(richness = length(unique(Latin.name)))

amphibia_species_richness_tally <- od_am_rept2 %>%
  filter(taxa == "Amphibia",
         Latin.name != "няма регистрации",
         Latin.name != "Epidalea sp.") %>%
  group_by(region, status, Degree.of.threat) %>% 
  summarise(richness = length(unique(Latin.name)))

# Plants
plant_bulgarian_status <- read.csv("data/plant_bulgarian_status.csv")
colnames(plant_bulgarian_status)

# Counting per village type

# plants

village_meta2024 <- village_pairs_status2024 %>%
  dplyr::select(village_en, region, status)

plants_status <- left_join(plants2024, village_meta2024)
colnames(plants_status)

plants_status <- plants_status %>%
  rename(Latin.name = species2)

plants_status <- left_join(plants_status, plant_bulgarian_status)

# Silene vulgaris is actually the subspecies that's threatened
# same for the capsella
# Aesculus hippocastanum is likely planted by humans
# same for Taxus baccata

# Verbascum juruk not true

# genuine threatened species recorded within the villages
# Himantoglossum caprinum
# in decreasing and in stable mountain villages

# Cardamine parviflora unlikely
plants_status <- plants_status %>% filter(Latin.name != "Cardamine parviflora",
                                          Latin.name != "Verbascum juruk")

plants_status_tally <- plants_status %>% ungroup() %>%
  dplyr::select(village_en, region, status, Latin.name) %>% distinct() %>%
  group_by(region, status) %>%
  summarise(richness_sum = length(unique(Latin.name)))

# Make a table of the top most abundant bird and plant species in each village category
bird_abundance <- read_excel("data/bird_abundance.xlsx")

# add village statuses

bird_abundance <- left_join(bird_abundance, village_pairs_status2023)

bird_abundance <- bird_abundance %>%
  filter(abundance != "80",
         abundance != "X")#remove the 80 pidgeons
  

bird_abundance_tally <- bird_abundance %>%
  ungroup() %>%
  group_by(species, region, status) %>%
  summarise(abundance = sum(as.numeric(as.character(abundance)), na.rm = T))

bird_abundance_tally_wide <- bird_abundance_tally %>%
  pivot_wider(names_from = status, values_from = abundance)

# Rubus ----
rubus <- plants2024_raw %>%
  filter(species2 %in% c("Rubus thyrsanthus",
                         "Rubus idaeus",
                         "Rubus hirtus", 
                         "Rubus discolor",
                         "Rubus canescens",
                         "Rubus caesius",
                         "Rubus")) %>% ungroup() %>%
  group_by(village_en, site) %>%
  summarise(rubus_cover = sum(cover_final)) %>% ungroup()

# add other variables

richness_demo_land$site_n <- parse_number(richness_demo_land$site)
richness_demo_land$site2 <- paste(richness_demo_land$village_en,
                                  richness_demo_land$site_n, sep = "_")

colnames(rubus)[2] <- "site2"

richness_demo_land_rubus <- left_join(richness_demo_land, rubus,
                                      by = "site2")
rubus_mountain_birds_plants <- richness_demo_land_rubus %>%
  filter(region == "Mountain",
         taxa %in% c("Birds", "Plants"))


ggplot(rubus_mountain_birds_plants, aes(x = rubus_cover, y = sp_richness)) +
  geom_point(colour = "#8cbca4") +
  geom_smooth(method = "lm", colour = "#8cbca4", fill = "#8cbca4") +
  facet_wrap(~taxa, scales = "free") +
  change_theme()

# running the model
prior4 <- c(set_prior(prior = 'normal(0,6)', class='b', coef="rubus_cover"), 	# global slope,
            set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 	# global intercept
            set_prior(prior = 'cauchy(0,2)', class='sd'))	# group-level intercepts

rubus_mountain_birds <- rubus_mountain_birds_plants %>% filter(taxa == "Birds")

bird_richness_rubus_model <- brm(bf(sp_richness ~ rubus_cover +
                                (1|village)),                                      # village as random (grouping factor)
                           data = rubus_mountain_birds, family = poisson(),
                           prior = prior4, iter = 10000,
                           warmup = 2000,
                           init = '0',
                           cores = 4, chains = 4)

summary(bird_plant_richness_rubus_model)

# saving the model
save(bird_richness_rubus_model, file="data/models/bird_richness_rubus_model.RData")

#plotting predictions
(birds_richness_rubus_plot <- rubus_mountain_birds_plants %>%
    modelr::data_grid(rubus_cover = seq_range(rubus_cover, n = 101)) %>%
    add_epred_draws(bird_richness_rubus_model, re_formula = NULL, allow_new_levels = TRUE) %>% #adding predicted draws from the model
    ggplot(aes(x = rubus_cover, y = sp_richness)) +
    tidybayes::stat_lineribbon(aes(y = .epred),           # adding confidence intervals
                               .width = c(0.95), alpha = 0.3, linewidth = 1.5,
                               colour = "#8cbca4", fill = "#8cbca4") +
    tidybayes::stat_lineribbon(aes(y = .epred) ,
                               .width = c(0), linewidth = 1.5,
                               colour = "#8cbca4", fill = "#8cbca4") +
    geom_point(data = rubus_mountain_birds, alpha = 0.8, colour = "#8cbca4") +   # adding raw data
    change_theme() +
    theme(legend.position = element_blank(),
          plot.title = element_text(face = "bold",                              # adding legend 
                                    hjust = -0.17)) +
    labs(x = "\nRubus cover\n(% of vegetation)",            # labels
         y = "Bird species richness\n",
         title = "A\n"))

rubus_mountain_plants <- rubus_mountain_birds_plants %>% filter(taxa == "Plants")

plant_richness_rubus_model <- brm(bf(sp_richness ~ rubus_cover +
                                      (1|village)),                                      # village as random (grouping factor)
                                 data = rubus_mountain_plants, family = poisson(),
                                 prior = prior4, iter = 10000,
                                 warmup = 2000,
                                 init = '0',
                                 cores = 4, chains = 4)

summary(plant_richness_rubus_model)

# saving the model
save(plant_richness_rubus_model, file="data/models/plant_richness_rubus_model.RData")

#plotting predictions
(plants_richness_rubus_plot <- rubus_mountain_plants %>%
    modelr::data_grid(rubus_cover = seq_range(rubus_cover, n = 101)) %>%
    add_epred_draws(plant_richness_rubus_model, re_formula = NULL, allow_new_levels = TRUE) %>% #adding predicted draws from the model
    ggplot(aes(x = rubus_cover, y = sp_richness)) +
    tidybayes::stat_lineribbon(aes(y = .epred),           # adding confidence intervals
                               .width = c(0.95), alpha = 0.3, linewidth = 1.5,
                               colour = "#8cbca4", fill = "#8cbca4") +
    tidybayes::stat_lineribbon(aes(y = .epred) ,
                               .width = c(0), linewidth = 1.5,
                               colour = "#8cbca4", fill = "#8cbca4") +
    geom_point(data = rubus_mountain_plants, alpha = 0.8, colour = "#8cbca4") +   # adding raw data
    change_theme() +
    theme(legend.position = element_blank(),
          plot.title = element_text(face = "bold",                              # adding legend 
                                    hjust = -0.17)) +
    labs(x = "\nRubus cover\n(% of vegetation)",            # labels
         y = "Plant species richness\n",
         title = "B\n"))

rubus_panel <- grid.arrange(birds_plants_richness_rubus_plot,
                            plants_richness_rubus_plot, ncol = 2)

ggsave(rubus_panel, file = "figures/rubus_panel.png", height = 5, width = 10)

ggplot(rubus_mountain_birds_plants, aes(x = human_pop_trend_2024, y = rubus_cover)) +
  geom_point(colour = "#8cbca4") +
  geom_smooth(method = "lm", colour = "#8cbca4", fill = "#8cbca4") +
  change_theme()


human_pop_rubus_model <- brm(bf(rubus_cover ~ human_pop_trend_2024 +
                                       (1|village)),                                      # village as random (grouping factor)
                                  data = rubus_mountain_plants, family = gaussian(),
                                  prior = prior3, iter = 10000,
                                  warmup = 2000,
                                  init = '0',
                                  cores = 4, chains = 4)

summary(human_pop_rubus_model)

# saving the model
save(human_pop_rubus_model, file="data/models/human_pop_rubus_model.RData")

#plotting predictions
(human_pop_rubus_plot <- rubus_mountain_plants %>%
    modelr::data_grid(human_pop_trend_2024 = seq_range(human_pop_trend_2024, n = 101)) %>%
    add_epred_draws(human_pop_rubus_model, re_formula = NULL, allow_new_levels = TRUE) %>% #adding predicted draws from the model
    ggplot(aes(x = human_pop_trend_2024, y = rubus_cover)) +
    tidybayes::stat_lineribbon(aes(y = .epred),           # adding confidence intervals
                               .width = c(0.95), alpha = 0.3, linewidth = 1.5,
                               colour = "#8cbca4", fill = "#8cbca4") +
    tidybayes::stat_lineribbon(aes(y = .epred) ,
                               .width = c(0), linewidth = 1.5,
                               colour = "#8cbca4", fill = "#8cbca4") +
    geom_point(data = rubus_mountain_plants, alpha = 0.8, colour = "#8cbca4") +   # adding raw data
    change_theme() +
    theme(legend.position = element_blank(),
          plot.title = element_text(face = "bold",                              # adding legend 
                                    hjust = -0.17)) +
    labs(x = "\nHuman population trend\n(slope of change per year)",            # labels
         y = "Rubus cover\n(% of vegetation)\n",
         title = "B\n") +
    scale_x_continuous(limits = c(-1.3, 0.3),
                       breaks = c(-1.2, -0.9, -0.6, -0.3, 0, 0.3),
                       labels = c("-1.2", "-0.9", "-0.6", "-0.3", "0", "0.3")))

ggsave(human_pop_rubus_plot, file = "figures/rubus_human_pop.png", height = 5, width = 5)

# Shrub and tree cover and the sp richness of birds and plants ----

# Combine plants2024_cover and bird_species_richness
bird_species_richness_simple <- bird_species_richness %>%
  dplyr::select(site2, sp_richness)

colnames(bird_species_richness_simple) <- c("site", "bird_richness")

plants_birds_combo <- left_join(plants2024_cover, bird_species_richness_simple,
                                by = "site")

plants_birds_combo <- plants_birds_combo %>%
  pivot_wider(names_from = layer2, values_from = layer_cover)

# tree cover
## Setting priors 
hier_prior_trees <- c(set_prior(prior = 'normal(0,6)', class='b', coef="Trees"), 	# global slope, 
                      set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 	# global intercept
                      set_prior(prior = 'cauchy(0,2)', class='sd'))	# group-level intercepts

hier_prior_shrubs <- c(set_prior(prior = 'normal(0,6)', class='b', coef="Shrubs"), 	# global slope, 
                      set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 	# global intercept
                      set_prior(prior = 'cauchy(0,2)', class='sd'))	# group-level intercepts

hier_prior_trees_shrubs <- c(set_prior(prior = 'normal(0,6)', class='b', coef="Trees_Shrubs"), 	# global slope, 
                       set_prior(prior = 'normal(0,6)', class='Intercept', coef=''), 	# global intercept
                       set_prior(prior = 'cauchy(0,2)', class='sd'))	# group-level intercepts

tree_cover_bird_sp_richness <- brm(bf(bird_richness ~ Trees*region +
                                      (1|village_en)),      
                                 data = plants_birds_combo, family = poisson(),
                                 prior = hier_prior_trees, iter = 10000,
                                 warmup = 2000,
                                 init = '0',
                                 cores = 4, chains = 4)

summary(tree_cover_bird_sp_richness)

save(tree_cover_bird_sp_richness, file="data/models/tree_cover_bird_richness.RData")

shrub_cover_bird_sp_richness <- brm(bf(bird_richness ~ Shrubs*region +
                                        (1|village_en)),      
                                   data = plants_birds_combo, family = poisson(),
                                   prior = hier_prior_shrubs, iter = 10000,
                                   warmup = 2000,
                                   init = '0',
                                   cores = 4, chains = 4)

summary(shrub_cover_bird_sp_richness)

save(shrub_cover_bird_sp_richness, file="data/models/shrub_cover_bird_richness.RData")

# Trees and shrubs combined
plants_birds_combo$Trees_Shrubs <- plants_birds_combo$Trees + plants_birds_combo$Shrubs

tree_shrub_cover_bird_sp_richness <- brm(bf(bird_richness ~ Trees_Shrubs*region +
                                         (1|village_en)),      
                                    data = plants_birds_combo, family = poisson(),
                                    prior = hier_prior_trees_shrubs, iter = 10000,
                                    warmup = 2000,
                                    init = '0',
                                    cores = 4, chains = 4)

summary(tree_shrub_cover_bird_sp_richness)

save(tree_shrub_cover_bird_sp_richness, file="data/models/tree_shrub_cover_bird_richness.RData")

(tree_cover_bird_richness_plot <- plants_birds_combo %>% 
  group_by(region) %>%
  modelr::data_grid(Trees = seq_range(Trees, n = 101)) %>%
  add_epred_draws(tree_cover_bird_sp_richness, re_formula = NULL, allow_new_levels = TRUE) %>%
  ggplot( aes(y = .epred, x = Trees, color = region,        
              fill = region)) +
  tidybayes::stat_lineribbon(aes(y = .epred, fill = region, colour = region),   
                             .width = c(0.95), alpha = 0.3, linewidth = 1.5) +
  tidybayes::stat_lineribbon(aes(y = .epred, fill = region, colour = region) ,
                             .width = c(0), linewidth = 1.5) +
  geom_point(data = plants_birds_combo, aes(y = bird_richness, x = Trees), alpha = 0.8) +        
  scale_color_manual(values = c("#df7a5f", "#8cbca4")) +
  scale_fill_manual(values = c("#df7a5f", "#8cbca4")) +
  change_theme() +
  theme(plot.title = element_text(face = "bold",         
                                  hjust = -0.17)) +
  labs(x = "\nTree cover\n(% of vegetation)",
       y = "Bird species richness\n",
       title = "A\n") +
  scale_x_continuous(limits = c(0, 100),
                     breaks = c(0, 25, 50, 75, 100)))

(shrub_cover_bird_richness_plot <- plants_birds_combo %>% 
    group_by(region) %>%
    modelr::data_grid(Shrubs = seq_range(Shrubs, n = 101)) %>%
    add_epred_draws(shrub_cover_bird_sp_richness, re_formula = NULL, allow_new_levels = TRUE) %>%
    ggplot( aes(y = .epred, x = Shrubs, color = region,        
                fill = region)) +
    tidybayes::stat_lineribbon(aes(y = .epred, fill = region, colour = region),   
                               .width = c(0.95), alpha = 0.3, linewidth = 1.5) +
    tidybayes::stat_lineribbon(aes(y = .epred, fill = region, colour = region) ,
                               .width = c(0), linewidth = 1.5) +
    geom_point(data = plants_birds_combo, aes(y = bird_richness, x = Shrubs), alpha = 0.8) +        
    scale_color_manual(values = c("#df7a5f", "#8cbca4")) +
    scale_fill_manual(values = c("#df7a5f", "#8cbca4")) +
    change_theme() +
    theme(plot.title = element_text(face = "bold",         
                                    hjust = -0.17)) +
    labs(x = "\nShrub cover\n(% of vegetation)",
         y = "Bird species richness\n",
         title = "B\n") +
    scale_x_continuous(limits = c(0, 100),
                       breaks = c(0, 25, 50, 75, 100)))

(tree_shrub_cover_bird_richness_plot <- plants_birds_combo %>% 
    group_by(region) %>%
    modelr::data_grid(Trees_Shrubs = seq_range(Trees_Shrubs, n = 101)) %>%
    add_epred_draws(tree_shrub_cover_bird_sp_richness, re_formula = NULL, allow_new_levels = TRUE) %>%
    ggplot( aes(y = .epred, x = Trees_Shrubs, color = region,        
                fill = region)) +
    tidybayes::stat_lineribbon(aes(y = .epred, fill = region, colour = region),   
                               .width = c(0.95), alpha = 0.3, linewidth = 1.5) +
    tidybayes::stat_lineribbon(aes(y = .epred, fill = region, colour = region) ,
                               .width = c(0), linewidth = 1.5) +
    geom_point(data = plants_birds_combo, aes(y = bird_richness, x = Trees_Shrubs), alpha = 0.8) +        
    scale_color_manual(values = c("#df7a5f", "#8cbca4")) +
    scale_fill_manual(values = c("#df7a5f", "#8cbca4")) +
    change_theme() +
    theme(plot.title = element_text(face = "bold",         
                                    hjust = -0.17)) +
    labs(x = "\nTree and shrub cover\n(% of vegetation)",
         y = "Bird species richness\n",
         title = "C\n") +
    scale_x_continuous(limits = c(0, 100),
                       breaks = c(0, 25, 50, 75, 100)))

birds_shrubs_trees_panel <- grid.arrange(tree_cover_bird_richness_plot,
                                         shrub_cover_bird_richness_plot,
                                         tree_shrub_cover_bird_richness_plot, ncol = 3)

ggsave(birds_shrubs_trees_panel, file = "figures/birds_shrubs_trees_panel.png",
       height = 5, width = 15)

# Sp richness of forbs and grasses
unique(plants2024_raw$layer)

herbal <- plants2024_raw %>%
  filter(layer %in% c("h", "hh")) %>%
  ungroup() %>%
  group_by(site) %>%
  summarise(herb_richness = length(unique(species2)))

herbs_combo <- left_join(plants2024_cover, herbal,
                                by = "site")

herbs_combo <- herbs_combo %>%
  pivot_wider(names_from = layer2, values_from = layer_cover)

# tree cover
tree_cover_herb_sp_richness <- brm(bf(herb_richness ~ Trees*region +
                                        (1|village_en)),      
                                   data = herbs_combo, family = poisson(),
                                   prior = hier_prior_trees, iter = 10000,
                                   warmup = 2000,
                                   init = '0',
                                   cores = 4, chains = 4)

summary(tree_cover_herb_sp_richness)

save(tree_cover_herb_sp_richness, file="data/models/tree_cover_herb_richness.RData")

shrub_cover_herb_sp_richness <- brm(bf(herb_richness ~ Shrubs*region +
                                         (1|village_en)),      
                                    data = herbs_combo, family = poisson(),
                                    prior = hier_prior_shrubs, iter = 10000,
                                    warmup = 2000,
                                    init = '0',
                                    cores = 4, chains = 4)

summary(shrub_cover_herb_sp_richness)

save(shrub_cover_herb_sp_richness, file="data/models/shrub_cover_herb_richness.RData")

# Trees and shrubs combined
herbs_combo$Trees_Shrubs <- herbs_combo$Trees + herbs_combo$Shrubs

tree_shrub_cover_herb_sp_richness <- brm(bf(herb_richness ~ Trees_Shrubs*region +
                                              (1|village_en)),      
                                         data = herbs_combo, family = poisson(),
                                         prior = hier_prior_trees_shrubs, iter = 10000,
                                         warmup = 2000,
                                         init = '0',
                                         cores = 4, chains = 4)

summary(tree_shrub_cover_herb_sp_richness)

save(tree_shrub_cover_herb_sp_richness, file="data/models/tree_shrub_cover_herb_richness.RData")

(tree_cover_herb_richness_plot <- herbs_combo %>% 
    group_by(region) %>%
    modelr::data_grid(Trees = seq_range(Trees, n = 101)) %>%
    add_epred_draws(tree_cover_herb_sp_richness, re_formula = NULL, allow_new_levels = TRUE) %>%
    ggplot( aes(y = .epred, x = Trees, color = region,        
                fill = region)) +
    tidybayes::stat_lineribbon(aes(y = .epred, fill = region, colour = region),   
                               .width = c(0.95), alpha = 0.3, linewidth = 1.5) +
    tidybayes::stat_lineribbon(aes(y = .epred, fill = region, colour = region) ,
                               .width = c(0), linewidth = 1.5) +
    geom_point(data = herbs_combo, aes(y = herb_richness, x = Trees), alpha = 0.8) +        
    scale_color_manual(values = c("#df7a5f", "#8cbca4")) +
    scale_fill_manual(values = c("#df7a5f", "#8cbca4")) +
    change_theme() +
    theme(plot.title = element_text(face = "bold",         
                                    hjust = -0.17)) +
    labs(x = "\nTree cover\n(% of vegetation)",
         y = "Grasses and forbs species richness\n",
         title = "A\n") +
    scale_x_continuous(limits = c(0, 100),
                       breaks = c(0, 25, 50, 75, 100)))

(shrub_cover_herb_richness_plot <- herbs_combo %>% 
    group_by(region) %>%
    modelr::data_grid(Shrubs = seq_range(Shrubs, n = 101)) %>%
    add_epred_draws(shrub_cover_herb_sp_richness, re_formula = NULL, allow_new_levels = TRUE) %>%
    ggplot( aes(y = .epred, x = Shrubs, color = region,        
                fill = region)) +
    tidybayes::stat_lineribbon(aes(y = .epred, fill = region, colour = region),   
                               .width = c(0.95), alpha = 0.3, linewidth = 1.5) +
    tidybayes::stat_lineribbon(aes(y = .epred, fill = region, colour = region) ,
                               .width = c(0), linewidth = 1.5) +
    geom_point(data = herbs_combo, aes(y = herb_richness, x = Shrubs), alpha = 0.8) +        
    scale_color_manual(values = c("#df7a5f", "#8cbca4")) +
    scale_fill_manual(values = c("#df7a5f", "#8cbca4")) +
    change_theme() +
    theme(plot.title = element_text(face = "bold",         
                                    hjust = -0.17)) +
    labs(x = "\nShrub cover\n(% of vegetation)",
         y = "Grasses and forbs species richness\n",
         title = "B\n") +
    scale_x_continuous(limits = c(0, 100),
                       breaks = c(0, 25, 50, 75, 100)))

(tree_shrub_cover_herb_richness_plot <- herbs_combo %>% 
    group_by(region) %>%
    modelr::data_grid(Trees_Shrubs = seq_range(Trees_Shrubs, n = 101)) %>%
    add_epred_draws(tree_shrub_cover_herb_sp_richness, re_formula = NULL, allow_new_levels = TRUE) %>%
    ggplot( aes(y = .epred, x = Trees_Shrubs, color = region,        
                fill = region)) +
    tidybayes::stat_lineribbon(aes(y = .epred, fill = region, colour = region),   
                               .width = c(0.95), alpha = 0.3, linewidth = 1.5) +
    tidybayes::stat_lineribbon(aes(y = .epred, fill = region, colour = region) ,
                               .width = c(0), linewidth = 1.5) +
    geom_point(data = herbs_combo, aes(y = herb_richness, x = Trees_Shrubs), alpha = 0.8) +        
    scale_color_manual(values = c("#df7a5f", "#8cbca4")) +
    scale_fill_manual(values = c("#df7a5f", "#8cbca4")) +
    change_theme() +
    theme(plot.title = element_text(face = "bold",         
                                    hjust = -0.17)) +
    labs(x = "\nTree and shrub cover\n(% of vegetation)",
         y = "Grasses and forbs species richness\n",
         title = "C\n") +
    scale_x_continuous(limits = c(0, 100),
                       breaks = c(0, 25, 50, 75, 100)))

herbs_shrubs_trees_panel <- grid.arrange(tree_cover_herb_richness_plot,
                                         shrub_cover_herb_richness_plot,
                                         tree_shrub_cover_herb_richness_plot, ncol = 3)

ggsave(herbs_shrubs_trees_panel, file = "figures/herbs_shrubs_trees_panel.png",
       height = 5, width = 15)