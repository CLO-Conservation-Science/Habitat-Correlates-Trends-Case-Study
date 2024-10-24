#### ------------------------------------------------------------------------------ ####
#### This script produces plots to visualize results from the manuscript
##   1. Create maps of species abundance
##   2. Create maps of species trend
##   3. Create maps of % land cover, geographic effect, & effect on abundance for each predictor
##   4. Create 1D marginal effect plot for each predictor
#### ------------------------------------------------------------------------------ ####
library(here)
library(sf)
library(terra)
library(rnaturalearth)
library(stringr)
library(ggplot2)
library(tidyverse)


#### Create species list to loop through for all figures
spp_list <- c("brespa", "sagthr", "sagspa1")
spp_names <- c("Brewer's Sparrow", "Sage Thrasher", "Sage Sparrow")


#### Load data with trend replicates
trend <- read.csv(here("Data", "trend_reps_wide_2021.csv"))


#### Create Figure directory for each species
for(i in 1:length(spp_list)){
  suppressWarnings(dir.create(file.path(here("Results", "Figures", "Maps", spp_list[i]))))
  suppressWarnings(dir.create(file.path(here("Results", "Figures", "Marginal_effect_plots", spp_list[i]))))
}

for(i in 1:length(spp_list)){
  
#### ------------------------- 1. Create map of species abundance ------------------------ ####
  
#### Load data
abd <- read.csv(here("Results", spp_list[i], paste0(spp_list[i], "_2021_landcovers.csv")))
abd <- abd %>% select(abd:longitude)

#### Create the plot
ggplot(data = abd) + 
  borders("world", col = "grey50", fill = "grey80", size = 0.075) +
  borders("state", col = "grey50", fill = NA, size = 0.075) +
  geom_point(aes(x = longitude, y = latitude, col = abd, fill = abd), pch = 21) + 
  scale_fill_gradient2(low = "white", mid = "#41b6c4", high = "#023858",
                       midpoint = max(abd$abd)/2, 
                       guide = guide_colorbar(frame.colour = "black", ticks.colour = "black"), 
                       aesthetics = c("color", "fill"),
                       name = "Relative \nabundance") +
  coord_cartesian(xlim = c(-125, -103), ylim = c(33,52), clip = "on") +
  theme(panel.border = element_rect(colour = "black", fill = NA),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(color = NA, fill = NA), #transparent legend bg
    legend.position = c(0.11, 0.15),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(0.3, "cm"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    text = element_text(size = 18)) +
  xlab("Longitude") + ylab("Latitude")

ggsave(here("Results", "Figures","Maps",spp_list[i],paste0(spp_list[i],"_Abundance.png")), dpi = 600, width = 5, height = 6, units = "in")



#### ------------------------- 2. Create map of species trend ------------------------ ####

#### Organize trend data for species i
spp_trend <- trend %>% filter(species==spp_list[i]) %>%
  rename_with(~gsub("X", "", .x)) %>%
  select(-c(species, fold)) %>% 
  t() %>%
  as.data.frame() %>% 
  drop_na()

## Calculate median, upper, and lower 80% trend estimate 
spp_trend_plot <- data.frame(latitude = abd$latitude, 
                            longitude = abd$longitude,
                            ppy_median = apply(spp_trend, 1, median),
                            ppy_upperCI80 = apply(spp_trend, 1, quantile, probs = 0.9),
                            ppy_lowerCI80 = apply(spp_trend, 1, quantile, probs = 0.1)) 

## To aid visualization only, max out the trends at +4 and -4
spp_trend_plot$ppy_median[which(spp_trend_plot$ppy_median > 4)] <- 4
spp_trend_plot$ppy_median[which(spp_trend_plot$ppy_median < -4)] <- -4
 
  
#### Create the plot (change "ppy_median" to "pyy_upperCI80" or "ppy_lowerCI80" to plot trend confidence intervals)
ggplot(data = spp_trend_plot) +
  borders("world", col = "grey50", fill = "grey80", size = 0.075) +
  borders("state", col = "grey50", fill = NA, size = 0.075) +
  geom_point(aes(x = longitude, y = latitude, col = ppy_median, fill = ppy_median, size = abd$abd), pch = 21) +
  scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#023858",
    midpoint = 0,
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black"),
    aesthetics = c("color", "fill"),
    name = "PPY \nTrend") +
  scale_radius(range = c(0.0, 1.5), guide = "none") +
  coord_cartesian(xlim = c(-125, -103), ylim = c(33,52), clip = "on") +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(), #remove major gridlines
        panel.grid.minor = element_blank(), #remove minor gridlines
        legend.background = element_rect(color = NA, fill = NA), #transparent legend bg
        legend.position = c(0.1, 0.15),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.3, "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 18))

ggsave(here("Results", "Figures","Maps",spp_list[i],paste0(spp_list[i],"_Trend.png")), dpi = 600, width = 5, height = 6, units = "in")


#### --------------- 3. Create map of land cover, geographic effect, & effect on abundance for each predictor -------------- ####

#### Load data
pland21 <- read.csv(here("Results", spp_list[i], paste0(spp_list[i], "_2021_landcovers.csv")))
pland21 <- pland21 %>% select(-c(ROW_NUM:abd, elev))

pland07 <- read.csv(here("Results", spp_list[i], paste0(spp_list[i], "_2007_landcovers.csv")))
pland07 <- pland07 %>% select(-c(ROW_NUM:abd, elev))

geo <- read.csv(here("Results", spp_list[i], paste0(spp_list[i], "_geo.effect_plotting.csv")))
geo.abd <- read.csv(here("Results", spp_list[i], paste0(spp_list[i], "_geo.abd.effect_plotting.csv")))

effect_1D <- read.csv(here("Results", spp_list[i], paste0(spp_list[i],"_1Deffect_plotting.csv")))


#### Create plotting datasets with mean and sd of effects for each PLAND (% land cover variable)

## For the geographic trend effect dataset
geo_plot_mean <- geo %>% group_by(ROW_NUM) %>%
  summarize(across(everything(), mean)) %>%
  select(-c(fold, ROW_NUM))
geo_plot_sd <- geo %>% group_by(ROW_NUM) %>%
  select(-c(latitude, longitude, fold)) %>%
  summarize(across(everything(), sd)) %>%
  mutate(latitude = geo_plot_mean$latitude,
         longitude = geo_plot_mean$longitude, .before = ) %>%
  select(latitude, longitude, everything(), -ROW_NUM) 
  
## For the abundance-weighted effect dataset
geo.abd_plot_mean <- geo.abd %>% group_by(ROW_NUM) %>%
  summarize(across(everything(), mean)) %>%
  select(-c(fold, ROW_NUM))
geo.abd_plot_sd <- geo.abd %>% group_by(ROW_NUM) %>%
  select(-c(latitude, longitude, fold)) %>%
  summarize(across(everything(), sd)) %>%
  mutate(latitude = geo_plot_mean$latitude,
         longitude = geo_plot_mean$longitude) %>%
  select(latitude, longitude, everything(), -ROW_NUM) 
rm(geo, geo.abd)

## For the 1-D marginal effects dataset
effect_1D_mean <- effect_1D %>% group_by(ROW_NUM) %>%
  summarize(across(everything(), mean)) %>%
  select(-c(Fold, ROW_NUM))
# This is equivalent to taking the mean of the mean, upper and lower SE bands for each fold. 
rm(effect_1D)

#### Start forloop
for(l in 3:ncol(geo_plot_mean)){
  pland.names <- str_remove(colnames(geo_plot_mean), ".PLAND") %>%
    str_replace_all("[.]"," ") %>%
    tolower()

  #### Create the map for PLAND
  pland21 <- pland21[names(geo_plot_mean)]  # make sure column orders match
  pland_sub <- pland21[,c(1:2,l)]
  
  ggplot(data = pland_sub) +
    borders("world", col = "grey50", fill = "grey80", size = 0.075) +
    borders("state", col = "grey50", fill = NA, size = 0.075) +
    geom_point(aes(x = longitude, y = latitude, col = pland_sub[,3], fill = pland_sub[,3]), pch = 21) +
    scale_fill_gradient2(low = "white", mid = "#78c679", high = "#00441B",
                         midpoint = 50,
                         guide = guide_colorbar(frame.colour = "black", ticks.colour = "black"),
                         aesthetics = c("color", "fill"),
                         name = "% in \n2021", limits = c(0, 100)) +
    coord_cartesian(xlim = c(-125, -103), ylim = c(33,52), clip = "on") +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(), #remove major gridlines
          panel.grid.minor = element_blank(), #remove minor gridlines
          legend.background = element_rect(color = NA, fill = NA), #transparent legend bg
          legend.position = c(0.12, 0.14),
          legend.key.width = unit(0.35, "cm"),
          legend.key.height = unit(0.4, "cm"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          text = element_text(size = 18)) +
    xlab("Longitude") + ylab("Latitude")

  ggsave(here("Results", "Figures","Maps",spp_list[i],paste0(spp_list[i],"_",pland.names[l],"_2021PLAND.png")), dpi = 600, width = 5, height = 6, units = "in")


  #### Create the map for the mean geographic effect
  geo_sub <- as.data.frame(geo_plot_mean[,c(1:2,l)])
  ggplot(data = geo_sub) +
    borders("world", col = "grey50", fill = "grey80", size = 0.075) +
    borders("state", col = "grey50", fill = NA, size = 0.075) +
    geom_point(aes(x = longitude, y = latitude, col = geo_sub[,3], fill = geo_sub[,3]), pch = 21) +
    scale_fill_gradient2(low = "#40004B", mid = "#f7f7f7", high = "#00441B",
                         midpoint = 0,
                         guide = guide_colorbar(frame.colour = "black", ticks.colour = NA),
                         aesthetics = c("color", "fill"),
                         name = "",
                         breaks = c(min(geo_sub[,3]), 0, max(geo_sub[,3])),
                         labels = c("more \nnegative", "", "more \npositive")) +
    coord_cartesian(xlim = c(-125, -103), ylim = c(33,52), clip = "on") +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(), #remove major gridlines
          panel.grid.minor = element_blank(), #remove minor gridlines
          legend.background = element_rect(color = NA, fill = NA), #transparent legend bg
          legend.position = c(0.12, 0.14),
          legend.key.width = unit(0.35, "cm"),
          legend.key.height = unit(0.4, "cm"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          text = element_text(size = 18),
          legend.spacing.x = unit(0.1, "cm")) +
      xlab("Longitude") + ylab("Latitude")

  ggsave(here("Results", "Figures","Maps",spp_list[i],paste0(spp_list[i],"_",pland.names[l],"_GeographicEffect_MEAN.png")), dpi = 600, width = 5, height = 6, units = "in")

  
  #### Create the map for the SD geographic effect
  geo_sub <- as.data.frame(geo_plot_sd[,c(1:2,l)])
  ggplot(data = geo_sub) +
    borders("world", col = "grey50", fill = "grey80", size = 0.075) +
    borders("state", col = "grey50", fill = NA, size = 0.075) +
    geom_point(aes(x = longitude, y = latitude, col = geo_sub[,3], fill = geo_sub[,3]), pch = 21) +
    scale_fill_gradient2(low = "#FFFFFF", mid = "#CBC2F2", high = "#0800B7",
                         midpoint = max(geo_sub[,3])/2, 
                         guide = guide_colorbar(frame.colour = "black", ticks.colour = NA), 
                         aesthetics = c("color", "fill"),
                         name = "SD") +
    coord_cartesian(xlim = c(-125, -103), ylim = c(33,52), clip = "on") +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(), #remove major gridlines
          panel.grid.minor = element_blank(), #remove minor gridlines
          legend.background = element_rect(color = NA, fill = NA), #transparent legend bg
          legend.position = c(0.12, 0.14),
          legend.key.width = unit(0.35, "cm"),
          legend.key.height = unit(0.4, "cm"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          text = element_text(size = 18),
          legend.spacing.x = unit(0.1, "cm")) +
    xlab("Longitude") + ylab("Latitude")
  
  ggsave(here("Results", "Figures","Maps",spp_list[i],paste0(spp_list[i],"_",pland.names[l],"_GeographicEffect_SD.png")), dpi = 600, width = 5, height = 6, units = "in")
  

  #### Create the map for the mean effect on change in abundance
  geo.abd_sub <- as.data.frame(geo.abd_plot_mean[,c(1:2,l)])
  ggplot(data = geo.abd_sub) +
    borders("world", col = "grey50", fill = "grey80", size = 0.075) +
    borders("state", col = "grey50", fill = NA, size = 0.075) +
    geom_point(aes(x = longitude, y = latitude, col = geo.abd_sub[,3], fill = geo.abd_sub[,3]), pch = 21) +
    scale_fill_gradient2(low = "#40004B", mid = "#f7f7f7", high = "#00441B",
                         midpoint = 0,
                         guide = guide_colorbar(frame.colour = "black", ticks.colour = NA),
                         aesthetics = c("color", "fill"),
                         name = "",
                         breaks = c(min(geo.abd_sub[,3]), 0, max(geo.abd_sub[,3])),
                         labels = c("more \nnegative", "", "more \npositive")) +
    coord_cartesian(xlim = c(-125, -103), ylim = c(33,52), clip = "on") +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(), #remove major gridlines
          panel.grid.minor = element_blank(), #remove minor gridlines
          legend.background = element_rect(color = NA, fill = NA), #transparent legend bg
          legend.position = c(0.12, 0.14),
          legend.key.width = unit(0.35, "cm"),
          legend.key.height = unit(0.4, "cm"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          text = element_text(size = 18)) +
    xlab("Longitude") + ylab("Latitude")

  ggsave(here("Results", "Figures","Maps",spp_list[i],paste0(spp_list[i],"_",pland.names[l],"_ChangeInAbundance_MEAN.png")), dpi = 600, width = 5, height = 6, units = "in")

  
  #### Create the map for the sd effect on change in abundance
  geo.abd_sub <- as.data.frame(geo.abd_plot_sd[,c(1:2,l)])
  ggplot(data = geo.abd_sub) +
    borders("world", col = "grey50", fill = "grey80", size = 0.075) +
    borders("state", col = "grey50", fill = NA, size = 0.075) +
    geom_point(aes(x = longitude, y = latitude, col = geo.abd_sub[,3], fill = geo.abd_sub[,3]), pch = 21) +
    scale_fill_gradient2(low = "#FFFFFF", mid = "#CBC2F2", high = "#0800B7",
                         midpoint = max(geo.abd_sub[,3])/2, 
                         guide = guide_colorbar(frame.colour = "black", ticks.colour = NA), 
                         aesthetics = c("color", "fill"),
                         name = "SD") +
    coord_cartesian(xlim = c(-125, -103), ylim = c(33,52), clip = "on") +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(), #remove major gridlines
          panel.grid.minor = element_blank(), #remove minor gridlines
          legend.background = element_rect(color = NA, fill = NA), #transparent legend bg
          legend.position = c(0.12, 0.14),
          legend.key.width = unit(0.35, "cm"),
          legend.key.height = unit(0.4, "cm"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          text = element_text(size = 18)) +
    xlab("Longitude") + ylab("Latitude")
  
  ggsave(here("Results", "Figures","Maps",spp_list[i],paste0(spp_list[i],"_",pland.names[l],"_ChangeInAbundance_SD.png")), dpi = 600, width = 5, height = 6, units = "in")
  
  
  
  #### ------------------------- 4. Create 1D effect plot for each predictor ------------------------ ####

  effect_1D_sub <- effect_1D_mean[,grepl(colnames(geo_plot_mean)[l], colnames(effect_1D_mean))]
  effect_1D_sub[, colnames(geo_plot_mean)[l]] <- pland07[, colnames(geo_plot_mean)[l]]
  effect_1D_sub <- as.data.frame(effect_1D_sub)

  
  ggplot(data = effect_1D_sub) +
    geom_line(mapping = aes(x = effect_1D_sub[,3], y = effect_1D_sub[,1])) +
    geom_ribbon(aes(x = effect_1D_sub[,3], ymin = effect_1D_sub[,1]-effect_1D_sub[,2], 
                ymax = effect_1D_sub[,1]+effect_1D_sub[,2]), alpha = 0.5, fill = "grey80")  +
    coord_cartesian(xlim = c(0, max(effect_1D_sub[,3])+5),
                             ylim = c(-2, 2), expand = FALSE) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill= NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          text = element_text(size = 18)) +
    xlab(paste0("Percent ", pland.names[l])) + ylab("Marginal effect size")

  ggsave(here("Results", "Figures","Marginal_effect_plots",spp_list[i],paste0(spp_list[i],"_",pland.names[l],"_1D_effectplot.png")), dpi = 600, width = 5, height = 6, units = "in")


} # l - predictors

} # i = species
