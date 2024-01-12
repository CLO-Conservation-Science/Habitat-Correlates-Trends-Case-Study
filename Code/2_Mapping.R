#### ------------------------------------------------------------------------------ ####
#### This script produces plots to visualize results from the manuscript
##   1. Create map of species abundance
##   2. Create map of species trend
##   3. Create map of pland, geographic effect & effect on abundance for each predictor
##   4. Create 1D effect plot for each predictor
#### ------------------------------------------------------------------------------ ####
library(here)
library(sf)
library(terra)
library(rnaturalearth)
library(stringr)
library(ggplot2)


#### Create species list to loop through for all figures
spp_list <- c("brespa", "sagthr", "sagspa1")
spp_names <- c("Brewer's Sparrow", "Sage Thrasher", "Sage Sparrow")

#### Create Figure directory for each species
for(i in 1:length(spp_list)){
  dir.create(file.path(here("Results", "Figures", "All Maps", spp_list[i])))
}

for(i in 1:length(spp_list)){
  
#### ------------------------- 1. Create map of species abundance ------------------------ ####
  
#### Load data
abd <- read.csv(here("Results", spp_list[i], paste0(spp_list[i], "_abundance_plotting.csv")))

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

ggsave(here("Results", "Figures","All Maps",spp_list[i],paste0(spp_list[i],"_Abundance.png")), dpi = 600, width = 5, height = 6, units = "in")



#### ------------------------- 2. Create map of species trend ------------------------ ####

#### Load data
trend <- read.csv(here("Results", spp_list[i], paste0(spp_list[i], "_trend_plotting.csv")))

#### Create the plot
ggplot(data = trend) +
  borders("world", col = "grey50", fill = "grey80", size = 0.075) +
  borders("state", col = "grey50", fill = NA, size = 0.075) +
  geom_point(aes(x = longitude, y = latitude, col = abd_ppy, fill = abd_ppy, size = abd$abd), pch = 21) +
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

ggsave(here("Results", "Figures","All Maps",spp_list[i],paste0(spp_list[i],"_Trend.png")), dpi = 600, width = 5, height = 6, units = "in")


#### ------------------------- 3. Create map of pland, geographic effect & effect on abundance for each predictor ------------------------ ####

#### Load data
pland <- read.csv(here("Results", spp_list[i], paste0(spp_list[i], "_pland_plotting.csv")))
geo <- read.csv(here("Results", spp_list[i], paste0(spp_list[i], "_geo.effect_plotting.csv")))
geo.abd <- read.csv(here("Results", spp_list[i], paste0(spp_list[i], "_geo.abd.effect_plotting.csv")))

for(l in 3:ncol(geo)){
  pland.names <- str_remove(colnames(geo), ".PLAND") %>%
    str_replace_all("[.]"," ") %>%
    tolower()

  #### Create the map for PLAND
  pland_sub <- pland[,c(1:2,l)]
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

  ggsave(here("Results", "Figures","All Maps",spp_list[i],paste0(spp_list[i],"_",pland.names[l],"_2021PLAND.png")), dpi = 600, width = 5, height = 6, units = "in")


  #### Create the map for the geographic effect
  geo_sub <- geo[,c(1:2,l)]
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

  ggsave(here("Results", "Figures","All Maps",spp_list[i],paste0(spp_list[i],"_",pland.names[l],"_GeographicEffect.png")), dpi = 600, width = 5, height = 6, units = "in")


  #### Create the map for the effect on change in abundance
  geo.abd_sub <- geo.abd[,c(1:2,l)]
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

  ggsave(here("Results", "Figures","All Maps",spp_list[i],paste0(spp_list[i],"_",pland.names[l],"_ChangeInAbundance.png")), dpi = 600, width = 5, height = 6, units = "in")


  #### ------------------------- 3. Create 1D effect plot for each predictor ------------------------ ####

  effect_1D_plotting <- read.csv(here("Results", spp_list[i], paste0(spp_list[i],"_1Deffect_plotting.csv")))

  effect_1D_plotting_sub <- effect_1D_plotting[,grepl(colnames(geo)[l], colnames(effect_1D_plotting))]

  ggplot(data = effect_1D_plotting_sub) +
    geom_line(mapping = aes(x = effect_1D_plotting_sub[,1], y = effect_1D_plotting_sub[,2])) +
    geom_ribbon(aes(x = effect_1D_plotting_sub[,1], ymin = effect_1D_plotting_sub[,2]-effect_1D_plotting_sub[,3], ymax =
                      effect_1D_plotting_sub[,2]+effect_1D_plotting_sub[,3]), alpha = 0.5, fill = "grey80") +
    coord_cartesian(xlim = c(0, max(effect_1D_plotting_sub[,1])+5),
                             ylim = c(-2, 2), expand = FALSE) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill= NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          text = element_text(size = 18)) +
    xlab(paste0("Percent ", pland.names[l])) + ylab("Marginal effect size")

  ggsave(here("Results", "Figures","Marginal effect plots",spp_list[i],paste0(spp_list[i],"_",pland.names[l],"_1D_effectplot.png")), dpi = 600, width = 5, height = 6, units = "in")


} # l - predictors

} # i = species
