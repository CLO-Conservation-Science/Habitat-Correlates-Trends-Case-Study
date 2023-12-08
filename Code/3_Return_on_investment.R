#### ----------------------------------------------------------------------------------- ####
#### Conduct a Return on Investment scenario using 2021 land cover
##   1. Loop through all species and predict to modified predictor surface.
##   2. Write functions for results
##   3. Compare between species and export datasets
##   4. Mapping
#### ----------------------------------------------------------------------------------- ####
library(tidyverse)
library(sf)
library(mgcv)
library(here)


#### Inputs
spp_list <- c("brespa", "sagthr", "sagspa1")   # Species to include
land.sub <- "Sparse.Herbaceous.PLAND"          # Land cover to decrease
land.add <- "Sparse.Forests.PLAND"             # Land cover to increase
perc.change <- 10                              # Amount of change (%)
polygon <- st_read(here("Results","ROI_Example","ROI_polygon.shp"))



#### -------------------- 1. Loop through 3 species and save predictions ---------------- ####
abd.unchanged <- vector("list", length(spp_list))
names(abd.unchanged) <- spp_list
abd.modified <- abd.unchanged
trend.modified <- abd.unchanged
trend.unchanged <- abd.unchanged


#### LOOP THROUGH SPECIES
for(i in spp_list){
  
  #### Prepare modified landcover data
  load(here("Results", i, paste0(i, "_gam_fit.RData")))
  D21 <- read.csv(here("Results", i, paste0(i, "_2021_landcovers.csv")))
  
  ## Select the SRD cells that fall within the polygon
  D21.sf <- D21 %>% st_as_sf(coords = c("longitude","latitude"), crs = st_crs(polygon)) # Same CRS
  D21.sf <- D21.sf[st_intersects(D21.sf, polygon) %>% lengths > 0,]                       # Faster than st_intersection()
  cells.in.poly <- D21.sf$ROW_NUM
  rm(D21.sf)
  
  ## Select the SRD cells that we are able to modify based on 2021 landcover values
  D21 <- D21 %>% filter(
    ROW_NUM %in% cells.in.poly,
    D21[, land.sub] > perc.change,            # Must be enough of the landcover we're decreasing
    D21[, land.add] <= (100 - perc.change))    # Must be able to increase without topping 100%
  # in our case, this gives 339 cells to modify.
  
  ## Modify the chosen landcover variable for cells in cellIDs
  D21.modified <- D21
  D21.modified[ , land.sub] <- D21.modified[ , land.sub] - perc.change
  D21.modified[ , land.add] <- D21.modified[ , land.add] + perc.change
  
  
  #### Predict GAM to the unchanged and modified surfaces
  pred <- predict.gam(d.gam, newdata = D21, type = "iterms", se.fit = TRUE)  
  pred.modified <- predict.gam(d.gam, newdata = D21.modified, type = "iterms", se.fit = TRUE)  

  
  #### Build geo.abd.modified
  columns <- names(d.gam$var.summary)[-which(names(d.gam$var.summary)=="elev")]  # focus on PLANDs
  geo.abd.modified <- as.data.frame(matrix(nrow = nrow(D21.modified), ncol = length(columns)))
  names(geo.abd.modified) <- columns
  geo.abd.modified$longitude <- D21.modified$longitude
  geo.abd.modified$latitude <- D21.modified$latitude
  geo.abd.modified$ROW_NUM <- D21.modified$ROW_NUM
  geo.abd.modified$abd <- D21.modified$abd
  geo.abd.modified$ppy <- D21.modified$response
  
  temp <- pred.modified$fit * D21.modified$abd
  zzz_max_pred <- abs(quantile(temp, probs = c(0.99)))           # 99% quantile of effect size across all predictors
  PLANDs <- colnames(geo.abd.modified)[grepl('PLAND', colnames(geo.abd.modified))]
  
  ## Loop through the modeled land covers to add predictions
  for(j in PLANDs){
    zero_feature <- D21.modified[ , j] 
    zero_feature[zero_feature!=0] <- 1
    zzz <- pred.modified$fit[ ,paste0("s(", j, ")")] * D21.modified$abd * zero_feature
    zzz[zzz < -1*zzz_max_pred] <- -1*zzz_max_pred
    zzz[zzz > zzz_max_pred] <- zzz_max_pred
    
    geo.abd.modified[ ,j] <- zzz
  }
    
  
  #### Build geo.abd
  geo.abd <- as.data.frame(matrix(nrow = nrow(D21), ncol = length(columns)))
  names(geo.abd) <- columns
  geo.abd$longitude <- D21$longitude
  geo.abd$latitude <- D21$latitude
  geo.abd$ROW_NUM <- D21$ROW_NUM
  geo.abd$abd <- D21$abd
  geo.abd$ppy <- D21$response
  
  ## Loop through the modeled land covers to add predictions
  for(j in PLANDs){
    zero_feature <- D21[ , j] 
    zero_feature[zero_feature!=0] <- 1
    zzz <- pred$fit[ ,paste0("s(", j, ")")] * D21$abd * zero_feature
    zzz[zzz < -1*zzz_max_pred] <- -1*zzz_max_pred
    zzz[zzz > zzz_max_pred] <- zzz_max_pred
    
    geo.abd[ ,j] <- zzz
  }
  rm(temp, zzz_max_pred)
  
  
  #### Build geo.trend.modified
  geo.trend.modified <- geo.abd.modified
  geo.trend.modified[ , c(grep("PLAND", names(geo.trend.modified)))] <- NA  # Empty PLAND columns
  zzz_max_pred2 <- abs(quantile(pred.modified$fit, probs = c(0.99)))    
  
  ## Loop through the modeled land covers to add predictions
  for(j in PLANDs){
    zero_feature <- D21.modified[ , j] 
    zero_feature[zero_feature!=0] <- 1
    zzz <- pred.modified$fit[ ,paste0("s(", j, ")")] * zero_feature
    zzz[zzz < -1*zzz_max_pred2] <- -1*zzz_max_pred2
    zzz[zzz > zzz_max_pred2] <- zzz_max_pred2
    
    geo.trend.modified[ ,j] <- zzz
  }
  
  
  #### Build geo.trend
  geo.trend <- geo.abd
  geo.trend[ , c(grep("PLAND", names(geo.trend)))] <- NA  # Empty PLAND columns
  
  ## Loop through the modeled land covers to add predictions
  for(j in PLANDs){
    zero_feature <- D21[ , j] 
    zero_feature[zero_feature!=0] <- 1
    zzz <- pred$fit[ ,paste0("s(", j, ")")] * zero_feature
    zzz[zzz < -1*zzz_max_pred2] <- -1*zzz_max_pred2
    zzz[zzz > zzz_max_pred2] <- zzz_max_pred2
    
    geo.trend[ ,j] <- zzz
  }
  
  ## Add to output
  abd.modified[[i]] <- geo.abd.modified
  abd.unchanged[[i]] <- geo.abd
  trend.modified[[i]] <- geo.trend.modified
  trend.unchanged[[i]] <- geo.trend
} # END LOOP THROUGH SPECIES

  
  
## Save! 
save(abd.modified, abd.unchanged, trend.modified, trend.unchanged,
     file = here("Results","ROI_Example", paste0(land.sub, "_", land.add, perc.change, ".RData")))



#### ----------------------- 2. Write functions for results ----------------------- ####
rm(list=setdiff(ls(), c("spp_list","land.add","land.sub", "perc.change"))) # start fresh
load(here("Results","ROI_Example","Sparse.Herbaceous.PLAND_Sparse.Forests.PLAND10.RData"))


#### Function to produce plotting data including columns on 
# a) difference in marginal effect attributed to modifying land.sub
# b) difference in marginal effect attributed to modifying land.add
# c) difference in marginal effect attributed to modifying both
plot_data <- function(spp, land.sub, land.add){
  
  ## Organize the data
  geo.abd1 <- abd.unchanged[[spp]]
  geo.abd2 <- abd.modified[[spp]]
  output <- geo.abd1 %>% select(latitude, longitude, ROW_NUM, abd)
  
  ## Calculate cell-wise differences in marginal effects
  output$diff_land.sub <- geo.abd2[ ,land.sub] - geo.abd1[ ,land.sub]
  output$diff_land.add <- geo.abd2[ ,land.add] - geo.abd1[ ,land.add]
  output$diff_both <- output$diff_land.sub + output$diff_land.add
  return(output)
}


##### Function to compare % of population 5 years after both modifications
compare.5years <- function(spp){
  
  ## Organize the data
  geo.trend1 <- trend.unchanged[[spp]]
  geo.trend2 <- trend.modified[[spp]]
  u <- data.frame(year0 = geo.trend1$abd,
                  year1 = NA, year2 = NA, year3 = NA, year4 = NA, year5 = NA)  # u for "unchanged"
  m <- u                                                                       # m for "modified"
  
  ## Population change without modification                                                                    
  prop.py.u <- geo.trend1$ppy/100         # proportion per year without modification                                             
  ppy.residual <- geo.trend1$ppy - apply(geo.trend1[, c(land.sub, land.add)], 1, sum) 
  # above: residual ppy not attributed to the 2 landcovers                          
  
  u$year1 <- u$year0 + (u$year0 * prop.py.u)
  u$year2 <- u$year1 + (u$year1 * prop.py.u)
  u$year3 <- u$year2 + (u$year2 * prop.py.u)
  u$year4 <- u$year3 + (u$year3 * prop.py.u)
  u$year5 <- u$year4 + (u$year4 * prop.py.u)
  
  ## Population change WITH modification                                                                    
  ppy.m <- apply(geo.trend2[, c(land.sub, land.add)], 1, sum) # ppy attributed to the two modified landcovers
  prop.py.m <- (ppy.m + ppy.residual)/100                     # add in the rest of the residual ppy
  
  m$year1 <- m$year0 + (m$year0 * prop.py.m)
  m$year2 <- m$year1 + (m$year1 * prop.py.m)
  m$year3 <- m$year2 + (m$year2 * prop.py.m)
  m$year4 <- m$year3 + (m$year3 * prop.py.m)
  m$year5 <- m$year4 + (m$year4 * prop.py.m)
  
  # Sum the years and calculate the % difference
  output <- c("current.n"=round(sum(geo.trend1$abd),0), "unmodified.n"=round(sum(u$year5),0), "modified.n"=round(sum(m$year5),0), 
              "perc.pop.unmod"=NA, "perc.pop.current"=NA)
  output["perc.pop.current"] <- round(((output["modified.n"] - output["current.n"]) / output["current.n"]) * 100,1)
  output["perc.pop.unmod"] <- round(((output["modified.n"] - output["unmodified.n"]) / output["unmodified.n"]) * 100,1)
  output["perc.pop.current"] <- paste0(output["perc.pop.current"], "%")
  output["perc.pop.unmod"] <- paste0(output["perc.pop.unmod"], "%")
  
  return(output)
}


#### --------------------- 3. Compare between species and export datasets --------------------- ####


#### Generate and export plotting data
brespa.plot <- plot_data(spp = "brespa", land.sub = land.sub, land.add = land.add)
sagthr.plot <- plot_data(spp = "sagthr", land.sub = land.sub, land.add = land.add)
sagspa1.plot <- plot_data(spp = "sagspa1", land.sub = land.sub, land.add = land.add)

## Examine results
hist(brespa.plot$diff_both)

## Export
save(brespa.plot, sagthr.plot, sagspa1.plot, file = here("Results","ROI_Example","plotting_output.RData"))


#### Examine effect of modification after 5 years
compare.5years(spp = "brespa")
compare.5years(spp = "sagthr")
compare.5years(spp = "sagspa1")




#### --------------------------------- 4. Mapping ----------------------------------- ####
#### Load in results files for plotting
load(here("Results","ROI_Example", "plotting_output.RData"))
polygon <- st_read(here("Results","ROI_Example","ROI_polygon.shp"))
results.list <- list(brespa.plot, sagthr.plot, sagspa1.plot)

#### Difference in marginal effect attributed to change in land.sub (Sparse Herb)
for(i in 1:length(spp_list)){
  plot.data <- results.list[[i]] %>% select(latitude, longitude, diff_land.sub)
  
  ggplot(data = plot.data) + 
    borders("world", col = "grey50", fill = "grey80", size = 0.075) +
    borders("state", col = "grey50", fill = NA, size = 0.075) +
    geom_point(aes(x = longitude, y = latitude, col = diff_land.sub, fill = diff_land.sub), pch = 21, size = 2.5) + 
    scale_fill_gradient2(low = "#40004B", mid = "#f7f7f7", high = "#00441B",
                         midpoint = 0, 
                         guide = guide_colorbar(frame.colour = "black", ticks.colour = NA), 
                         aesthetics = c("color", "fill"),
                         name = "",
                         limits = c(-0.8,0.8),
                         breaks = c(-0.8, 0, 0.8),
                         labels = c("more \nnegative", "", "more \npositive")) +
    geom_sf(data = polygon, fill = NA, col = "black", lwd = 1) + 
    coord_sf(xlim = c(-120, -107), ylim = c(36,47), clip = "on") +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(), #remove major gridlines
          panel.grid.minor = element_blank(), #remove minor gridlines
          legend.background = element_rect(color = NA, fill = NA), #transparent legend bg
          legend.position = c(0.12, 0.18),
          legend.key.width = unit(0.35, "cm"),
          legend.key.height = unit(0.4, "cm"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          text = element_text(size = 18),
          legend.spacing.x = unit(0.1, "cm")) + 
    xlab("Longitude") + ylab("Latitude") 
  
  ggsave(here("Results","ROI_Example", paste0(spp_list[i], "-", perc.change, land.sub, ".png")), dpi = 600, width = 5, height = 6, units = "in")
  
}




#### Difference in marginal effect attributed to change in land.add (Sparse Forest)
for(i in 1:length(spp_list)){
  plot.data <- results.list[[i]] %>% select(latitude, longitude, diff_land.add)
  
  ggplot(data = plot.data) + 
    borders("world", col = "grey50", fill = "grey80", size = 0.075) +
    borders("state", col = "grey50", fill = NA, size = 0.075) +
    geom_point(aes(x = longitude, y = latitude, col = diff_land.add, fill = diff_land.add), pch = 21, size = 2.5) + 
    scale_fill_gradient2(low = "#40004B", mid = "#f7f7f7", high = "#00441B",
                         midpoint = 0, 
                         guide = guide_colorbar(frame.colour = "black", ticks.colour = NA), 
                         aesthetics = c("color", "fill"),
                         name = "",
                         limits = c(-0.8,0.8),
                         breaks = c(-0.8, 0, 0.8),
                         labels = c("more \nnegative", "", "more \npositive")) +
    geom_sf(data = polygon, fill = NA, col = "black", lwd = 0.75) + 
    coord_sf(xlim = c(-120, -107), ylim = c(36,47), clip = "on") +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(), #remove major gridlines
          panel.grid.minor = element_blank(), #remove minor gridlines
          legend.background = element_rect(color = NA, fill = NA), #transparent legend bg
          legend.position = c(0.12, 0.18),
          legend.key.width = unit(0.35, "cm"),
          legend.key.height = unit(0.4, "cm"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          text = element_text(size = 18),
          legend.spacing.x = unit(0.1, "cm")) + 
    xlab("Longitude") + ylab("Latitude") 
  
  ggsave(here("Results","ROI_Example", paste0(spp_list[i], "+", perc.change, land.add, ".png")), dpi = 600, width = 5, height = 6, units = "in")
  
}



#### Difference in marginal effect attributed to BOTH changes
for(i in 1:length(spp_list)){
  plot.data <- results.list[[i]] %>% select(latitude, longitude, diff_both)
  
  ggplot(data = plot.data) + 
    borders("world", col = "grey50", fill = "grey80", size = 0.075) +
    borders("state", col = "grey50", fill = NA, size = 0.075) +
    geom_point(aes(x = longitude, y = latitude, col = diff_both, fill = diff_both), pch = 21, size = 2.5) + 
    scale_fill_gradient2(low = "#40004B", mid = "#f7f7f7", high = "#00441B",
                              midpoint = 0, 
                              guide = guide_colorbar(frame.colour = "black", ticks.colour = NA), 
                              aesthetics = c("color", "fill"),
                              name = "",
                              limits = c(-0.8,0.8),
                              breaks = c(-0.8, 0, 0.8),
                              labels = c("more \nnegative", "", "more \npositive")) +
    geom_sf(data = polygon, fill = NA, col = "black", lwd = 0.75) + 
    coord_sf(xlim = c(-120, -107), ylim = c(36,47), clip = "on") +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(), #remove major gridlines
          panel.grid.minor = element_blank(), #remove minor gridlines
          legend.background = element_rect(color = NA, fill = NA), #transparent legend bg
          legend.position = c(0.12, 0.18),
          legend.key.width = unit(0.35, "cm"),
          legend.key.height = unit(0.4, "cm"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          text = element_text(size = 18),
          legend.spacing.x = unit(0.1, "cm")) + 
    xlab("Longitude") + ylab("Latitude") 
  
  ggsave(here("Results","ROI_Example", paste0(spp_list[i], "_", perc.change, "both.png")), dpi = 600, width = 5, height = 6, units = "in")
  
}
