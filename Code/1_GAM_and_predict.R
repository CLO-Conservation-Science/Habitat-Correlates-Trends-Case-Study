#### ------------------------------------------------------------------------------------------- ####
#### This script uses a GAM to uncover the correlates of eBird population trends for 3 species
##   1. Prepare predictors and data for GAM models
##   2. Loop over species: fit the model, predict to 2021 land cover, and export datasets for plotting
#### ------------------------------------------------------------------------------------------- ####
library(tidyverse)
library(mgcv)
library(maps)
library(tictoc)
library(here)
library(dplyr)


#### Load data
load(here("Data", "trend_and_abd_wide_2021.RData"))
srd <- read.csv(here("Data", "landcover_2007.csv"))
srd21 <- read.csv(here("Data", "landcover_2021.csv"))
# srd = "spatial reference database"

## Check that Trend, Abundance, and land cover data sets are in the same order
sum(as.numeric(names(cluster_data_trend)) == srd$srd_id) == nrow(srd)
sum(srd$srd_id == srd21$srd_id) == nrow(srd)                   # check that row order matches
sum(names(srd)==names(srd21)) == ncol(srd)                     # check that columns match
# Should be TRUE



#### ------------------------- 1. Prepare data for the  model ------------------------ ####

#### Prep land cover data for modeling and predictions
srd <- rename(srd, ROW_NUM = srd_id)        
srd21 <- rename(srd21, ROW_NUM = srd_id)
srd_agg_pred <- srd %>% select(-ROW_NUM)


#### Sagebrush species information
spp_list <- c("brespa", "sagthr", "sagspa1")

#### Create folders to store results for each species
for(i in 1:length(spp_list)){
  dir.create(file.path(here("Results", spp_list[i])))
  dir.create(file.path(here("Results", "Figures","Marginal Effect Plots", spp_list[i])))
  }



#### -------------------------------2. Giant loop over species ------------------------------ ####
tic()  # Start timer
for (iii_spp in 1:length(spp_list)){
  
# =============================================================
# Single Species Response
# =============================================================
    spp_name <- spp_list[iii_spp]
    spp_index <- rownames(cluster_data_trend) == spp_name
 
    spp_abd <- cluster_data_abd[spp_index, ]
    spp_abd <- spp_abd[!is.na(spp_abd)]

    resp <- data.frame(
      response = as.numeric(cluster_data_trend[spp_index, ]),
      ROW_NUM = as.numeric(colnames(cluster_data_trend)), 
      abd = as.numeric(cluster_data_abd[spp_index, ]) )
    D <- cbind(resp, srd_agg_pred)

    D$weights <- apply(cluster_data_abd[spp_index, ], 2, mean, na.rm=T)
    D$weights <- D$weights/sum(D$weights, na.rm=T)

    # Trend uncertainties
    ttt_ul <- apply(cluster_data_trend_ul[spp_index, ], 2, mean, na.rm=T)
    ttt_ll <- apply(cluster_data_trend_ll[spp_index, ], 2, mean, na.rm=T)
    ttt_cl_width <- ttt_ul - ttt_ll
    
    # More weights on smaller widths
    ttt_wt <- 1/ttt_cl_width / 4
    D$weights <- D$weights * ttt_wt
    
    print(paste(iii_spp, spp_name)) 
  
# =============================================================
# Run the GAM
# =============================================================
  
  #### Prepare the data
  D <- D[!is.na(D$response),]
  dim(D)
  
  ## Re-normalize weights
  D$weights <- D$weights/mean(D$weights)

  ## Identify Predictors that vary sufficiently for smooths  
  min_ss <- 30
  pred_names <- names(srd_agg_pred)
  pred_unique <- rep(0, ncol(srd_agg_pred))
  for (iii in 1:ncol(srd_agg_pred)){
    pred_unique[iii] <- length(unique(
      D[ ,   names(D) == paste0(pred_names[iii],"")   ]))
  }
  
  cbind(pred_names, pred_unique)
  pred_names <- pred_names[ pred_unique > min_ss ]
  pred_names <-  pred_names[ !is.na(pred_names )]
  pred_names

   # #### Contstruct model Formula
  model_name <- NULL
  for (iii in 4:length(pred_names)){
    model_name[iii-3] <- 
    paste0(
        "s( ",pred_names[iii],",bs=\"ds\", k=4, m=c(1,0)) + ")
  }      
  model_name <- c("response ~", model_name,
      "s(elev, bs=\"ds\", k=10, m=c(1,0)) + ",
      "s(longitude, latitude, bs=\"ds\",k=200, m=c(1,0.5)) +", 
      "1")
  model_name <- paste(model_name, collapse = " ")

  #### Run the model
  # Note Selection + Extra Gamma penalization
  d.gam <- bam( 
    as.formula(model_name), 
    weights = weights, 
    gamma = 5,
    select = T, 
    discrete = T, 
    data = D )
  summary(d.gam)


# =============================================================
# Save Model Summaries  
# =============================================================
  sink(file=here("Results", spp_name, paste0(spp_name, "_gam.summary.txt")))
  print(summary(d.gam))
  sink()
  save(d.gam, D, file = here("Results", spp_name, paste0(spp_name, "_gam_fit.RData"))) # model and data
  
  ## Rank Strongest Effects based on F-stat
  max_feature_summaries <- length(pred_names) -1
  gam_pis <- NULL
  d.sum <- summary(d.gam)
  top_effect_dogindex <- order(d.sum$s.table[,3], decreasing=T)
  top_effect_dogindex <- top_effect_dogindex[1:max_feature_summaries]
  gam_pis <- as.data.frame(d.sum$s.table[top_effect_dogindex,3])
  names(gam_pis) <- "pis"
 
  #### Predictor Effect Estimates (type="iterms")
  dog  <- predict.gam(
    d.gam, 
    type = "iterms", se.fit = TRUE) 
  
  ## Save out predictor effect estimates for 1-D marginal effect plots
  effect_plotting_df <- cbind(D, data.frame(dog))
  write.csv(effect_plotting_df, here("Results", spp_name, paste0(spp_name,"_1Deffect_plotting.csv")), row.names = F)
  
  
# =============================================================
# Predict to 2021 land cover surface
# =============================================================
  
  ## Subset srd21 to the same rows used to fit the GAM
  lc21 <- srd21 %>% select(ROW_NUM, Barren.PLAND:Herbaceous.Wetlands.PLAND)
  D21 <- D %>% select(response:elev, weights) %>% left_join(lc21, by = "ROW_NUM")
  D21 <- D21[names(D)]
  
  ## Predict and export land cover dataset for ROI
  dog21 <- predict.gam(d.gam, newdata = D21, type = "iterms", se.fit = TRUE) 
  write.csv(D21, here("Results", spp_name, paste0(spp_name,"_2021_landcovers.csv")), row.names = F)
  
  
# =============================================================
# Produce datasets for interpretive plotting aids: Abundance and Trend. 
# =============================================================

  ## Initial settings for outputs to aid plotting
  zzz <- dog21$fit*spp_abd
  zzz_max_dog <- max(abs(quantile(zzz, probs = c(0.99))))
  zzz_lim_abdchange <- c(-1*zzz_max_dog, zzz_max_dog)

  ## Save out CSV with species abundance
  zzz <-  spp_abd 
  save.abd <- cbind("longitude" = D21$longitude, "latitude" = D21$latitude, "abd" = zzz)
  write.csv(save.abd, here("Results", spp_name, paste0(spp_name,"_abundance_plotting.csv")), row.names = F)
  
  ## Save out CSV with species trend
  zzz <-  D21$response
  zzz_max <- 4
  zzz[zzz < -1*zzz_max] <- -1*zzz_max
  zzz[zzz > zzz_max] <- zzz_max
  save.trend <- cbind("longitude" = D21$longitude, "latitude" = D21$latitude, "abd_ppy" = zzz)
  write.csv(save.trend, here("Results", spp_name, paste0(spp_name,"_trend_plotting.csv")), row.names = F)
  
  
# ===================================================================================
# Produce datasets for interpretive plotting aids: Landcover and geographic effects
# ===================================================================================
  
  ## Create containers to hold plotting results
  save.pland <- cbind("longitude" = D21$longitude, "latitude" = D21$latitude)
  save.geo <- cbind("longitude" = D21$longitude, "latitude" = D21$latitude)
  save.geo.abd <- cbind("longitude" = D21$longitude, "latitude" = D21$latitude)
    
  
  #### Initiate loop through all predictors
  count_effect <- 0  # This will count how many times we go through the loop below
  count_effect2 <- 0

  for(iii_effect in top_effect_dogindex){
    count_effect <- count_effect + 1
    zzz_name <- names(d.gam$var.summary)[top_effect_dogindex][count_effect]
    
  if (zzz_name != "longitude" & zzz_name != "elev"){
    count_effect2 <- count_effect2 + 1
  
    ## PLAND dataset
    zzz <- D21[, names(D21) == zzz_name] 
    save.pland <- cbind(save.pland, zzz)
    colnames(save.pland)[count_effect2+2] <- zzz_name
    
    ## Geographic PPY effect dataset
    zzz <- dog21$fit[, iii_effect]
    zzz_lim <- c(-1*max(abs(zzz)), max(abs(zzz)))
    save.geo <- cbind(save.geo, zzz)
    colnames(save.geo)[count_effect2+2] <- zzz_name
    
    ## Geographic abundance-weighted PPY effect dataset
    # Zero out areas w/o feature of interest so that importance is not summarized there
    zero_feature <- D21[, names(D21) == zzz_name ] 
    zero_feature[zero_feature!=0] <- 1
    zzz <- dog21$fit[,iii_effect] * spp_abd * zero_feature
    zzz[zzz < -1*zzz_max_dog] <- -1*zzz_max_dog
    zzz[zzz > zzz_max_dog] <- zzz_max_dog
    
    # Add to out SAVE.GEO.ABD dataframe for later plotting
    save.geo.abd <- cbind(save.geo.abd, zzz)
    colnames(save.geo.abd)[count_effect2+2] <- zzz_name
  } # if != longitude  


    #### Export plotting datasets to directory (save.pland, save.geo, save.geo.abd)
    write.csv(save.pland, here("Results", spp_name, 
                               paste0(spp_name,"_pland_plotting.csv")), row.names = F)
    write.csv(save.geo, here("Results", spp_name, 
                               paste0(spp_name,"_geo.effect_plotting.csv")), row.names = F)
    write.csv(save.geo.abd, here("Results", spp_name, 
                               paste0(spp_name,"_geo.abd.effect_plotting.csv")), row.names = F)
  
  } # Close loop through Predictors
  
} # Close loop through Sagebrush species

toc()   # end timer
