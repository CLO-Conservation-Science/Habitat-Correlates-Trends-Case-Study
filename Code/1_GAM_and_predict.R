#### ------------------------------------------------------------------------------------------- ####
#### This script uses GAMs to uncover the correlates of eBird population trends for 3 species
##   1. Prepare predictors and data for GAM models
##   2. Loop over species: fit the models, predict to 2021 land cover, and export datasets for plotting
#### ------------------------------------------------------------------------------------------- ####
library(tidyverse)
library(mgcv)
library(tictoc)
library(here)
library(dplyr)


#### Load data
abd_all <- read.csv(here("Data", "abundance_wide_2021.csv"))
trend_reps_all <- read.csv(here("Data", "trend_reps_wide_2021.csv"))
srd07 <- read.csv(here("Data", "landcover_2007.csv"))              # srd = "spatial reference database"
srd21 <- read.csv(here("Data", "landcover_2021.csv"))
abd_all <- rename_with(abd_all, ~gsub("X", "", .x))                # removes "X" from column names
trend_reps_all <- rename_with(trend_reps_all, ~gsub("X", "", .x))  # removes "X" from column names

## Check that Trend, Abundance, and land cover data sets are in the same order
trend_cells <- as.numeric(names(trend_reps_all)[3:ncol(trend_reps_all)])
sum(trend_cells == srd07$srd_id) == nrow(srd07)
sum(names(abd_all)==srd07$srd_id) == nrow(srd07)                    
sum(srd07$srd_id == srd21$srd_id) == nrow(srd07)    # check that row order matches
sum(names(srd07)==names(srd21)) == ncol(srd07)      # check that columns match



#### ------------------------- 1. Prepare data for the  model ------------------------ ####

#### Prep land cover data for modeling and predictions
srd07 <- rename(srd07, ROW_NUM = srd_id)        
srd21 <- rename(srd21, ROW_NUM = srd_id)
srd_agg_pred <- srd07 %>% select(-ROW_NUM)


#### Sagebrush species list
spp_list <- c("brespa", "sagthr", "sagspa1")
row.names(abd_all) <- spp_list


#### Create folders to store results for each species
for(i in 1:length(spp_list)){
  suppressWarnings(dir.create(file.path(here("Results", spp_list[i]))))
  suppressWarnings(dir.create(file.path(here("Results", spp_list[i], "GAM_outputs"))))
  }



#### ------------------------- 2. Giant loop over species and folds ----------------------- ####
tic()  # Start timer. Could take up to 1.5 hours on a laptop.

for (iii_spp in 1:length(spp_list)){
    
  #### Organize species-level data
  spp_name <- spp_list[iii_spp] 
  spp_index <- rownames(abd_all) == spp_name
  abd_spp <- abd_all[spp_index, ]
  trend_reps_spp <- trend_reps_all %>% filter(species==spp_name) %>%
    select(-c(species, fold))
  
  #### Identify Predictors that vary sufficiently for smooths  
  min_ss <- 30
  srd_spp <- srd_agg_pred %>% mutate(abd = as.numeric(abd_spp[1, ])) %>%
    drop_na(abd) %>%
    select(-abd)
  pred_names <- names(srd_spp)
  pred_unique <- rep(0, ncol(srd_spp))
  
  for (iii in 1:ncol(srd_spp)){
    pred_unique[iii] <- length(unique(srd_spp[ , names(srd_spp) == paste0(pred_names[iii],"")]))
  }
    
  pred_names <- pred_names[ pred_unique > min_ss ]
  pred_names <-  pred_names[ !is.na(pred_names )]
  pred_names
  print(paste("starting loop through folds for", spp_name))
    
    
# =============================================================
# Start loop through 100 folds of trend estimates 
# =============================================================
    
  #### Loop through 100 folds to propagate trend uncertainty
  r.sq <- NULL
  for(iii_fold in 1:nrow(trend_reps_spp)){
    print(paste("fold",iii_fold, spp_name)) 
      
    #### Prepare the data for modeling   
    resp <- data.frame(
      response = as.numeric(trend_reps_spp[iii_fold, ]),
      ROW_NUM = as.numeric(colnames(trend_reps_spp)), 
      abd = as.numeric(abd_spp[1, ]) )
    D <- cbind(resp, srd_agg_pred)
      
    ## Weight the likelihood by relative abundance (normalized)
    D$weights <- D$abd/mean(D$abd, na.rm = T)
    D <- D[!is.na(D$response),]
    
    ## Set up objects to receive outputs
    if(iii_fold == 1){
      gam.summaries <- as.data.frame(matrix(nrow = 0, ncol = length(pred_names)+1))
      names(gam.summaries) <- c("Fold", "Metric", pred_names[4:length(pred_names)], "Elev", "Lat-Lon")
    }
  
    
    
    # =============================================================
    # Run the GAM
    # =============================================================
      
    #### Construct model Formula
    model_name <- NULL
    for (iii in 4:length(pred_names)){
      model_name[iii-3] <- 
        paste0( "s( ",pred_names[iii],",bs=\"ds\", k=4, m=c(1,0)) + ")
    }      
    model_name <- c("response ~", model_name,
                    "s(elev, bs=\"ds\", k=10, m=c(1,0)) + ",
                    "s(longitude, latitude, bs=\"ds\",k=200, m=c(1,0.5)) +", 
                    "1")
    model_name <- paste(model_name, collapse = " ")
      
    #### Run the model. Note Selection + Extra Gamma penalization
    d.gam <- bam( 
      as.formula(model_name), 
      weights = weights, 
      gamma = 5,
      select = T, 
      discrete = T, 
      data = D )
    #summary(d.gam)
      
      
    # =============================================================
    # Save Model Summaries  
    # =============================================================
   
    save(d.gam, file = here("Results", spp_name, "GAM_outputs", paste0(spp_name, "_gam_fold",iii_fold,".RData"))) # save model object
    
    #### Save F value, p value, and R2 from each fold
    d.sum <- summary(d.gam)
    d.add <- as.data.frame(d.sum$s.table) %>% select(-c(edf, Ref.df)) %>%
      rownames_to_column("predictor") %>%
      t() %>%
      as.data.frame() %>%
      rownames_to_column("Metric") %>%
      mutate(Fold = rep(iii_fold), .before = Metric) %>%
      slice(-1)
    names(d.add) <- names(gam.summaries)
    gam.summaries <- rbind(gam.summaries, d.add)
      
    r.sq[iii_fold] <- d.sum$r.sq
    
    
    #### Predictor Effect Estimates (type="iterms")
    pred  <- predict.gam(
      d.gam, 
      type = "iterms", se.fit = TRUE) 
    
    ## Save out predictor effect estimates for 1-D marginal effect plots
    d.effect_plotting <- cbind(data.frame(ROW_NUM = D$ROW_NUM), data.frame(pred)) # marginal effects and SE for this fold
    d.effect_plotting <- d.effect_plotting %>%
      mutate(Fold = iii_fold, .before = ROW_NUM)
    
    if(iii_fold == 1){
      effect_plotting <- d.effect_plotting
      rm(d.effect_plotting)
    } else{
      effect_plotting <- rbind(effect_plotting, d.effect_plotting)
    }
      
      
      
    # =============================================================
    # Predict to 2021 land cover surface
    # =============================================================
    
    #### Generate predictions and export landcover data formatted for ROI
    ## Subset srd21 to the same rows used to fit the GAM
    lc21 <- srd21 %>% select(ROW_NUM, Barren.PLAND:Herbaceous.Wetlands.PLAND)
    D21 <- D %>% select(response:elev, weights) %>% left_join(lc21, by = "ROW_NUM")
    D21 <- D21[names(D)]
    
    ## Predict the GAM to the 2021 land covers
    pred21 <- predict.gam(d.gam, newdata = D21, type = "iterms", se.fit = TRUE) 
    
    ## Export 2007 and 2021 landcover datasets used in the model
    if(iii_fold == 1){
      D21_output <- D21 %>% select(-c(response,weights))
      D_output <- D %>% select(-c(response,weights))
      write.csv(D21_output, here("Results", spp_name, paste0(spp_name,"_2021_landcovers.csv")), row.names = F)
      write.csv(D_output, here("Results", spp_name, paste0(spp_name,"_2007_landcovers.csv")), row.names = F)
    } 
      
  
    
    # ===================================================================================
    # Produce datasets for interpretive plotting aids: Geographic effects
    # ===================================================================================
    
    #### Create containers to hold plotting results
    save.geo <- cbind("longitude" = D21$longitude, "latitude" = D21$latitude, "ROW_NUM" = D21$ROW_NUM)
    save.geo.abd <- cbind("longitude" = D21$longitude, "latitude" = D21$latitude, "ROW_NUM" = D21$ROW_NUM)
    
    #### Initiate loop through all predictors
    for(iii_effect in 1:length(colnames(pred21$fit))){
     zzz_name <- names(d.gam$var.summary)[iii_effect]
       
     if (zzz_name != "longitude" & zzz_name != "elev"){
       ## Geographic PPY effect dataset
       zzz <- pred21$fit[, iii_effect]
       zzz_lim <- c(-1*max(abs(zzz)), max(abs(zzz)))
       save.geo <- cbind(save.geo, zzz)
       colnames(save.geo)[iii_effect+3] <- zzz_name
         
       ## Geographic abundance-weighted PPY effect dataset
       # Zero out areas w/o feature of interest so that importance is not summarized there
       zero_feature <- D21[, names(D21) == zzz_name ] 
       zero_feature[zero_feature!=0] <- 1
       zzz_max_pred <- max(abs(quantile(pred21$fit*abd_spp, probs = c(0.99), na.rm = T)))
       zzz_lim_abdchange <- c(-1*zzz_max_pred, zzz_max_pred)            # Settings for outputs to aid plotting
       
       zzz <- pred21$fit[,iii_effect] * D21$abd * zero_feature
       zzz[zzz < -1*zzz_max_pred] <- -1*zzz_max_pred
       zzz[zzz > zzz_max_pred] <- zzz_max_pred
       save.geo.abd <- cbind(save.geo.abd, zzz)
       colnames(save.geo.abd)[iii_effect+3] <- zzz_name
     } 
    } # Close loop through Predictors
      
      
    #### Bind the results from this fold to the main dataset
    save.geo <- as.data.frame(save.geo)
    save.geo.abd <- as.data.frame(save.geo.abd)
    save.geo$fold <- rep(iii_fold, length.out=nrow(save.geo))
    save.geo.abd$fold <- rep(iii_fold, length.out=nrow(save.geo.abd))
    
    if(iii_fold == 1){
      save.geo.folds <- save.geo
      save.geo.abd.folds <- save.geo.abd
    } else{
      save.geo.folds <- rbind(save.geo.folds, save.geo)
      save.geo.abd.folds <- rbind(save.geo.abd.folds, save.geo.abd)
    }
  
     
   } # Close loop through 100 folds
  
  
  #### Export datasets to directory
  write.csv(save.geo.folds, here("Results", spp_name, 
                                 paste0(spp_name,"_geo.effect_plotting.csv")), row.names = F)
  write.csv(save.geo.abd.folds, here("Results", spp_name, 
                                     paste0(spp_name,"_geo.abd.effect_plotting.csv")), row.names = F)
  write.csv(gam.summaries, here("Results", spp_name, 
                                paste0(spp_name,"_GAM_summaries.csv")), row.names = F)
  write.csv(as.data.frame(r.sq), here("Results", spp_name, 
                                paste0(spp_name,"_R2.csv")), row.names = F)
  write.csv(effect_plotting, here("Results", spp_name, 
                                  paste0(spp_name,"_1Deffect_plotting.csv")), row.names = F)
  rm(save.geo.folds, save.geo.abd.folds, save.geo, save.geo.abd, r.sq)
  
} # Close loop through Sagebrush species


toc()   # end timer
