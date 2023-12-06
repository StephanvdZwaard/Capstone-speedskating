perform_feature_engineering <- function(data_i_raw, data_e_raw, data_general) {
  
      # Input:   Raw dataset with data from internal or external load
      #          General dataset
      #          Type of raw data: internal or external
      # Process: Perform feature engineering based on raw data
      # Output:  General dataset with added features
      

        # Feature engineering on external training load
        features_external <- data_e_raw %>% 
          
                             # Obtain only data from records within train / test set.
                             inner_join(data_general %>% select(train_id), by='train_id') %>% 
          
                             # Perform calculations by training session 
                             group_by(train_id) %>%
          
                             # Add information on turns and straights on the ice rink (grouped by training session)
                             mutate(segment = case_when(loop_id_end == 4 & lag(loop_id_end,3) == 1 ~ 'turn1',
                                                        loop_id_end == 9 & lag(loop_id_end,3) == 6 ~ 'turn2',
                                                        loop_id_end == 1 & lag(loop_id_end,4) == 9 ~ 'straight1',
                                                        loop_id_end == 6 & lag(loop_id_end,2) == 4 ~ 'straight2',
                                                        TRUE ~ '')) %>% 
          
                             # Determine average speed over all loops within turn (~83 meter) / straight (~110 meter) and convert from m/s to km/h
                             mutate(ice_turn1_speed     = 83.265/rollapply(data = duration, width = 3, FUN = sum, align = 'right', fill = NA, na.rm=T),
                                    ice_turn2_speed     = 83.265/rollapply(data = duration, width = 3, FUN = sum, align = 'right', fill = NA, na.rm=T),
                                    ice_straight1_speed = 110.45/rollapply(data = duration, width = 4, FUN = sum, align = 'right', fill = NA, na.rm=T),
                                    ice_straight2_speed = 110.45/rollapply(data = duration, width = 2, FUN = sum, align = 'right', fill = NA, na.rm=T)) %>% 
                             mutate(ice_turn1_speed     = ifelse(segment == 'turn1',    ice_turn1_speed*3.6,NA),
                                    ice_turn2_speed     = ifelse(segment == 'turn2',    ice_turn2_speed*3.6,NA),
                                    ice_straight1_speed = ifelse(segment == 'straight1',ice_straight1_speed*3.6,NA),
                                    ice_straight2_speed = ifelse(segment == 'straight2',ice_straight2_speed*3.6,NA)) %>% 
                        
                             # Determine if passing is within certain speeds windows (% of max speed)
                             mutate(speed_15_20         = ifelse(between(speed_rel,15,20),1,0),
                                    speed_20_25         = ifelse(between(speed_rel,20,25),1,0),
                                    speed_25_30         = ifelse(between(speed_rel,25,30),1,0),
                                    speed_70_75         = ifelse(between(speed_rel,70,75),1,0),
                                    speed_75_80         = ifelse(between(speed_rel,75,80),1,0),
                                    speed_80_85         = ifelse(between(speed_rel,80,85),1,0),
                                    speed_85_90         = ifelse(between(speed_rel,85,90),1,0),
                                    speed_90_95         = ifelse(between(speed_rel,90,95),1,0),
                                    speed_95_100        = ifelse(speed_rel>95,1,0)) %>%
                                    
                             # Add newly engineered features per training session
                             summarise(skater_id               = skater_id[1],
                                       ice_mean_speed          = mean(speed,na.rm=T),
                                       ice_mean_rel_speed      = mean(speed_rel,na.rm=T),
                                       ice_median_speed        = median(speed,na.rm=T),
                                       ice_iqr_speed.          = IQR(speed,na.rm=T),
                                       ice_sd_speed            = sd(speed,na.rm=T),
                                       
                                       ice_95p_speed_segment   = quantile(speed, probs=.95,na.rm=T),
                                       ice_90p_speed_segment   = quantile(speed, probs=.90,na.rm=T),
                                       ice_75p_speed_segment   = quantile(speed, probs=.75,na.rm=T),
                                       ice_25p_speed_segment   = quantile(speed, probs=.25,na.rm=T),
                                       
                                       ice_n_20p_maxspeed      = sum(speed_15_20,na.rm=T),
                                       ice_n_25p_maxspeed      = sum(speed_20_25,na.rm=T),
                                       ice_n_30p_maxspeed      = sum(speed_25_30,na.rm=T),
                                       ice_n_75p_maxspeed      = sum(speed_70_75,na.rm=T),
                                       ice_n_80p_maxspeed      = sum(speed_75_80,na.rm=T),
                                       ice_n_85p_maxspeed      = sum(speed_80_85,na.rm=T),
                                       ice_n_90p_maxspeed      = sum(speed_85_90,na.rm=T),
                                       ice_n_95p_maxspeed      = sum(speed_90_95,na.rm=T),
                                       ice_n_100p_maxspeed     = sum(speed_95_100,na.rm=T),
        
                                       ice_max_speed_segment   = max(speed,na.rm=T),
                                       ice_max_accel_segment   = quantile(acceleration,prob=.99,na.rm=T),
                                       ice_fastest_turn1       = quantile(ice_turn1_speed,prob=.999,na.rm=T),
                                       ice_fastest_turn2       = quantile(ice_turn2_speed,prob=.999,na.rm=T),
                                       ice_fastest_straight1   = quantile(ice_straight1_speed,prob=.999,na.rm=T),
                                       ice_fastest_straight2   = quantile(ice_straight2_speed,prob=.999,na.rm=T)) %>%
                                       ungroup() %>%
          
                                # Impute missings by skater mean.
                                group_by(skater_id) %>%
                                mutate(ice_fastest_turn1       = ifelse(is.na(ice_fastest_turn1),mean(ice_fastest_turn1,na.rm=T),ice_fastest_turn1),
                                       ice_fastest_straight1   = ifelse(is.na(ice_fastest_straight1),mean(ice_fastest_straight1,na.rm=T),ice_fastest_straight1),
                                       ice_fastest_straight2   = ifelse(is.na(ice_fastest_straight2),mean(ice_fastest_straight2,na.rm=T),ice_fastest_straight2)) %>%
                                ungroup() %>%
                                select(-skater_id)
          
          # Feature engineering on internal training load
          features_internal <- data_i_raw %>% 
            
                                # Obtain only data from records within train / test set.
                                inner_join(data_general %>% select(train_id), by='train_id') %>% 
                                
                                # Perform calculations by training session 
                                group_by(train_id) %>%

                                # Calculate rolling averages for maximal heart rate and determine failure of obtaining heart rate (value = 0)
                                mutate(HR_zero            = ifelse(HR==0,1,0),
                                       HR                 = ifelse(HR==0,NA,HR),
                                       HR.15avg           = rollapply(data = HR, width = 15, FUN = mean, align = "right", fill = NA, na.rm = T),
                                       HR.05avg           = rollapply(data = HR, width = 5,  FUN = mean, align = "right", fill = NA, na.rm = T)) %>%
            
                                # Determine if passing is within certain speeds windows (% of max speed)
                                mutate(HR_50_55         = ifelse(between(HR_rel,50,55),1,0),
                                       HR_55_60         = ifelse(between(HR_rel,55,60),1,0),
                                       HR_60_65         = ifelse(between(HR_rel,60,65),1,0),
                                       HR_65_70         = ifelse(between(HR_rel,65,70),1,0),
                                       HR_70_75         = ifelse(between(HR_rel,70,75),1,0),
                                       HR_75_80         = ifelse(between(HR_rel,75,80),1,0),
                                       HR_80_85         = ifelse(between(HR_rel,80,85),1,0),
                                       HR_85_90         = ifelse(between(HR_rel,85,90),1,0),
                                       HR_90_95         = ifelse(between(HR_rel,90,95),1,0),
                                       HR_95_100        = ifelse(HR_rel>95,1,0)) %>%
            
                                # Add newly engineered features per training session
                                summarise(HR_max.01s      = max(HR,na.rm=T),
                                          HR_max.05s      = round(max(HR.05avg,na.rm=T)),
                                          HR_max.15s      = round(max(HR.15avg,na.rm=T)),
                                          HR_mean         = round(mean(HR,na.rm=T)),
                                          HR_median       = median(HR,na.rm=T),
                                          HR_sd           = round(sd(HR,na.rm=T)),
                                          HR_iqr          = IQR(HR,na.rm=T),
                                          
                                          HR_95p          = quantile(HR,probs=.95,na.rm=T),
                                          HR_90p          = quantile(HR,probs=.90,na.rm=T),
                                          HR_75p          = quantile(HR,probs=.75,na.rm=T),
                                          HR_25p          = quantile(HR,probs=.25,na.rm=T),
                                          
                                          n_55p_maxHR     = sum(HR_50_55,na.rm=T),
                                          n_60p_maxHR     = sum(HR_55_60,na.rm=T),
                                          n_65p_maxHR     = sum(HR_60_65,na.rm=T),
                                          n_70p_maxHR     = sum(HR_65_70,na.rm=T),
                                          n_75p_maxHR     = sum(HR_70_75,na.rm=T),
                                          n_80p_maxHR     = sum(HR_75_80,na.rm=T),
                                          n_85p_maxHR     = sum(HR_80_85,na.rm=T),
                                          n_90p_maxHR     = sum(HR_85_90,na.rm=T),
                                          n_95p_maxHR     = sum(HR_90_95,na.rm=T),
                                          n_100p_maxHR    = sum(HR_95_100,na.rm=T),
                                          
                                          HR_zeros        = round(sum(HR_zero)/length(HR_zero),digits=3)) %>%
                                ungroup() 
            
 
  # Combine features with general dataset
  data_engineered <- data_general %>%
                     left_join(features_external, by='train_id') %>%
                     left_join(features_internal, by='train_id') %>%
          
                     # Unname named numerics from quantile functions
                     mutate_at(.vars = vars(contains('p_speed_segment'),contains('turn'),contains('straight'),ice_max_accel_segment,HR_95p,HR_90p,HR_75p,HR_25p),
                               .funs = list(~unname(.))) %>%

                     # Remove character and date columns as well as identifying columns before modelling
                     select(-c(date, contains('start'),contains('id'))) %>%
    
                     # Reorder columns
                     select(training_type, year:session, contains('ice'), contains('HR'),everything())
          
  # Output
  return(data_engineered)
  
}
