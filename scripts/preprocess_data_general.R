preprocess_data_general <- function(data_general) {
  
  # Input:   General dataset with overview of all training sessions
  # Process: Perform some preprocessing and data cleaning and a bit of feature engineering on the general dataset
  # Output:  Processed dataset
  
  # ---- General data ---------
  
  data_general <- data_general %>%
                  
                  # Add identifier for training session
                  mutate(train_id        = row_number()) %>% 
    
                  # Add information on date
                  mutate(date            = as.Date(date)) %>%
                  mutate(season          = ifelse(date < as.Date("2019-07-01"),'2018/2019','2019/2020'),
                         daypart         = ifelse(hour(hms(ice_start))<12,'am','pm')) %>%
                             
                  # Process the target variable training type into factor
                  mutate(training_type   = case_when(training_type == 'Extensive interval'  ~ 'Ext_Interval',
                                                     training_type == 'Intensive interval'  ~ 'Int_Interval')) %>%
                  mutate(training_type   = factor(training_type, levels = c('Ext_Interval','Int_Interval'))) %>%

                  # Convert categorical variables into binary features
                  mutate(season          = ifelse(season=="2018/2019",0,1),
                         daypart         = ifelse(daypart=="am",0,1),
                         gender          = ifelse(gender=="M",1,0),
                         skater_type     = ifelse(skater_type=='Sprint',1,0)) %>%
  
                  mutate(# Convert into numeric
                        ice_nr_laps      = as.numeric(ice_nr_laps),
                        ice_duration     = as.numeric(ice_duration),
                    
                         # Add information about date and time
                         year            = strftime(date,"%Y"),
                         month           = strftime(date,"%m"),
                         day             = strftime(date,"%d"),
                         weekday         = strftime(date,"%u"),
                         yearday         = strftime(date,"%j"),
                         hour            = hour(hms(ice_start))) %>% 
    
                  # Add information on training impulse derived from HR zones and percentage HR zones
                  mutate(TRIMP_edwards   = 1*HR_z1_min + 2*HR_z2_min + 3*HR_z3_min + 4*HR_z4_min + 5*HR_z5_min,
                         HR_z1_perc      = HR_z1_min / HR_duration,
                         HR_z2_perc      = HR_z2_min / HR_duration,
                         HR_z3_perc      = HR_z3_min / HR_duration,
                         HR_z4_perc      = HR_z4_min / HR_duration,
                         HR_z5_perc      = HR_z5_min / HR_duration) %>%
  
                  mutate_at(.vars = vars(year:yearday), .funs=list(~as.numeric(.))) %>%
    
                  select(train_id,date,year,season,month,day,weekday,yearday,daypart,hour,everything())
  
  # Output
  return(data_general)
  
}