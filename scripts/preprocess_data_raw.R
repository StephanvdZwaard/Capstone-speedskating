preprocess_data_raw <- function(data_raw, data_general, type) {
  
      # Input:   Raw dataset with data from internal or external load
      #          General dataset
      #          Type of raw data: internal or external
      # Process: Perform some preprocessing and add relevant info from the general dataset
      # Output:  Processed dataset
      
      # ---- Raw data ---------
  
        data_raw <- data_raw %>%  
                  
                  # Convert date into date format 
                    mutate(date  = as.Date(date)) %>%        

                  # Add information on training session to raw datasets (from general dataset)
                    left_join(data_general %>% select(date,skater_id,session,train_id,training_type),
                              by = c('date', 'session', 'skater_id')) %>%
                    
                  # Add time information (in sec) per session
                    group_by(train_id) %>%
                    mutate(time  = seconds(hms(time))) %>%
                    mutate(time  = time-time[1]) %>%
                    ungroup() 
          
        if (type == 'external') {
          
          data_raw <- data_raw %>%
            
                      # Add information on relative speed
                      mutate(speed_rel = speed / max_speed *100) %>%
          
                      # Remove outliers / unrealistic values
                      mutate(acceleration = ifelse(acceleration<10,acceleration, NA), #remove data points above the expected maximal human acceleration of 10m/s^2 
                             duration     = ifelse(duration < 1200,duration, NA))     #remove data points above the expected maximal delay of mopping the ice rink (20 min) 
          
        } else if (type == 'internal') {
          
          data_raw <- data_raw %>%
                      mutate(HR_rel = HR / max_HR *100)
          
        }

  # Output
  return(data_raw)
  
}
