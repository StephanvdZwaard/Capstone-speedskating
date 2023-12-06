# ------------------------------------------------------------------------------------------------------------------------ #
#                           Script for performing Capstone project for Data Science course                                 #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Perform capstone project to predict intensive and extensive interval training sessions in speed skating    #
# Authors:      Stephan van der Zwaard [stephan_vanderzwaard@live.nl]                                                      #
# Date:         02-12-2023                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    4.3.2 (2023-10-31) 
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #


# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Settings & dependencies                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #

    # ------------------------------------------------------
    # Install and import libraries
    # ------------------------------------------------------   
    
      #Install packages if not already installed
      if (!require(anytime))   install.packages('anytime')
      if (!require(caret))     install.packages('caret')
      if (!require(lubridate)) install.packages('lubridate')
      if (!require(zoo))       install.packages('zoo')
      if (!require(dplyr))     install.packages('dplyr')
      if (!require(glmnet))    install.packages('glmnet')
      if (!require(kernlab))   install.packages('kernlab')
      if (!require(MLeval))    install.packages('MLeval')
      if (!require(rmarkdown)) install.packages('rmarkdown')
      if (!require(knitr))     install.packages('knitr')
      if (!require(stringr))   install.packages('stringr')
      if (!require(tidyr))     install.packages('tidyr')
      if (!require(randomForest)) install.packages('randomForest')

      #Import libraries 
      library(anytime)
      library(caret)
      library(lubridate)
      library(zoo)
      library(dplyr)
      library(glmnet)
      library(kernlab)
      library(MLeval)      
      library(rmarkdown)
      library(knitr)
      library(stringr)
      library(tidyr)      
      library(tidyverse)
      library(randomForest)
      
    #library(purrr)
    ##library(readr)
    #library(readxl)

    # ------------------------------------------------------
    # Set options
    # ------------------------------------------------------   
    
      # set options
      options(stringsAsFactors = F)
    
    # ------------------------------------------------------
    # Load helper scripts
    # ------------------------------------------------------   

      source('./capstone/scripts/preprocess_data_general.R')                      # Helper function to pre-process general data
      source('./capstone/scripts/preprocess_data_raw.R')                          # Helper function to pre-process raw data
      source('./capstone/scripts/feature_engineering.R')                          # Helper function to engineer features from the raw data (without contamination between train and test sets)
      source('./capstone/scripts/perform_ml_modelling.R')                         # Helper function to perform machine learning modelling
      source('./capstone/scripts/eval_ml_modelling.R')                            # Helper function to evaluate model performance
    
    
# ------------------------------------------------------------------------------------------------------------------------ #
#                         Classifying intensive vs extensive interval training sessions in speed skating                   #                                        #
# ------------------------------------------------------------------------------------------------------------------------ #

    # ------------------------------------------------------
    # Load data (and some preliminary inspection)
    # ------------------------------------------------------   
    

      # Load general speed skating data for caption project
      data       <- read.csv('./capstone/data/data_speedskating.csv')
      
      # Load detailed speed skating data for internal training load (heart rate)
      data_i_raw <- read.csv('./capstone/data/data_speedskating_internal_raw.csv')
      
      # Load detailed speed skating data for external training load (speed)
      data_e_raw <- read.csv('./capstone/data/data_speedskating_external_raw.csv')
      
      
    # ------------------------------------------------------
    # Data pre-processing 
    # ------------------------------------------------------   
    
      # Perform preliminary pre-processing for general data 
      data       <- preprocess_data_general(data)
      
      # Perform preliminary pre-processing for raw data 
      data_i_raw <- preprocess_data_raw(data_i_raw, data, 'internal')
      data_e_raw <- preprocess_data_raw(data_e_raw, data, 'external')
      
      
    # ------------------------------------------------------
    # Data partitioning
    # ------------------------------------------------------   
      
      # Data partitioning: split data into train and holdout test set
      set.seed(123)
      trainIndex      <- createDataPartition(data$training_type, times=1, p = .65, list=F)
      data_train      <- data[trainIndex,]
      data_test       <- data[-trainIndex,]
      
    # ------------------------------------------------------
    # Data exploration | Exploratory data analysis
    # ------------------------------------------------------ 

        # Inspect structure and important summary statistics.
        str(data)
        summary(data)
        
        #Check for missing data in each column
        colSums(is.na(data))
        
        # Check number of speed skaters
        unique(data$skater_id) %>% length() %>% print()
        
        # Check sex of speed skaters
        data %>% group_by(skater_id) %>% filter(row_number()==1) %>% 
                 group_by(gender) %>% summarise(n=n())
        
        # Data visualization: plot collected data throughout the season for each speedskater
        ggplot(data, aes(x = skater_id, y = date)) + 
          geom_point(alpha =.5) +
          ggtitle("Data collection of training sessions") +
          scale_x_continuous(breaks=seq(1,21,1)) +
          theme_classic() + 
          theme(plot.title   = element_text(size=14, face="bold"),
                axis.title.y = element_blank(),
                axis.title.x = element_text(size=12, face="bold"),
                axis.text    = element_text(size=10, face="bold"))
        
        # Check intensive and extensive interval training sessions
        data %>% group_by(training_type) %>% summarise(n=n())
        
        # Inspect maximal heart rate from the raw data
        data_i_raw %>% group_by(skater_id,date,session) %>% summarise(HRmax = max(HR)) %>% 
          ggplot(aes(x=factor(skater_id),y=HRmax)) + geom_boxplot() + 
          xlab('skater_id') + ggtitle('Maximal heart rate during training sessions across all speed skaters') +
          theme_bw()
        
        # Inspect acceleration from the raw data: are values below the maximal human acceleration of ~10m/s2?
        ggplot(data_e_raw,aes(x=1,y=acceleration)) + geom_boxplot() + geom_hline(yintercept=10, color='red') + theme_bw()
        
        # Inspect duration between loops from the raw data: is duration not longer than the time it takes to mop the ice rink (~20 minutes)?
        ggplot(data_e_raw,aes(x=1,y=duration)) + geom_boxplot() + geom_hline(yintercept=1200, color='red') + theme_bw()
        
        # Inspect heart rate during intensive and extensive interval sessions
        data_i_raw %>% filter(skater_id==5, date %in% c('2018-12-07','2018-12-11')) %>% 
          ggplot(aes(x=time,y=HR)) + geom_line() + facet_wrap(~training_type) +
          xlab('time [s]') + ggtitle('Example of heart rate during extensive and intensive training sessions') +
          theme_bw()
        
        # Inspect speed during intensive and extensive interval sessions
        data_e_raw %>% filter(skater_id==5, date %in% c('2018-12-07','2018-12-11')) %>% 
          ggplot(aes(x=time,y=speed)) + geom_line() + facet_wrap(~training_type) +
          xlab('time [s]') + ggtitle('Example of speed during extensive and intensive training sessions') +
          theme_bw()
        
        #Check for distribution of heart rate on sessions in training data: reveals some differences
        ggplot(data_i_raw %>% filter(!is.na(training_type)) %>% filter(train_id %in% data_train$train_id), 
               aes(x=training_type,y=HR_rel, fill=training_type)) + geom_violin() + xlab('') + ylab('Heart rate (% of maximal HR)') + theme_bw() + ggtitle('Heart rate per training type')
        
        #Check for distribution of speed on sessions in training data: reveals some differences
        ggplot(data_e_raw %>% filter(!is.na(training_type)) %>% filter(train_id %in% data_train$train_id), 
               aes(x=training_type,y=speed_rel, fill=training_type)) + geom_violin() + xlab('') + ylab('Speed (% of maximal speed') + theme_bw() + ggtitle('Speed per training type')
        ggplot(data_e_raw %>% filter(!is.na(training_type)) %>% filter(train_id %in% data_train$train_id), 
               aes(x=speed_rel, color=training_type)) + geom_density(bw=2) + theme_bw() + ggtitle('Check for distribution of speed')

        #Check for distribution of acceleration: rather similar between training types
        ggplot(data_e_raw %>% filter(!is.na(training_type)) %>% filter(train_id %in% data_train$train_id), 
               aes(x=acceleration, color=training_type)) + geom_density(bw=.00001) + scale_x_continuous(limits=c(-2,2))+ theme_bw() + ggtitle('Check for distribution of acceleration')
        
    
    # ------------------------------------------------------
    # Feature engineering
    # ------------------------------------------------------ 
        
        # Perform feature engineering on train set
        data_train <- perform_feature_engineering(data_i_raw, data_e_raw, data_train)
          
        # Perform feature engineering separately on final validation set
        data_test <- perform_feature_engineering(data_i_raw, data_e_raw, data_test)
        
    # ------------------------------------------------------
    # Modelling  
    # ------------------------------------------------------ 
          
        # Perform ML modelling machine learning models based on imbalanced classes
        model_def_1 <- perform_ml_modelling(data_train, 'rf')
        model_def_2 <- perform_ml_modelling(data_train, 'glmnet')
        model_def_3 <- perform_ml_modelling(data_train, 'svm')
        
        # Evaluate machine learning models based on their ROC-curve and AUC
        res <- evalm(list(model_def_1,model_def_2,model_def_3),
                     c('RandomForest','Glmnet','SVM'), title = 'ROC plot based on imbalanced classes')
        res$roc
        
        # Note that upsampling techniques may be applied on the minority class to improve training of the ML models 
        
        # Upsample train set to accomodate for class imbalance
        data_train_upsample <- upSample(data_train %>% select(-training_type),
                                        data_train %>% pull(training_type) %>% as.factor()) %>% 
                               rename(training_type = Class)
        
        # Perform ML modelling machine learning models based on equal classes
        model_ups_1 <- perform_ml_modelling(data_train_upsample, 'rf')
        model_ups_3 <- perform_ml_modelling(data_train_upsample, 'svm')
        model_ups_2 <- perform_ml_modelling(data_train_upsample, 'glmnet')
        
        # Evaluate machine learning models based on their ROC-curve and AUC
        res <- evalm(list(model_ups_1,model_ups_2,model_ups_3), 
                     c('RandomForest','Glmnet','SVM'), title = 'ROC plot after upsampling to equal classes')
        res$roc
        

    # ------------------------------------------------------
    # Model performance evaluation 
    # ------------------------------------------------------ 
        
        # Obtain final results on unseen test set for each of the models (both before and after upsampling the minority class)
        results <- rbind('model_rf_def'  = eval_ml_modelling(model_def_1, data_test, 'metric'),
                         'model_glm_def' = eval_ml_modelling(model_def_2, data_test, 'metric'),
                         'model_svm_def' = eval_ml_modelling(model_def_3, data_test, 'metric'),
                         'model_rf_ups'  = eval_ml_modelling(model_ups_1, data_test, 'metric'),
                         'model_glm_ups' = eval_ml_modelling(model_ups_2, data_test, 'metric'),
                         'model_svm_ups' = eval_ml_modelling(model_ups_3, data_test, 'metric'))
        results <- as.data.frame(results) %>% rownames_to_column() %>%
                   mutate(data = ifelse(str_detect(rowname,'def'),'default','upsampled'),
                          model = gsub('(.*)_(.*)_(.*)','\\2',rowname)) %>%
                   select(model,data,F1,`Balanced Accuracy`,Sensitivity,Specificity)
        
   # ------------------------------------------------------
   # Best model evaluation 
   # ------------------------------------------------------ 
        
        # Obtain confusion matrix from best performing model
        eval_ml_modelling(model_ups_3, data_test, 'cmatrix') 
        
        # Best performing model based on balanced accuracy is model3 after upsampling. 
        # Here I provide the top features based on feature importance scores:
        feature_importance <- varImp(model_ups_3)
        ggplot(feature_importance, top=25) + ggtitle('Feature importance plot') + theme_bw()

        
# -------------------------------------------------------------------------------------------------------
#                                               END OF SYNTAX 
# ------------------------------------------------------------------------------------------------------- 
