perform_ml_modelling <- function(data_train, type) {
  
      # Input:   train data set, and type of algorithm: rf, glmnet, svm
      # Process: perform ML modelling based on train set and type of algorithm using 10-fold cross-validation
      #          with preprocessing within the folds and hyperparameter tuning. Models are optimized for ROC metric
      # Output:  final model
  
      # Set cross-validation parameters
      fitControl  <- trainControl(method='cv', number =10,                       # Apply 10-fold cross-validation
                                  classProbs=T,                                  # Set parameters for classification problem and save predictions e.g. for ROC curve
                                  summaryFunction = twoClassSummary, 
                                  savePredictions = T)
      
      # Train model using caret package
  
      if (type == 'rf') {
        
          # 1. Random forest
          model_rf  <- train(training_type ~ ., data = data_train,
                          method = 'rf',                                         # Use random forest algorithm
                          trControl = fitControl,              
                          verbose = F,              
                          tuneLength = 10,              
                          preProcess = c("center","scale"),                      # Perform preprocessing within nested cross-validation
                          metric = 'ROC')                                        # Optimise for ROC metric
          
          # Output model
          return(model_rf)
          
      } else if (type == 'glmnet') {
        
          # 2. Glmnet              
          model_glm  <- train(training_type ~ ., data = data_train,              
                          method = 'glmnet',                                     # Use elastic net algorithm
                          trControl = fitControl,              
                          verbose = F,              
                          family = 'binomial',              
                          tuneLength = 10,              
                          preProcess = c("center","scale"),              
                          metric = 'ROC')  
          
          # Output model
          return(model_glm)
          
      } else if (type == 'svm') {   
        
          # 3. SVM              
          model_svm  <- train(training_type ~ ., data = data_train,              
                          method = 'svmLinear',                                  # Use support vector machine (linear) algorithm
                          trControl = fitControl,              
                          verbose = F,              
                          family = 'binomial',
                          tuneLength = 10,
                          preProcess = c("center","scale"),
                          metric = 'ROC')
          
          # Output model
          return(model_svm)
      }
      
}