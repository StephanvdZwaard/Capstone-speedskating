eval_ml_modelling <- function(model, data_test, type) {
  
      # Input:   test data set, model and output type: confusion matrix (cmatrix) or metrics (metric)
      # Process: predict class (intensive or extensive interval training) based on selected model and 
      #          generate confusion matrix
      # Output:  model performance characteristics
  
      # Predict classes from test set
      predicted <- predict(model, newdata = data_test)
      
      # Create confusion matrix based on predictions and actual classes
      cm <- confusionMatrix(data = predicted, reference = data_test$training_type)
      
      # Provide relevant outcome metrics of model performance
      
      if (type == 'cmatrix') {
        
        output <- cm$table
        
      } else if (type == 'metric') {
        
        output <- cm$byClass
        
      }
      
      # Output model
      return(output)
}