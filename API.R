# API.R

# Load required libraries
library(plumber)
library(randomForest)
library(ggplot2)
library(rpart)
library(tidymodels)
library(dplyr)
library(yardstick)



if(!file.exists("diabetes_binary_health_indicators_BRFSS2015.csv")) {
  stop("Data file not found")
}

# Read in your data
diabetes_data <- read.csv("diabetes_binary_health_indicators_BRFSS2015.csv")


# Add print statements to verify data loading
print(sprintf("Loaded %d rows of data", nrow(diabetes_data)))
print(sprintf("Number of diabetic cases: %d", 
              sum(diabetes_data$Diabetes_binary == 1)))

# Convert Diabetes_binary to a factor
diabetes_data$Diabetes_binary <- as.factor(diabetes_data$Diabetes_binary)

# Create train/test split
set.seed(123)
data_split <- initial_split(diabetes_data, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

# Define the model
tree_model <- decision_tree(
  mode = "classification",
  cost_complexity = 0.0001,  # Replace with the best cost_complexity value from your tuning results
  tree_depth = 10            # Replace with the best tree_depth value from your tuning results
) %>%
  set_mode("classification") %>%
  set_engine("rpart")

# Define the recipe
tree_recipe <- recipe(Diabetes_binary ~ HighBP + HighChol + PhysActivity + Sex  + GenHlth +                         HvyAlcoholConsump + MentHlth + PhysHlth , 
                      data = train_data) %>%
  step_dummy(all_nominal_predictors())

# Define the workflow
tree_workflow <- workflow() %>%
  add_model(tree_model) %>%
  add_recipe(tree_recipe)

# Fit the 'best' model to the entire data set
best_model <- tree_workflow %>%
  fit(data = diabetes_data)

# After model fitting, add verification
print("Model fitting completed")
print(best_model)

prediction_history <- data.frame()

#* @apiTitle Diabetes Prediction API
#* @apiDescription API endpoints for diabetes prediction and model information.

#* Predict diabetes risk
#* @param HighBP High blood pressure (0 = No, 1 = Yes)
#* @param HighChol High cholesterol (0 = No, 1 = Yes)
#* @param PhysActivity Physical activity (0 = No, 1 = Yes)
#* @param Sex Gender (0 = Female, 1 = Male)
#* @param GenHlth General health (1 = Excellent, 2 = Very Good, 3 = Good, 4 = Fair, 5 = Poor)
#* @param HvyAlcoholConsump Heavy alcohol consumption (0 = No, 1 = Yes)
#* @param MentHlth Number of days with poor mental health in the past 30 days
#* @param PhysHlth Number of days with poor physical health in the past 30 days
#* @get /pred
function(HighBP = 0, HighChol = 0, PhysActivity = 1, Sex = 0, GenHlth = 3, HvyAlcoholConsump = 0, MentHlth = 0, PhysHlth = 0) {
  input_data <- data.frame(
    HighBP = as.integer(HighBP),
    HighChol = as.integer(HighChol),
    PhysActivity = as.integer(PhysActivity),
    Sex = as.integer(Sex),
    GenHlth = as.integer(GenHlth),
    HvyAlcoholConsump = as.integer(HvyAlcoholConsump),
    MentHlth = as.integer(MentHlth),
    PhysHlth = as.integer(PhysHlth)
  )
  
  # Get prediction
  prediction <- predict(best_model, new_data = input_data, type = "prob")$.pred_1
  
  # Store prediction and input data
  prediction_record <- cbind(input_data, 
                             Diabetes_binary = as.factor(ifelse(prediction > 0.5, 1, 0)))
  prediction_history <<- rbind(prediction_history, prediction_record)
  
  return(list(diabetes_risk = prediction))
}
# Example function calls:
# http://localhost:8000/pred?HighBP=1&HighChol=1&PhysActivity=0&Sex=1&GenHlth=4&HvyAlcoholConsump=0&MentHlth=10&PhysHlth=15
# http://localhost:8000/pred?HighBP=0&HighChol=0&PhysActivity=1&Sex=0&GenHlth=2&HvyAlcoholConsump=0&MentHlth=2&PhysHlth=0
# http://localhost:8000/pred

#* Model information
#* Model information
#* @get /info
function() {
  # Ensure the model is loaded and accessible
  if (!exists("best_model")) {
    return(list(error = "Model not loaded"))
  }
  
  # Return model specifications
  info <- list(
    model_type = "Decision Tree Classification Model",
    features = names(diabetes_data)[!names(diabetes_data) %in% "Diabetes_binary"],
    data_info = list(
      total_samples = nrow(diabetes_data),
      features_count = ncol(diabetes_data) - 1
    ),
    hyperparameters = list(
      cost_complexity = 0.001,
      tree_depth = 5
    ),
    timestamp = as.character(Sys.time())
  )
  
  return(info)
}
#* @get /confusion
#* @serializer png
function() {
  # Use both original data and prediction history
  all_predictions <- predict(best_model, new_data = rbind(diabetes_data[names(prediction_history)], 
                                                          prediction_history))
  
  actual_values <- c(diabetes_data$Diabetes_binary, 
                     prediction_history$Diabetes_binary)
  
  # Create fresh confusion matrix
  cm <- table(
    Predicted = all_predictions$.pred_class, 
    Actual = actual_values
  )
  
  # Set up the plot
  par(mar = c(6, 6, 4, 4))
  
  # Create the plot with red to blue gradient
  image(1:2, 1:2, 
        t(cm),
        col = colorRampPalette(c("#D73027", "#4575B4"))(100),
        xlab = "Predicted",
        ylab = "Actual",
        main = sprintf("Confusion Matrix\n(Including %d new predictions)", 
                       nrow(prediction_history)),
        axes = FALSE)
  
  # Add custom axes
  axis(1, at = c(1, 2), labels = c("0", "1"))
  axis(2, at = c(1, 2), labels = c("0", "1"))
  
  # Add text for the counts and percentages
  total <- sum(cm)
  for(i in 1:2) {
    for(j in 1:2) {
      text(i, j, 
           sprintf("n = %d\n%.1f%%", 
                   cm[i,j], 
                   100 * cm[i,j]/total),
           col = "white",
           cex = 1.5)
    }
  }
  
  # Add a box around the plot
  box()
  
  # Print statistics to console
  print(sprintf("Total predictions in history: %d", nrow(prediction_history)))
  print("Current Confusion Matrix:")
  print(cm)
}
# At the end of your script
#pr <- plumber::plumb("API.R")
#pr$run(port=8000)