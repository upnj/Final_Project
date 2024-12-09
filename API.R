# API.R

# Load required libraries
library(plumber)
library(randomForest)
library(ggplot2)

# Read in your data
diabetes_data <- read.csv("diabetes_binary_health_indicators_BRFSS2015.csv")

# Define the model
tree_model <- decision_tree(
  mode = "classification",
  cost_complexity = 0.001,  # Replace with the best cost_complexity value from your tuning results
  tree_depth = 5            # Replace with the best tree_depth value from your tuning results
) %>%
  set_mode("classification") %>%
  set_engine("rpart")

# Define the recipe
tree_recipe <- recipe(Diabetes_binary ~ HighBP + HighChol + PhysActivity + Sex  + GenHlth +                         HvyAlcoholConsump + MentHlth + PhysHlth , data = diabetes_data) %>%
  step_dummy(all_nominal_predictors())

# Define the workflow
tree_workflow <- workflow() %>%
  add_model(tree_model) %>%
  add_recipe(tree_recipe)

# Fit the 'best' model to the entire data set
best_model <- tree_workflow %>%
  fit(data = diabetes_data)


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
  
  prediction <- predict(best_model, new_data = input_data, type = "prob")$.pred_1
  
  return(list(diabetes_risk = prediction))
}

# Example function calls:
# http://localhost:8000/pred?HighBP=1&HighChol=1&PhysActivity=0&Sex=1&GenHlth=4&HvyAlcoholConsump=0&MentHlth=10&PhysHlth=15
# http://localhost:8000/pred?HighBP=0&HighChol=0&PhysActivity=1&Sex=0&GenHlth=2&HvyAlcoholConsump=0&MentHlth=2&PhysHlth=0
# http://localhost:8000/pred

#* Model information
#* @get /info
function() {
  info <- list(
    name = "Upendra Joshi",
    url = "https://upnj.github.io/Final_Project/"
  )
  
  return(info)
}

#* Confusion matrix plot
#* @serializer png
#* @get /confusion
function() {
  predictions <- predict(best_model, new_data = diabetes_data) %>% 
    bind_cols(diabetes_data %>% select(Diabetes_binary))
  
  confusion_matrix <- table(predictions$.pred_class, predictions$Diabetes_binary)
  
  plot <- ggplot(data = as.data.frame(confusion_matrix), aes(x = Var1, y = Var2, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq), color = "white", size = 8) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(title = "Confusion Matrix", x = "Predicted", y = "Actual") +
    theme_minimal()
  
  return(plot)
}