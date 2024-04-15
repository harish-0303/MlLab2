# Load necessary libraries
library(caret)
library(rpart)
library(rpart.plot)  # for visualization

# Set working directory (replace with your actual data path)
setwd("C:/Users/MSI/OneDrive/Desktop/ML/")  # Adjust as needed

# Read data from CSV file
data <- read.csv("oulad-assessments.csv")

# Handle missing values (consider appropriate methods based on data)
data <- na.omit(data)  # Simple removal; explore alternatives if needed

# Convert 'score' to a factor with appropriate levels
data$score <- as.factor(data$score)

# Split data into training and testing sets (assuming 'score' is the target)
set.seed(123)  # For reproducibility
training_indices <- createDataPartition(data$score, p = 0.8, list = FALSE)
training_set <- data[training_indices, ]
testing_set <- data[-training_indices, ]

# Train the decision tree model
model <- rpart(score ~ ., data = training_set)

# Make predictions on the testing set
predictions <- predict(model, testing_set, type = "class")  # Predict class labels

# Evaluate the model's performance
confusionMatrix(predictions, testing_set$score)

# Visualize the decision tree
rpart.plot(model,
           main = "Decision Tree for Student Assessment",
           extra = 4,  # Display class distribution at leaves
           type = 2,   # Label all nodes
           fallen.leaves = TRUE,  # Add fallen leaves
           shadow.col = "gray",  # Add shadow to nodes
           box.palette = c("lightblue", "lightcoral"),  # Custom color scheme
           cex = 0.8,
           branch.lty = 3,  # Set branch line style
           branch.lwd = 2,  # Set branch line width
           under = TRUE)  # Draw the branch under the node
