library(lightgbm)
library(methods)

# We load in the agaricus dataset
# In this example, we are aiming to predict whether a mushroom is edible
data(agaricus.train, package = "lightgbm")
data(agaricus.test, package = "lightgbm")
train <- agaricus.train
test <- agaricus.test

# The loaded data is stored in sparseMatrix, and label is a numeric vector in {0,1}
class(train$label)
class(train$data)

#--------------------Basic Training using lightgbm----------------
# This is the basic usage of lightgbm you can put matrix in data field
# Note: we are putting in sparse matrix here, lightgbm naturally handles sparse input
# Use sparse matrix when your feature is sparse (e.g. when you are using one-hot encoding vector)

print("Training lightgbm with sparseMatrix")
bst <- lightgbm(
  data = train$data
  , label = train$label
  , num_leaves = 4L
  , learning_rate = 1.0
  , nrounds = 2L
  , objective = "binary"
)
