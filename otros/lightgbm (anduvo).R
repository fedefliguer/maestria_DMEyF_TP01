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


# Alternatively, you can put in dense matrix, i.e. basic R-matrix
print("Training lightgbm with Matrix")
bst <- lightgbm(
  data = as.matrix(train$data)
  , label = train$label
  , num_leaves = 4L
  , learning_rate = 1.0
  , nrounds = 2L
  , objective = "binary"
)

# You can also put in lgb.Dataset object, which stores label, data and other meta datas needed for advanced features
print("Training lightgbm with lgb.Dataset")
dtrain <- lgb.Dataset(
  data = train$data
  , label = train$label
)
bst <- lightgbm(
  data = dtrain
  , num_leaves = 4L
  , learning_rate = 1.0
  , nrounds = 2L
  , objective = "binary"
)

# Verbose = 0,1,2
print("Train lightgbm with verbose 0, no message")
bst <- lightgbm(
  data = dtrain
  , num_leaves = 4L
  , learning_rate = 1.0
  , nrounds = 2L
  , objective = "binary"
  , verbose = 0L
)

print("Train lightgbm with verbose 1, print evaluation metric")
bst <- lightgbm(
  data = dtrain
  , num_leaves = 4L
  , learning_rate = 1.0
  , nrounds = 2L
  , nthread = 2L
  , objective = "binary"
  , verbose = 1L
)

print("Train lightgbm with verbose 2, also print information about tree")
bst <- lightgbm(
  data = dtrain
  , num_leaves = 4L
  , learning_rate = 1.0
  , nrounds = 2L
  , nthread = 2L
  , objective = "binary"
  , verbose = 2L
)

# You can also specify data as file path to a LibSVM/TCV/CSV format input
# Since we do not have this file with us, the following line is just for illustration
# bst <- lightgbm(
#     data = "agaricus.train.svm"
#     , num_leaves = 4L
#     , learning_rate = 1.0
#     , nrounds = 2L
#     , objective = "binary"
# )

#--------------------Basic prediction using lightgbm--------------
# You can do prediction using the following line
# You can put in Matrix, sparseMatrix, or lgb.Dataset
pred <- predict(bst, test$data)
err <- mean(as.numeric(pred > 0.5) != test$label)
print(paste("test-error=", err))
