library(lightgbm)
library(methods)

# We load in the agaricus dataset
# In this example, we are aiming to predict whether a mushroom is edible
train <- agaricus.train
test <- agaricus.test
dtrain <- lgb.Dataset(data = train$data, label = train$label, free_raw_data = FALSE)
dtest <- lgb.Dataset.create.valid(dtrain, data = test$data, label = test$label)
valids <- list(train = dtrain, test = dtest)

# To train with valids, use lgb.train, which contains more advanced features
# valids allows us to monitor the evaluation result on all data in the list
print("Train lightgbm using lgb.train with valids")
bst <- lgb.train(
  data = dtrain
  , num_leaves = 4L
  , learning_rate = 1.0
  , nrounds = 2L
  , valids = valids
  , nthread = 2L
  , objective = "binary"
)
