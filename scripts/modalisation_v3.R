# libraries ---------------------------------------------------------------
if (require(pacman) == FALSE) {
  install.packages("pacman")
}
pacman::p_load(tidyverse, caret, modelr, readr, gmodels, rattle, rpart, ROCR)

# load data ---------------------------------------------------------------
data <- read_rds("data/train.rds")

# pre process -------------------------------------------------------------

# variable extraction
data$gill.attachment <- NULL
data$veil.type <- NULL

# feature selection
image_var <- c("cap.shape","cap.color",
               "stalk.color.above.ring",
               "stalk.color.below.ring",
               "class","bruises","population")
data <- data[,image_var]

# exluding duplicated observations
data <- unique(data)

# is the data balanced between p and e?
prop.table(table(data$class))

# duplicating poisonous mushrooms
mush_poison <- data %>% 
  filter(class == "p")
data_balance <- data %>% bind_rows(mush_poison) 

# model -------------------------------------------------------------------

# train and test
set.seed(123)
train_id <- createDataPartition(
  y = data_balance$class,
  p = 0.75, 
  list = F
)
train <- data_balance[train_id,]
test  <- data_balance[-train_id,]

# cross validation
control_method <-"repeatedcv"
control_folds <- 10
control_repeats <- 1
fitControl <- trainControl(method = control_method,
                           number = control_folds,
                           repeats = control_repeats,
                           classProbs = TRUE,
                           verboseIter = FALSE,
                           )

#set training parameters
train_method <- "C5.0"
train_metric <- "ROC"
train_tuneLength <- 15

# model 
set.seed(123)
mod_1 <- train(class ~ .,
               data = train,
               method = train_method,
               metric = train_metric,
               tuneLength = train_tuneLength,
               trControl = fitControl)

# main predictors
# bruises > cap color > cap shape > stalk color above > stalk color below > population 

# error check -------------------------------------------------------------

confusionMatrix(predict(mod_1, test), as.factor(test$class),
                dnn = c("Prediction", "Reference"))

# 1. add errors on my set ----------------------------------------------------

# find the errors
mod_errors <- data_balance %>% 
  add_predictions(model = mod_1, var = "pred") %>% 
  filter(class != pred & class == "p")

# adding the errors in our data
data_balance_err <- data_balance %>% 
  bind_rows(mod_errors) %>% select(-pred)

deads <- 1
for (i in 1:15) {
  if (deads != 0) {
    # creating the model 
    set.seed(123)
    train_id <- createDataPartition(
      y = data_balance_err$class,
      p = 0.75, 
      list = F
    )
    train_err <- data_balance_err[train_id,]
    test_err  <- data_balance_err[-train_id,]
    
    #set training parameters
    train_method <- "C5.0"
    train_metric <- "ROC"
    train_tuneLength <- 15
    
    # model 
    set.seed(123)
    mod_2 <- train(class ~ .,
                   data = train_err,
                   method = train_method,
                   metric = train_metric,
                   tuneLength = train_tuneLength,
                   trControl = fitControl)
    
    # find the errors
    mod_errors_loop <- data_balance_err %>% 
      add_predictions(model = mod_2, var = "pred") %>% 
      filter(class != pred & class == "p")
    
    # adding the errors in our data
    data_balance_err <- data_balance_err %>% 
      bind_rows(mod_errors_loop) %>% select(-pred)
    deads <- nrow(mod_errors_loop)
    
    # confusion matrix
    print(
    confusionMatrix(predict(mod_2, test_err), as.factor(test_err$class),
                    dnn = c("Prediction", "Reference"))
    )
  }
}

# saving the model
write_rds(mod_2, path = "models/model_iterations.rds")
