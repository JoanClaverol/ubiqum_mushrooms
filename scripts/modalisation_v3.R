# libraries ---------------------------------------------------------------
if (require(pacman) == FALSE) {
  install.packages("pacman")
}
pacman::p_load(tidyverse, caret, modelr, readr, gmodels, rattle, rpart, ROCR,
               gridExtra)

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

# balance of the dataset
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
train_method <- "rpart"
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
index <- 1
errors_df <- tibble()

while (deads != 0) {
    
    # model creation 
    set.seed(123)
    train_id <- createDataPartition(
      y = data_balance_err$class,
      p = 0.75, 
      list = F
    )
    train_err <- data_balance_err[train_id,]
    test_err  <- data_balance_err[-train_id,]
    
    #set training parameters
    train_method <- "rpart"
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
      bind_rows(mod_errors_loop) %>% 
      select(-pred)
    deads <- nrow(mod_errors_loop)
    
    # confusion matrix
    print(
    confusionMatrix(predict(mod_2, test_err), as.factor(test_err$class),
                    dnn = c("Prediction", "Reference"))
    )
    
    errors_df <- errors_df %>% 
      bind_rows(mod_errors_loop %>% 
                  mutate(index = index))
    
    # visualize the errors
    # for (i in 1:length(image_var)) {
    #   temp_errors <- mod_errors_loop %>% 
    #     gather(key = "var" , value = "values", image_var) %>% 
    #     filter(var == image_var[i])
    #   
    #   assign(
    #     paste0("p", i), 
    #     ggplot(data = temp_errors) + 
    #       geom_bar(aes(x = values)) + labs(title = image_var[i]))
    # }
    # print(grid.arrange(p1, p2, p3, p4, p6, p7, 
    #              top = paste("Number of deads",deads)))
    index = index + 1
}

# saving the model
write_rds(mod_2, path = "models/model_iterations.rds")



# visalizations -----------------------------------------------------------
library(gganimate)
gganimate <- errors_df %>%
  pivot_longer(cols = c(-bruises, -pred, -class, -index), 
               names_to = "names", values_to = "values") %>% 
  ggplot(aes(x = values)) +
    geom_bar(fill = "dodgerblue4") +
    facet_wrap(.~names) +
    theme_bw()+ 
    transition_time(index) +
    ease_aes(default = "linear")
animate(gganimate)


library(gapminder)
p <- ggplot(
  gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
animate(p)
