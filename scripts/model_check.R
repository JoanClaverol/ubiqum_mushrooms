
# libraries ----
library(tidyverse)
library(caret)

# import ------
test <- read_rds(path = "data/test_img.rds")
test$class <- as.factor(test$class)

# checking the model ----

# load the model 
load("C:/Users/romcl/Desktop/Flo&JuliAlwaysChachara.rda")
student_model <- RFmodel2

# checking the results
test$predictions <- predict(object = student_model, test)
postResample(test$class, test$predictions)

# dead or not
{
  number_deads <- 0
  for (i in 1:nrow(test)) {
    if (test$class[i] == "p" & test$predictions[i] == "e") {
      number_deads <- number_deads + 1
    }
  }
    
  if (number_deads != 0) {
    print(paste("You died",number_deads,"times :("))
  } else {
    print("You survived!")
  }
}

# consfusion matrix 
confusionMatrix(data = as.factor(test$predictions), 
                reference = as.factor(test$class))

