# import test
test <- readRDS(file = "data/test.rds")

# define collected variables with images
image_var <- c("cap.shape","cap.color",
               "stalk.color.above.ring",
               "stalk.color.below.ring",
               "class","bruises","population")

# defining the test
test_img <- test[image_var]

# saving the model
saveRDS(test_img, file = "data/test_img.rds")
