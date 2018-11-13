## =========================================================
# MotoVison 2.0 - multi-classification models
# Version: 1.1
# Author: Payam Mokhtarian
# Update: 2018-11-06
# Note: Pretarined network from ImageNet
## =========================================================

RequiredPkg <- c("tensorflow","reticulate","keras")
LoadPkg <- suppressPackageStartupMessages(lapply(RequiredPkg, require, character.only = TRUE))

# Python madule adjustment
PIL <- import("PIL")
PIL$ImageFile$LOAD_TRUNCATED_IMAGES <- TRUE

##----------------- Setup
label_list <- list.files(path = "~/MotoMe Projects/Keras_CNN/data/training")

# Number of output classes
output_n <- length(label_list)

# Image size to scale down to
img_width <- 224
img_height <- 224
target_size <- c(img_width, img_height)

# Batch size
batch_size_input <- 20
# RGB = 3 channels
channels <- 3
# epochs
epochs <- 35 # 25
# Steps per epoch
steps_per_epoch_input <- 150 # 100
# Validation steps
validation_steps_input <- 150 # 100
# Optimizer error
optimizer_error <- 2e-5

# Path to image folders
train_image_files_path <- "~/MotoMe Projects/Keras_CNN/data/training/"
valid_image_files_path <- "~/MotoMe Projects/Keras_CNN/data/validation/"

##----------------- Pretrained models
# VGG19 full
full_conv_base_vgg19 <- application_vgg19(weights = "imagenet",
                                          include_top = FALSE,
                                          input_shape =  c(img_width, img_height, channels))
summary(full_conv_base_vgg19)
# ResNet50
# full_conv_base_resnet <- application_resnet50(include_top = TRUE, 
#                                               input_tensor = NULL,  
#                                               pooling = NULL)
# summary(full_conv_base_resnet)
# Pre-trained feature model
full_conv_base <- full_conv_base_vgg19 # or full_conv_base_resnet or full_conv_base_vgg19
full_conv_base_name <- paste0("motome_vgg_based_",gsub("-","",Sys.Date())) # or motome_resnet_based or motome_vgg_based

##----------------- 
# Feature extraction
motovision_model <- keras_model_sequential() %>% 
  full_conv_base %>% 
  layer_flatten() %>% 
  layer_dense(units = 256, activation = "relu") %>% 
  layer_dense(units = output_n, activation = "sigmoid")
summary(motovision_model)
length(motovision_model$trainable_weights)

# Freez network
freeze_weights(full_conv_base)
length(motovision_model$trainable_weights)

# Data augmentation
train_datagen = image_data_generator(
  rescale = 1/255
  # rotation_range = 40,
  # width_shift_range = 0.2,
  # height_shift_range = 0.2,
  # shear_range = 0.2,
  # zoom_range = 0.2,
  # horizontal_flip = TRUE,
  # fill_mode = "nearest"
)

# Note that the validation data shouldn't be augmented!
valid_datagen <- image_data_generator(rescale = 1/255) 

train_generator <- flow_images_from_directory(
  train_image_files_path,     # Target directory  
  train_datagen,              # Data generator
  target_size = target_size,  # Resizes all images to 224 × 224
  batch_size = batch_size_input,
  class_mode = "categorical"  # binary_crossentropy loss for binary labels
)

validation_generator <- flow_images_from_directory(
  valid_image_files_path,
  valid_datagen,
  target_size = target_size,
  batch_size = batch_size_input,
  class_mode = "categorical"
)

motovision_model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(lr = optimizer_error),
  metrics = c("accuracy")
)

motovision_history <- motovision_model %>% fit_generator(
  train_generator,
  steps_per_epoch = steps_per_epoch_input,
  epochs = epochs,
  validation_data = validation_generator,
  validation_steps = validation_steps_input,
  verbose = 1,
  callbacks = list(
    callback_tensorboard(log_dir = "~/MotoMe Projects/VRUM_MotoVision_CNN/logs")
  )
)

## Save motovision model
save_model_hdf5(object = motovision_model, 
                filepath = paste0("~/MotoMe Projects/Keras_CNN/saved_model/",full_conv_base_name,".h5"), 
                overwrite = TRUE,
                include_optimizer = TRUE)

## Training results
jpeg(paste0("~/MotoMe Projects/VRUM_MotoVision_CNN/plot/motovision_history_",gsub("-","",Sys.Date()),".jpg"), 
     width = 800, 
     height = 500)
plot(motovision_history)
dev.off()

## Model training logs
tensorboard("~/MotoMe Projects/VRUM_MotoVision_CNN/logs")
sessionInfo()

## Fine tuning
# Unfreez weights
# unfreeze_weights(full_conv_base, from = "block3_conv1")
# 
# # re-train
# motovision_model %>% compile(
#   loss = "categorical_crossentropy",
#   optimizer = optimizer_rmsprop(lr = optimizer_error),
#   metrics = c("accuracy")
# )
# 
# motovision_history <- motovision_model %>% fit_generator(
#   train_generator,
#   steps_per_epoch = steps_per_epoch_input,
#   epochs = epochs,
#   validation_data = validation_generator,
#   validation_steps = validation_steps_input
# )

## Unit testing
# Load motovision model
motovision_model <- load_model_hdf5(filepath = paste0("~/MotoMe Projects/Keras_CNN/saved_model/",full_conv_base_name,".h5"),
                         custom_objects = NULL,
                         compile = TRUE)
# Testing
test_image_files_path <- "~/MotoMe Projects/Keras_CNN/data/testing/"
test_datagen <- image_data_generator(rescale = 1/255) 
test_generator <- flow_images_from_directory(
  test_image_files_path,
  test_datagen,
  target_size = target_size,
  batch_size = batch_size_input,
  class_mode = "categorical"
)

# Prediction
motovision_prediction <- motovision_model %>% predict_generator(test_generator, 
                                                                step = steps_per_epoch_input, 
                                                                verbose = 1)
