## =========================================================
# MotoVision 2.0 classifier
# Version: 1.1
# Author: Payam Mokhtarian
# Update: 2018-11-14
# Note: 
## =========================================================

##----------------- Libraries
RequiredPkg <- c("RPostgreSQL","jsonlite","RCurl","crayon","tensorflow","reticulate","keras")
LoadPkg <- suppressPackageStartupMessages(lapply(RequiredPkg, require, character.only = TRUE))

# Python madule adjustment
PIL <- import("PIL")
PIL$ImageFile$LOAD_TRUNCATED_IMAGES <- TRUE

##----------------- Connection
args <- c("vrum","Blast2market!")
AWSPSQLConnect <- dbConnect(
  dbDriver("PostgreSQL"),
  host = 'vrum-mm-dev.cq7vqkl7mcwj.ap-southeast-2.rds.amazonaws.com',
  dbname = 'vrum_mm_dev',
  user = args[1],
  password = args[2]
)

##----------------- Setup and functions
label_list <- list.files(path = "~/MotoMe Projects/Keras_CNN/data/training")

# Define image preprocessor for use with keras vgg19
image_preprocessor = function(image_path) {
  image = image_load(image_path, target_size = c(224,224))
  image = image_to_array(image)
  image = array_reshape(image, c(1, dim(image)))
  image = imagenet_preprocess_input(image)
  return(image)
}

# Function to read in files housing all cat/dog breeds in imagenet labels
read_dog_cat_labels = function(path) {
  labs = readLines(path)
  labs = trimws(unlist(strsplit(labs, ',')))
  labs = gsub('\\s+', '_', labs)
  return(labs)
}

# Prefix path
prefix_path <- "https://s3-ap-southeast-2.amazonaws.com/core-network-prod-syd-s3-s3bucket-1i4yt4k3a3znc/public/vrum/motome/prod-s3-caradvice-images/6/prodS3CaradviceImages/image/original/"
# Download image function
motovision_download <- function(imageurl) {
  image_name <- gsub(pattern = "https://s3-ap-southeast-2.amazonaws.com/core-network-prod-syd-s3-s3bucket-1i4yt4k3a3znc/public/vrum/motome/prod-s3-caradvice-images/6/prodS3CaradviceImages/image/original/",
                     replacement = "", imageurl)
  download.file(url = imageurl, destfile = paste0('data/new_image/',image_name), quiet = FALSE)
}

# Load MotoVision 2.0 model
full_conv_base_name <- "motome_vgg_based_20181109" # or motome_resnet_based
MotoVision2 <- load_model_hdf5(filepath = paste0("~/MotoMe Projects/Keras_CNN/saved_model/",full_conv_base_name,".h5"),
                               custom_objects = NULL,
                               compile = TRUE)

# MotoVition2 prediction function
pred.motovision <- function(model, label_list, image_list, image_url) {
  labelPreds <- data.frame(predict(model, image_list))
  names(labelPreds) <- label_list
  labelPreds$imageurl <- image_url
  list(labelPreds = labelPreds)
}

##----------------- Download images on local disk
# Load image urls
cars_query <- paste0("SELECT DISTINCT make, family FROM new_glasses_vrum_data ORDER BY make, family ASC")
cars_data <- dbGetQuery(AWSPSQLConnect, cars_query)

# Start processing
MotoVisionDataLabels <- NULL
for(i in 1:nrow(cars_data)) {
  cat(paste0(i, " out of ", nrow(cars_data), "\n"))
  images_query <- paste0("SELECT make, family, imageurl FROM image_classified_data 
                        WHERE make = '",cars_data$make[i],"' AND family = '",cars_data$family[i],"' 
                         AND daystamp <= '",Sys.Date(),"' ORDER BY natural_order ASC")
  image_url_data <- dbGetQuery(AWSPSQLConnect, images_query)
  
  # Download images
  cat(yellow(paste0("Start downloading ... \n")))
  str_dl <- Sys.time()
  lapply(image_url_data$imageurl, motovision_download)
  download_time <- Sys.time()-str_dl
  cat(green(paste0("Images have been downloaded. ",download_time," \n")))
  ##----------------- Local image pre-processing
  # Define image paths to classify
  image_paths = list.files('data/new_image', recursive = TRUE, full.names = TRUE)
  # Preprocess images
  cat(yellow(paste0("Start image pre-processing ... \n")))
  str_image_proc <- Sys.time()
  image_list = lapply(image_paths, image_preprocessor)
  image_proc_time <- Sys.time()-str_image_proc
  cat(green(paste0("Image processing has been compeleted. ",image_proc_time," \n")))
  # Predict labels
  cat(yellow(paste0("Start predicting ... \n")))
  str_motovision <- Sys.time()
  MotoVisionData <- NULL
  for(j in 1:length(image_list)) {
    MotoVisionData_temp <- NULL
    tryCatch({
      output.pred.motovision <- pred.motovision(model = MotoVision2,
                                                label_list = label_list,
                                                image_list = image_list[j], 
                                                image_url = paste0(prefix_path,gsub(pattern = "data/new_image/", replacement = "", image_paths[j])))
      MotoVisionData_temp <- output.pred.motovision$labelPreds
    }, error = function(e) {})
    MotoVisionData <- rbind(MotoVisionData,MotoVisionData_temp)
  }
  motovision_time <- Sys.time()-str_motovision
  cat(green(paste0("MotoVision image recognition has been done. ",motovision_time," \n")))
  # Delete files
  do.call(file.remove, list(list.files('data/new_image', full.names = TRUE)))
  # Save data
  if(!is.null(MotoVisionData)) {
    MotoVisionData$make <- cars_data$make[i]
    MotoVisionData$family <- cars_data$family[i]
  }
  MotoVisionDataLabels <- rbind(MotoVisionDataLabels, MotoVisionData)
}

# Probability data
motovision_prob_data <- reshape(MotoVisionDataLabels,
                                direction = "long", 
                                varying = list(names(MotoVisionDataLabels)[1:length(label_list)]), 
                                v.names = "probability", 
                                idvar = c("imageurl","make","family" ), 
                                timevar = "class", times = names(MotoVisionDataLabels)[1:length(label_list)])
row.names(motovision_prob_data) <- NULL
motovision_prob_data <- motovision_prob_data[motovision_prob_data$probability >= 0.75,]
motovision_prob_data <- motovision_prob_data[with(motovision_prob_data, order(imageurl)),]

##----------------- Write into DB
# Row data
dbSendQuery(AWSPSQLConnect,"DROP TABLE IF EXISTS motovision_class_data")
dbWriteTable(AWSPSQLConnect,"motovision_class_data", MotoVisionDataLabels, row.names = FALSE)
# Transformd data
dbSendQuery(AWSPSQLConnect,"DROP TABLE IF EXISTS motovision_prob_data")
dbWriteTable(AWSPSQLConnect,"motovision_prob_data", motovision_prob_data, row.names = FALSE)
