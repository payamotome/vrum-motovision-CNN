##------------ Libraries
RequiredPkg <- c("RPostgreSQL","jsonlite","RCurl","crayon")
LoadPkg <- suppressPackageStartupMessages(lapply(RequiredPkg, require, character.only = TRUE))

##------------ Connections      
# VRUM Postgres AWS database
AWSPSQLConnect <- dbConnect(
  dbDriver("PostgreSQL"),
  host = 'vrum-mm-dev.cq7vqkl7mcwj.ap-southeast-2.rds.amazonaws.com',
  dbname = 'vrum_mm_dev',
  user = Sys.getenv("database_userid"),
  password = Sys.getenv("database_password")
)

##---------- Function: MotoMe endpoints
VRUM.API <- function(controler, par = list(...)) {
  options(warn = -1)
  # Details
  if(controler == "details") {
    recAPI <- getForm(paste0("https://www.motome.com.au/api/services/cars/details?activeOnly=false"),
                      httpheader = c('Accept' = 'application/json', Accept = 'application/json'))
    APIdata <- data.frame(fromJSON(recAPI))
  }
  # Make
  if(controler == "make") {
    recAPI <- getForm(paste0("https://www.motome.com.au/api/services/cars/details/",par$make,"?activeOnly=false"),
                      httpheader = c('Accept' = 'application/json', Accept = 'application/json'))
    APIdata <- fromJSON(recAPI[1])
  }
  # Model
  if(controler == "model") {
    recAPI <- getForm(paste0("https://www.motome.com.au/api/services/cars/details/",par$make,"/",par$model,"?activeOnly=false"),
                      httpheader = c('Accept' = 'application/json', Accept = 'application/json'))
    APIdata <- fromJSON(recAPI[1])
  }
  # API Output
  list(controler = controler, parameter = unlist(par), APIout = APIdata)
}

##------------ Downloader function
image.downloder <- function(input_url) {
  file_name <- gsub(pattern = "https://s3-ap-southeast-2.amazonaws.com/core-network-prod-syd-s3-s3bucket-1i4yt4k3a3znc/public/vrum/motome/prod-s3-caradvice-images/6/prodS3CaradviceImages/image/original/",
                    replacement = "", input_url)
  tryCatch({
    download.file(input_url, paste0("~/MotoMe Projects/VRUM_MotoVision_CNN/",file_name))
  }, error = function(e) {})
}

##------------ Load image url
# Call all make and family of the most newest cars given MotoMe details and make/model end-points
make_data <- VRUM.API(controler = "details", par = list())
new_make <- make_data$APIout[make_data$APIout$isActive,]$name
all_new_car_data <- NULL
for(k in 1:length(new_make)) {
  cat(yellow(paste0("Retrive all models of ",new_make[k]," - ",k," out of ",length(new_make),"\n")))
  model_data <- VRUM.API(controler = "make", par = list(make = gsub(" ", "%20", new_make[k])))
  new_model <- model_data$APIout$models$name[model_data$APIout$models$isActive]
  if(length(new_model) > 0) {
    new_car_data <- data.frame("make" = rep(new_make[k],length(new_model)), "family" = new_model)
  }
  if(length(new_model) == 0) {
    new_car_data <- NULL
  }
  all_new_car_data <- rbind(all_new_car_data,new_car_data)
}
all_new_car_data$make <- as.character(all_new_car_data$make)
all_new_car_data$family <- as.character(all_new_car_data$family)
input.data <- all_new_car_data[all_new_car_data$family != "4/4",]
# Call image controller API
# input.data <- input.data[1:100,]
image_data <- NULL
for(i in 1:nrow(input.data)) {
  cat(yellow$bold(paste0("Hit image GET end-point for ", input.data$make[i], " ", input.data$family[i], " - car ", i, " out of ", nrow(input.data), " \n")))
  input_make <- gsub(" ","%20",input.data$make[i])
  input_model <- gsub(" ","%20",input.data$family[i])
  input_model <- gsub("/","%2F",input_model)
  image_url_new_data <- getForm(paste0("https://www.motome.com.au/api/services/cars/images/",input_make,"/",input_model), 
                                httpheader = c('Accept' = 'application/json', Accept = 'application/json'))
  image_data <- rbind(image_data, data.frame(fromJSON(image_url_new_data)))
}

# image_data$url[6029]
##------------ Download images
lapply(image_data$url, image.downloder)
