# -------------------------------------------------------------------------
# PRAKTIKUM MINGGU 11 ANALISIS SPASIAL LINGKUNGAN 
# belajarR-09: R Modelling 
# title: Landcover Classification in R
# source: https://github.com/HannaMeyer/Geostat2018/
# data: https://drive.google.com/drive/u/1/folders/1TYcV31iBJs0oCD42xzmey3CzD43T1HUJ
# modified script by Ziyadatul Hikmah | 07 Nov 2023
# --------------------

# install and load packages -----------------------------------------------
# libraries we need
libs <- c("sf","raster","caret","tidyverse")

# install packages
installed_libs <- libs %in% row.names(
  install.packages()
)

if(any(installed_libs == F)) {
  install.packages(
    libs[!installed_libs]
  )
}

# load libraries 
invisible(lapply(libs, library, character.only = T))

# set working directory ---------------------------------------------------

# load and explore the data -----------------------------------------------
# import data
sentinel <- stack("sentinel2017.grd")
training <- read_sf("trainingSites.shp")

# extract raster information ----------------------------------------------
extract_raster <- function(){
  # extract raster menjadi data frame yang dioverlay dengan data training 
  extr <- terra::extract(
    sentinel, training,
    df = T
  )
  # merge data hasil extract dengan informasi kelas tutupan lahan di data training
  extr_df <- merge(
    extr, training,
    by.x = "ID", 
    by.y = "id"
  )
  
  return(extr_df)
}

extr_data <- extract_raster()

# split data  -------------------------------------------------------------
set.seed(100)
# membagi data menjadi data training (70%) dan data validasi (30%)
trainingSamples <- caret::createDataPartition(
    extr_data$Class, 
    p = 0.7,
    list = F
  )

# data training 
trainData <- extr_data[trainingSamples,]

# data validasi
testData <- extr_data[-trainingSamples,]

# visualize relationships -------------------------------------------------
# untuk membandingkan tingkat yellowness dengan kelas tutupan lahan
boxplot(trainData$yellowness~trainData$Class, las = 2)

# untuk melihat korelasi antra
featurePlot(x = trainData[, c("B03","B04","B08","yellowness")], 
            y = factor(trainData$Class), 
            plot = "pairs",
            auto.key = list(columns = 4))

# random forest model -----------------------------------------------------
# define variabel menjadi variabel prediktor dan response
predictors <- c("B02","B03","B04","B05","B06","B07",
                "B08","B8A","yellowness","NDVI")
response <- "Class"

# model 
set.seed(100)
model_RF <- caret::train(
  trainData[,predictors],
  trainData[,response],
  method = "rf",
  trControl = caret::trainControl(method = "cv"),
  importance = T
)

print(model_RF)

# plot hasil modeling
plot(model_RF)

# plot variabel importance
plot(varImp(model_RF))

# model prediction --------------------------------------------------------
# prediksi tutupan lahan berdasarkan model RF
predict_RF <- predict(sentinel, model_RF)

# save raster landcover hasil modeling
writeRaster(predict_RF,
            filename = "landcover_classification.tif")

# model validation --------------------------------------------------------
# gunakan data validasi (30%) untuk membuat model validation
validasi <- predict(model_RF,testData)
table(testData$Class, validasi)

# done ! ------------------------------------------------------------------


