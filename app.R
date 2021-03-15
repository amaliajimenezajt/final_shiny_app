#install.packages("devtools")
#install.packages("usethis")
#install.packages("reticulate")
#install.packages("tensorflow")
#install.packages("keras")
# reticulate::py_install("kaggle")
# install_tensorflow(method = 'conda', envname = 'r-reticulate')


library(devtools)
library(kaggler)
library(reticulate)
library(keras)
library(tensorflow)
library(reticulate)

######### UPLOAD THE CSV DATA

kaggle <- import("kaggle")
kaggle$api$authenticate()
kaggle$api$dataset_download_files("sakshigoyal7/credit-card-customers", "BankChurners.csv", unzip = T)

#########   Read the csv data

data <- read.csv("BankChurners.csv/BankChurners.csv",sep=",")


######## OBSERVE IF THERE ARE NA'S

sum(is.na(data))  

######### THERE IS NO NA

########## CLEAN THE DATA 

row.names(data)=data[,1]
data = data[,-1]
colnames(data)
table(data$Income_Category)
