#install.packages("devtools")
#install.packages("usethis")
#install.packages("reticulate")
#install.packages("tensorflow")
#install.packages("keras")
reticulate::py_install("kaggle")


install_tensorflow(method = 'conda', envname = 'r-reticulate')library(devtools)
library(kaggler)
library(reticulate)
library(keras)
library(tensorflow)
install_tensorflow()

library(reticulate)
py_install("pandas")

devtools::install_git("https://github.com/bernardo-dauria/kaggler.git")

kaggler::kgl_auth(username="amaliajimenezt",key="8e177825f7c983276080202675cc94f6")


kgl_datasets_list(search = "sakshigoyal7/credit-card-customers")
bank <- kgl_datasets_download(owner_dataset = "sakshigoyal7/credit-card-customers", 
                               fileName = "BankChurners.csv")


kaggle <- import("kaggle")
kaggle$api$authenticate()
kaggle$api$dataset_download_files("sakshigoyal7/credit-card-customers", "BankChurners.csv", unzip = T)





sakshigoyal7/credit-card-customers

kgl_datasets_list(search = "sakshigoyal7/credit-card-customers")
bank <- kgl_datasets_download(owner_dataset = "sakshigoyal7/credit-card-customers",
                              fileName = "BankChurners.csv",
                              datasetVersionNumber=1)

summary(bank)


bank <- kgl_datasets_download(owner_dataset = "amaliajimenezt/bank-churners", 
                              fileName = "bank_data_churners.csv")

