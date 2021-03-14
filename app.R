install.packages("devtools")
install.packages("usethis")

library(devtools)
library(kaggler)

devtools::install_git("https://github.com/bernardo-dauria/kaggler.git")

kaggler::kgl_auth(username="amaliajimenezt",key="8e177825f7c983276080202675cc94f6")


kgl_datasets_list(search = "sakshigoyal7/credit-card-customers")
bank <- kgl_datasets_download(owner_dataset = "sakshigoyal7/credit-card-customers", 
                               fileName = "BankChurners.csv")

sakshigoyal7/credit-card-customers

kgl_datasets_list(search = "sakshigoyal7/credit-card-customers")
bank <- kgl_datasets_download(owner_dataset = "sakshigoyal7/credit-card-customers",
                              fileName = "BankChurners.csv",
                              datasetVersionNumber=1)

summary(bank)


bank <- kgl_datasets_download(owner_dataset = "amaliajimenezt/bank-churners", 
                              fileName = "bank_data_churners.csv")

