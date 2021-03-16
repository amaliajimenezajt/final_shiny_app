#install.packages("devtools")
#install.packages("usethis")
#install.packages("reticulate")
#install.packages("tensorflow")
#install.packages("keras")
# reticulate::py_install("kaggle")
# install_tensorflow(method = 'conda', envname = 'r-reticulate')

library(dplyr)
library(devtools)
library(kaggler)
library(reticulate)
library(keras)
library(tensorflow)
library(reticulate)

##################################### UPLOAD THE CSV DATA

kaggle <- import("kaggle")
kaggle$api$authenticate()
kaggle$api$dataset_download_files("sakshigoyal7/credit-card-customers", "BankChurners.csv", unzip = T)

#################################   Read the csv data

data_real <- read.csv("BankChurners.csv/BankChurners.csv",sep=",")
row.names(data_real )=data_real [,1] 
data_real = data_real[,-1]


data <- read.csv("BankChurners.csv/BankChurners.csv",sep=",")

######## OBSERVE IF THERE ARE NA'S

sum(is.na(data))  

######### THERE IS NO NA

########## CLEAN THE DATA 

row.names(data)=data[,1]
data = data[,-1]
colnames(data)
table(data$Income_Category)



################## AIM OF THE STUDY:
#### For the aim of the study (description in the app), it is consirable eliminate the following variables which 
#### are not neccesary for it:

data <- data[,-c(4,9,10,11,12,21,22)]


###################################  Now, i will convert the variables:

data$Attrition_Flag=as.factor(data$Attrition_Flag)
data$Card_Category=as.factor(data$Card_Category)
data$Customer_Age=as.integer(data$Customer_Age)
data$Gender=as.factor(data$Gender)
data$Education_Level=as.factor(data$Education_Level)
data$Income_Category=as.factor(data$Income_Category)
data$Avg_Open_To_Buy=as.integer(data$Avg_Open_To_Buy)


#### For make classification i will transform the responde variable as follow:
################################### TRANSFORM THE RESPONSE VARIABLE:

Income_Category_final=matrix(NA,nrow=nrow(data),ncol=1)
for (i in 1:nrow(data)){
  if(data[i,15]=="$120K +"){
    Income_Category_final[i]="FirstClass"
  }
  if(data[i,15]=="$60K - $80K"){
    Income_Category_final[i]="SecondClass"
  }
  if(data[i,15]=="$80K - $120K"){
    Income_Category_final[i]="FirstClass"
  }
  if(data[i,15]=="$40K - $60K"){
    Income_Category_final[i]="SecondClass"
  }
  if(data[i,15]=="Less than $40K"){
    Income_Category_final[i]="SecondClass"
  }
}

data = data.frame(data[,-15],Income_Category_final)
data$Income_Category_final = as.factor(data$Income_Category_final)


############################################################### APP

library(shiny)
library(shinythemes)


#################################### UI SECTION

summary_tabPanel <- tabPanel("Summary Data",
                             sidebarLayout(position = "left",
                                           sidebarPanel(
                                             h3(strong("Data Summary")),
                                             p("This shiny app is inspired by the classification problem addressed in the kaggle platform on the :",
                                               code("Bank_Churners"),"where we found the following variables involved in the problem after cleaning the database:",
                                             "- Hello wolrd","- Helloyyy"),
                                             selectInput(inputId = "DataSet",
                                                         label = "Choose a data set to show:",
                                                         choices = c("Real Data","Data Cleaned")),
                                             numericInput(inputId = "obs",
                                                          label = "Number of observations to view:",
                                                          value = 10)
                                                
                                           ), # sidebarpanel
                                           mainPanel(
                                          verbatimTextOutput("summary"),
                                         tableOutput("view"))

                             ) #sidebarLayout
) # tabPanel






ui <- navbarPage("Shiny by Amalia Jiménez",
                 theme = shinytheme("superhero"),
                 summary_tabPanel
                 )

################################## SERVER SECTION



server <- function(input, output) {
  
  
DataSetInput <- reactive({
    switch(input$DataSet,
           "Real Data" = data_real[,c(1:4)],
           "Data Cleaned" = data[,c(1:4)])
  })
  
output$summary <- renderPrint({
    DataSet <- DataSetInput()
    summary(DataSet)
  })
  
output$view <- renderTable({
    head(DataSetInput(), n = input$obs)
  })
  
  
  
  
  
  
}


################################## RUN THE APP

shinyApp(ui = ui, server = server)






