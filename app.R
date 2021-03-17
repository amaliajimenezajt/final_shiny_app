# install.packages("devtools")
# install.packages("usethis")
# install.packages("reticulate")
# install.packages("tensorflow")
# install.packages("keras")
# reticulate::py_install("kaggle")
# install_tensorflow(method = 'conda', envname = 'r-reticulate')

library(dplyr)
library(devtools)
library(kaggler)
library(reticulate)
library(keras)
library(tensorflow)
library(reticulate)
library(ggplot2)
library(devtools)
library(factoextra)
library(NbClust)

##################################### UPLOAD THE CSV DATA

kaggle <- import("kaggle")
kaggle$api$authenticate()
kaggle$api$dataset_download_files("sakshigoyal7/credit-card-customers", "BankChurners.csv", unzip = T)

#################################   Read the csv data

data_real <- read.csv("BankChurners.csv/BankChurners.csv",sep=",")
row.names(data_real )=data_real [,1] 
data_real = data_real[,-1]


data <- read.csv("BankChurners.csv/BankChurners.csv",sep=",")

########## CLEAN THE DATA 

row.names(data)=data[,1]
data = data[,-1]
colnames(data)
table(data$Income_Category)


################## AIM OF THE STUDY:
#### For the aim of the study (description in the app), it is consirable eliminate the following variables which 
#### are not neccesary for it:

data <- data[,-c(4,6,9,10,11,12,21,22)]


######## OBSERVE IF THERE ARE NA'S

sum(is.na(data))  


############################## CORRELATION NUMERICAL VARIABLES

library(corrplot)
X <- data[,c(2,7:14)]
corrplot(cor(X),is.corr=T)

## As shown in the corplot, there is a high correlation between "Avg_Open_To_Buy" and "Credit_Limit"
## Therefore, to avoid bad results in the models, we omitted the variables "Credit_Limit" and "Total_Trans_Ct".

data <- data[,-c(7,9)]

######### THERE IS NO NA in categorical variables, however see in the categorical variable the type "Unknown":

table(data$Income_Category) # 1112 observations unknown
table(data$Education_Level) #  1519   observations unknown
table(data$Card_Category) # 0 unknown


################################### CONVERT KNOWN in NA

data$Income_Category[data$Income_Category == "Unknown"] <- NA
data$Education_Level[data$Education_Level == "Unknown"] <- NA

sum(is.na(data$Income_Category))
sum(is.na(data$Education_Level))

################################### IMPUT NA 

data <- data[-c(which(is.na(data$Income_Category)),which(is.na(data$Education_Level))),]

###################################  Now, I will convert the variables:


data$Gender=as.factor(data$Gender)
data$Attrition_Flag=as.factor(data$Attrition_Flag)
data$Card_Category=as.factor(data$Card_Category)
data$Customer_Age=as.integer(data$Customer_Age)
data$Gender=as.factor(data$Gender)
data$Education_Level=as.factor(data$Education_Level)
data$Income_Category=as.factor(data$Income_Category)


#### For make classification i will transform the response variable as follow:
################################### TRANSFORM THE RESPONSE VARIABLE:

Income_Category_final=matrix(NA,nrow=nrow(data),ncol=1)
for (i in 1:nrow(data)){
  if(data[i,5]=="$120K +"){
    Income_Category_final[i]="FirstClass"
  }
  if(data[i,5]=="$60K - $80K"){
    Income_Category_final[i]="SecondClass"
  }
  if(data[i,5]=="$80K - $120K"){
    Income_Category_final[i]="FirstClass"
  }
  if(data[i,5]=="$40K - $60K"){
    Income_Category_final[i]="SecondClass"
  }
  if(data[i,5]=="Less than $40K"){
    Income_Category_final[i]="SecondClass"
  }
}

data = data.frame(data[,-5],Income_Category_final)
data$Income_Category_final = as.factor(data$Income_Category_final)

############## NUMERICAL VARIABLES

num_data <- data[,c(2,6:11)]
num_data <- num_data[,-c(3,6)]
new_data <-data.frame(num_data
                      ,data$Income_Category_final)
X_scale <- scale(num_data)

############## CATEGORICAL VARIABLES

cat_data <- data[,c(1,3,4,5,12)]
cat_data <- cat_data [,-2]

############################################################### APP

library(shiny)
library(shinythemes)
library(plotly)
library(shinyjs)
library(caret)
library(AppliedPredictiveModeling)
library(caret)
library(shiny)
library(e1071)
library(pROC)


######################################################

choices_val_num <- colnames(num_data)


choices_val_cat <- colnames(cat_data)

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
                                           mainPanel(tabsetPanel(type = "tabs",
                                                                 tabPanel("Summary", verbatimTextOutput("summary")),
                                                                 tabPanel("View", tableOutput("view"))))
                                           

                             ) #sidebarLayout
) # tabPanel

numerical.plots <- tabPanel("Numerical Plots",
                            sidebarLayout(position="right",
                            sidebarPanel(h3(strong("BOX-PLOT/HISTOGRAM")),
                                         p("Select the numerical variable you want to display, if you want to see the histogram select the number of bins desired:"),
                                         HTML("<hr>"),
                                         radioButtons(inputId="variablesnum", 
                                                      label=h4("Select Numeric Variable to show"),
                                                      choices=choices_val_num),
                                         radioButtons(inputId="variablescat", 
                                                      label=h4("Select Numeric Variable to show"),
                                                      choices=choices_val_cat),
                                         sliderInput("bins", label=h4("Select n-bins for the histogram plot"),
                                                     min = 1, max = 50, value = 30))
                                         , # sidebar panel
                            mainPanel(tabsetPanel(type = "tabs",
                                                  tabPanel("Box-plot", plotOutput("boxplot")),
                                                  tabPanel("Histogram", plotOutput("histPlot"))))
  
                            ) # sidebarlayout
    
) # tab panel 

cat_plot <- tabPanel("Categorical Plotly",
                     useShinyjs(),
                    style = "border: 1px solid silver;",
                    cellWidths = 300,
                    cellArgs = list(style = "padding: 6px"),
                     h3("This is the Plotly of the categorical variable Income which is the response in the clasification problem"),
                     column(6,plotlyOutput(outputId = "plotlysec"),height="600px"),
                     column(6,plotlyOutput(outputId = "plotlypoint"),height="600px"),
                          
) # tab panel 


classif_tab <- tabPanel("Clasification Problem",
                        sidebarLayout(
                          sidebarPanel(
                            p("Hello"),
                            br(),
                            sliderInput("train",
                                        "Training Fraction Data:",
                                        value = 0.75, step = 0.05, min = 0.60, max = 0.80),
                            br(),
                            HTML("<hr>"),
                            radioButtons("methodsec", h4("Caret Model"),
                                         c("Linear Discriminant Analysis (lda)"          = "lda",
                                           "Quadratic Discriminant Analysis (lda)" = "qda",
                                           "Random Forest (rf) "                        = "rf",
                                           "Support Vector Machines"  = "svm"
                                         ))),
                            mainPanel(tabsetPanel(type = "tabs",
                                          tabPanel("Summary-Fit",  verbatimTextOutput("fit")),
                                          tabPanel("ConfusionMatrix",  verbatimTextOutput("confusion")),
                                          tabPanel("Importance variables",  plotOutput("importance"))
                            ))
                            
                        ) # sidebarLayout
) # tabpanel

cluster_panel <- tabPanel("Clustering",
                          sidebarLayout(
                            sidebarPanel(
                              p("Cluster"),
                              numericInput("randomseed", "Random Seed", 1996, min=100, max=10000),
                              HTML("<hr>"),
                              selectInput(inputId = "numberclus",
                                          label = "Choose method number cluster:",
                                          choices = c("Elbow method"="wss",
                                                      "Silhouette method"="silhouette",
                                                       "Gap statistics"="gap_stat")),
                              numericInput("numcluster", "Number of Cluster", 2, min=2, max=5,step = 1)),
                            mainPanel(tabsetPanel(type = "tabs",
                                                  tabPanel("Number of cluster",  plotOutput("numcluster")),
                                                  tabPanel("Summary k-means",  verbatimTextOutput("kmean")),
                                                  tabPanel("plotkmean",  plotOutput("kmean"))

                            ))
                                                 
            
                )
)






ui <- navbarPage("Shiny by Amalia Jiménez",
                 theme = shinytheme("superhero"),
                 summary_tabPanel,
                 numerical.plots,
                 cat_plot,
                 classif_tab, 
                 cluster_panel
                 )

################################## SERVER SECTION

server_train <- function(train){
  set.seed(1996)
  datatrain <- createDataPartition(data$Income_Category_final, p=train, list=FALSE)
  trainBank <- new_data [datatrain,]
  testBank <- new_data [-trainBank,]
  invisible(list(training=trainBank,testing=testBank))
}

server_model <- function(method,training){
  ctrl <- trainControl(method = "cv", 
                       number = 5)
  fit <- train(Income_Category_final ~ ., 
                 method = method,
                 data = training,
                 preProcess = c("center", "scale"),
                 metric = "Accuracy",
                 trControl = ctrl)
  invisible(fit)
  
}

response_fit <- function(methodsec, training)
{
  datat <- server_train(train)
  fitting <- server_model(methodsec, datat$training)
  invisible(fitting)
}



server <- function(input, output) {
  
  
DataSetInput <- reactive({
    switch(input$DataSet,
           "Real Data" = data_real,
           "Data Cleaned" = data)
  })
  
output$summary <- renderPrint({
    DataSet <- DataSetInput()
    summary(DataSet)
  })
  
output$view <- renderTable({
    head(DataSetInput(), n = input$obs)
  })


output$boxplot <- renderPlot({
  ggplot(data,aes(x=data[,input$variablescat], y=data[,input$variablesnum])) +
    geom_boxplot(aes(fill=data[,input$variablescat]),notchwidth = 0.8,outlier.colour="red",outlier.fill="red",
                                   outlier.size=1) + 
    stat_summary(fun.y=mean, geom="point", shape=18,
                      size=3, color="red")+
    theme_bw() 

})

output$histPlot <- renderPlot({
  ggplot(data = data, aes_string(x = input$variablesnum)) +
    geom_histogram(bins = input$bins, fill = "darkslategray4", color = "black") +
    theme_bw()

})


output$plotlysec <- renderPlotly({
  plot_ly(data, x = ~Total_Trans_Amt, y = ~Total_Trans_Ct, type = "scatter", mode = "markers",
          symbol = ~Income_Category_final) %>% 
    layout(title = "Freq12 vs Freq 24",
           xaxis = list(title = "Total_Trans_Amt"), 
           yaxis = list(title = "Total_Trans_Ct"))
})


output$plotlypoint <- renderPlotly({
  colors <- c('rgb(211,94,96)', 'rgb(128,133,133)')
  
  plot1 <- plot_ly(data, labels = ~Income_Category_final,  type = 'pie',
                   marker = list(colors = colors,
                                 line = list(color = '#FFFFFF', width = 1)))
  plot1 <- plot1 %>% layout(title = 'PIE  PLOT CARD VARIABLE',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
})

datamodel <- reactive({
  response_fit(input$train,input$methodsec)
})

output$fit <- renderPrint({
  datamodel()$fitting
})

output$numcluster <- renderPlot({
   fviz_nbclust(X_scale,kmeans,method=numberclus,k.max=10)+
     geom_vline(xintercept = 8, linetype = 2)+
     labs(subtitle = "Elbow method")
 })

}


################################## RUN THE APP

shinyApp(ui = ui, server = server)






