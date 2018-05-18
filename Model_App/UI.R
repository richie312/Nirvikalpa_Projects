
## Load the necessary packages

library(shiny)
library(e1071)
library(shinythemes)
library(randomForest)
library(RColorBrewer)
library(ggplot2)
library(ipred)
library(caret)
library(ROCR)
library(dplyr)
library(ModelMetrics)
library(rpart)
library(rpart.plot)
library(ggthemes)
library(gridExtra)
library(DT)
library(corrplot)



shinyUI(fluidPage(
  theme = shinytheme('darkly'),
  themeSelector(),
  
  
  navbarPage(
    title = "Data Modelling",  
    id= "nav",
    tabPanel("Data", value ="Data",
             
             
             (titlePanel("Data ToolBar")),
             
             sidebarLayout( 
               
               sidebarPanel( includeCSS("mystyle.css"), style = "color: #FF7D33;",
                             
                             fileInput(inputId = "file",label = "File", buttonLabel="Upload",
                                       accept = c(
                                         "text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                             helpText("Default max. file size is 100MB"),
                             tags$hr(),
                             h5(helpText("Select the read.table parameters below")),
                             checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
                             checkboxInput(inputId = "stringAsFactors", label = "stringAsFactors", value= FALSE),
                             br(),
                             radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
                             
                             actionButton(inputId = 'plot', label = 'Correlogram'),
                             br(),br(),
                             h5(helpText("Please make sure the dataset has been splitted on Data Modelling tab before download")),
                             br(),
                             downloadButton(outputId = "downloadfile",label="Download Dataset"),
                             br(),br(),
                             radioButtons(inputId = "dataset",label="Dataset", choices = c(
                               "Train","Test"),selected = "Train"), width =3
               ),
               
               
               
               
               mainPanel( style ="color: #FF7D33;
                          font-weight: 900;",
                          
                          DTOutput("table"),
                          tags$hr(),
                          h4(helpText("The App is preloaded with German Credit
                                      <https://github.com/richie312/Data_Camp_Tutorials_R/blob/master/ML_Tree_App/New%20folder/train.csv>, 
                                      You can choose your own dataset as well.", style ="color: #FF7D33;
                                      font-weight: 900;")),
                          br(), br(),
                          
                          textOutput("cor_text"),
                          plotOutput('plot')
                          
                          
                          )
               
                          )
               ),
    
    tabPanel("Data Modelling", value = "Model",
             
             (titlePanel("Pre-Processing and Modelling")),
             
             
             sidebarLayout(
               
               sidebarPanel( titlePanel("Missing Values"),includeCSS("mystyle.css"),style = "color: #FF7D33;",
                             tags$hr(),
                             h5(helpText("Select the Appropriate Missing Imputation criterion")),
                             br(),
                             radioButtons(inputId = 'Imputation', label = 'Variable', 
                                          choices = c("Continous", "Categorical", "Both"), 
                                          selected = "Both"),
                             br(),
                             actionButton(inputId = "Impute", label = "Impute"),
                             tags$hr(),
                             h5(helpText("Select the column number for the response variable, you
                                         can get the column number just by not selecting the header on Data tab.")),
                             selectInput(inputId = "col_num",label="col_num", choices=c(1:50),selected = 17),
                             h5(helpText("Split the Dataset")),
                             br(),br(),
                             sliderInput(inputId = "split_ratio", label="Split Ratio", min=0, max=1, value = 0.7),
                             actionButton(inputId = "Train", label="Train"),
                             actionButton(inputId = "Test", label="Test"),
                             br(), br(),
                             tags$hr(),
                             h5(helpText("Select the Model")),
                             selectInput(inputId = "Model", label= "Model",c("Decision Tree","Random Forest",
                                                                             "Logistic Regression", "Bagging")),
                             br(),
                             sliderInput(inputId = "nTrees", label="nTrees", min=0, max=10000, value = 950),
                             br(),
                             selectInput(inputId = 'cv', label = 'Cross Validation', c(0:10)),
                             tags$hr(),
                             h5(helpText("Specifiy the response variable")),
                             textInput(inputId = "Response", label = "Response Variable", value = "Enter the response var"),
                             tags$hr(),
                             h5(helpText("Fit the Model on training data")),
                             actionButton(inputId = "Fit", label ="Fit")
                             
                             ),
               
               
               
               
               
               mainPanel(style ="color: #FF7D33;
                         font-weight: 900;",tabsetPanel(
                           tabPanel("PreProcess_Tables", value ="PreProcess",
                                    DTOutput("table_Imputation"),
                                    br(),br(),
                                    textOutput("Text_train"),
                                    textOutput("obs_train"),
                                    textOutput("Text_test"),
                                    textOutput("obs_test")
                                    
                           ),
                           tabPanel("Model_Summary", value = "Summary",
                                    verbatimTextOutput("model")
                                    
                           ),
                           tabPanel("Plot", value="Model_Plot",
                                    
                                    plotOutput("Model_Plot"),
                                    br(),br(),
                                    plotOutput("plot_cptable")
                                    
                                    
                                    
                           )
                           
               )
               
             )
             )
             
    ),
    tabPanel("ROC & Confusion Matrix", value ="ROC",
             (titlePanel("ROC & Confusion Matrix")),
             
             
             sidebarLayout(
               
               sidebarPanel( titlePanel("Confusion Matrix"),includeCSS("mystyle.css"),style = "color: #FF7D33;",
                             tags$hr(),
                             h5(helpText("Generate the prediction list for each model")),
                             actionButton(inputId = "Predict_List", label = "Predict_List"),
                             br(), br(),
                             actionButton(inputId = "AUC", label="AUC"),
                             br(),br(),
                             sliderInput("threshold", label = "threshold",min=0,max=1, value=0.6),
                             br(),br(),
                             actionButton(inputId = "ConfusionMatrix", label="confusionMatrix")
                             
                             
                             
               ),
               
               
               
               
               
               mainPanel(style ="color: #FF7D33;
                         font-weight: 900;",tabsetPanel(
                           tabPanel("ROC & AUC", value ="ROC", 
                                    
                                    textOutput("text_predict"),
                                    textOutput("Logistic_Text"),
                                    DTOutput("List"),
                                    br(), br(),
                                    textOutput("text_auc"),
                                    textOutput("AUC"),
                                    br(),br(),
                                    plotOutput("ROC_Plot")
                                    
                                    
                           ),
                           tabPanel("ConfusionMatrix", value = "Matrix",
                                    
                                    plotOutput("Matrix_table"),
                                    br(),br(),
                                    plotOutput("Result")
                                    
                                    
                           )
                           
               )
               
             )
             )    
    ) 
    
  )
)
)
