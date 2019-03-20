library(dplyr)
library(plsdepot)
library(ggplot2)
library(visreg)
library(caret)
library(ggvis)
library(shiny)


df.adminpredict <- read.csv("/Users/keith/Documents/WPI/DS 501/CS3/Admission_Predict_Ver1.1.csv")
str(df.adminpredict)

###The dataset contains several parameters which are considered important during the application for Masters Programs. 
### The parameters included are : 
# 1. GRE Scores ( out of 340 ) 
# 2. TOEFL Scores ( out of 120 ) 
# 3. University Rating ( out of 5 ) 
# 4. Statement of Purpose and Letter of Recommendation Strength ( out of 5 ) 
# 5. Undergraduate GPA ( out of 10 ) 
# 6. Research Experience ( either 0 or 1 ) 
# 7. Chance of Admit ( ranging from 0 to 1 )

### to clean data let's remove serial number (trivial input)
df.a<- df.adminpredict[,-1]


library(shiny)

cbname <- names(df.adminpredict[2:8])
yrange <- c(min(df.a$Chance.of.Admit), max(df.a$Chance.of.Admit))

# Define UI for application.
ui <- fluidPage(
  
  # Application title
  titlePanel("Graduate Admissions Acceptance"),

    sidebarPanel('Correlatable Parameters',
      selectInput('xcol',
                  label = "Factors / Plots to View", 
                  c("GRE Score", "TOEFL Score", "University Rating", "Statement of Purpose Rating", "Letter(s) of Recommendation Rating", "College Grade Point Average", "Undergraduate Research")),
      sliderInput('yrange',
                  label = "Y-Axis Zoom / Range of Admissions Likelihood", 
                  min = 0.0, 
                  max = 1.0, 
                  value = c(yrange)),
      numericInput('uGRE','Enter Your GRE Score (0-340)', min = 0, max = 340, value = 320),
      numericInput('uTOEFL','Enter Your TOEFL Score (0-120)', min = 0, max = 120, value = 100),
      numericInput('uUR','Enter Your University Rating (1-5)', min = 1, max = 5, value = 4),
      numericInput('uSOP','Enter the Qualitative Strength of your Statement of Purpose (1-5)', min = 1, max = 5, value = 4),
      numericInput('uLOR','Enter the Qualitative Strength of your Letter(s) of Recommendation (1-5)', min = 1, max = 5, value = 4),
      numericInput('uCGPA','Enter your Undergraduate GPA (0-10)', min = 0, max = 10, value = 8),
      checkboxInput('uUGR','I conducted Academic Research in my Undergraduate', value = FALSE)
    ),

  
    # Show outputs
    mainPanel(
      textOutput("selfactor"),
      textOutput("min_max"),
      textOutput("uchance"),
      plotOutput("plot"),
      verbatimTextOutput("lmSum"),
      verbatimTextOutput("cor"),
      textOutput("writeup")
      
    )
  )


shinyApp(ui = ui, server = server)

deployApp(shinyAppDir("Users/keith/Documents/WPI/DS 501/CS3/"))




