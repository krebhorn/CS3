#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(plsdepot)
library(ggplot2)
library(visreg)
library(caret)
library(ggvis)

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
df.a <- df.adminpredict[,-1]

### Graph some Data for Linear Regression - see what it looks like. 

p1 = qplot(CGPA, Chance.of.Admit, data=df.a) + geom_point(colour = "#3366FF", size = 3)
p1 

###further examination shows that in general all of these correlate pretty well

cor(df.a$GRE.Score,df.a$Chance.of.Admit)
cor(df.a$TOEFL.Score,df.a$Chance.of.Admit)
cor(df.a$University.Rating,df.a$Chance.of.Admit)
cor(df.a$SOP,df.a$Chance.of.Admit)
cor(df.a$LOR,df.a$Chance.of.Admit)
cor(df.a$CGPA,df.a$Chance.of.Admit)

## Split into test and training sets
set.seed(1258) #reproucibility
splitdf.a = caret::createDataPartition(df.a[,1], p = 0.8, list=F, times=1)
splitdf.a
traindf.a = df.a[splitdf.a,]
head(traindf.a)
testdf.a = df.a[!row.names(df.a) %in% row.names(traindf.a),]
testdf.a = df.a[-splitdf.a,]
testdf.a


### do some regression
lr = lm(Chance.of.Admit ~ CGPA, data=traindf.a)
fitted(lr)
resid(lr)
lr
summary(lr)

p2 = p1 + geom_abline(intercept = lr[1]$coefficients[1], slope = lr[1]$coefficients[2], color="red")
p2

predict(lr, newdata=testdf.a)
predAdmit = data.frame(predict(lr, newdata=testdf.a))
names(predAdmit)[1] = 'Predicted'
predAdmit$Reference = testdf.a[,c('Chance.of.Admit')]
p3 = qplot(Reference, Predicted, data=predAdmit) + geom_point(colour = "#006600", size = 3)
p3


library(shiny)

cbname <- names(df.adminpredict[2:8])
yrange <- c(min(df.a$Chance.of.Admit), max(df.a$Chance.of.Admit))

# Define UI for application.
ui <- fluidPage(
  
  # Application title
  titlePanel("Graduate Admissions Acceptance"),
  
  # Sidebar with a slider input for number of bins 
  headerPanel('Correlatable Parameters'),
  sidebarPanel(
    selectInput('xcol',
                label = "Factors",
                choices = c(cbname))
    sliderInput('yrange',
                label = "Range of Admissions Likelihood", 
                min = 0.0, 
                max = 1.0, 
                value = c(yrange))
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    textOutput("selfactor"),
    textOutput("min_max")
    #plotOutput("plot")
  )
)


server <- function(input,output){
  
  output$selfactor <- renderText({
    paste("You selected", input$xcol)
  })
  #data <- reactive({(df.a[input$xcol,])
  
  # output$plot <- renderPlot({
  #   ggplot(data = data(), aes(x = input$xcol, y = df.a$Chance.of.Admit))+geom_point()
  #   
  #   })
}  
shinyApp(ui = ui, server = server)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

