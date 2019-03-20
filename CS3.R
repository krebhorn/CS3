library(dplyr)
library(plsdepot)
library(ggplot2)
library(visreg)
library(caret)
library(ggvis)
library(shiny)

rsconnect::setAccountInfo(name='krebhorn',
                          token='2054AA4AE2C22A2354CDE4A791076BC0',
                          secret='DbgIU67Hy4j4gc7brKzCwtac/4RsQGeGfU036BAA')
library(rsconnect)
rsconnect::deployApp("/Users/keith/Documents/WPI/DS 501/CS3/")

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

  
server <- function(input,output){
  
  df <- reactive({
    r <- data.frame(y = df.a$Chance.of.Admit(), x = input$xcol)
  })
  
  udf <- reactive ({
    r<- data.frame(input$uGRE, input$uTOEFL, input$uUR, input$uSOP, input$uLOR, input$uCGPA, input$uUGR)
  })

  lr <- lm(df.a$Chance.of.Admit ~ df.a$GRE.Score + df.a$TOEFL.Score + df.a$University.Rating + df.a$SOP + df.a$LOR + df.a$CGPA, data = df.a)
  p <- reactive(lr$coefficients[2:7]*udf()[1:6])
  pred.a = reactive(lr$coefficients[1] + sum(p()))
  output$uchance = renderText({
     paste("Your predicted admissions chance is", round(pred.a(),2)*100,"%" )})
    
  output$selfactor <- renderText({
    paste("You selected", input$xcol ,"plot to view")
  })
  
  output$min_max <- renderText({
    paste("The range of interest is", input$yrange[1], "to", input$yrange[2])
  })
  
  output$plot <- renderPlot({
  x <- switch(input$xcol,
                 "GRE Score" = df.a$GRE.Score,
                 "TOEFL Score" = df.a$TOEFL.Score,
                 "University Rating" = df.a$University.Rating,
                 "Statement of Purpose Rating" = df.a$SOP,
                 "Letter(s) of Recommendation Rating" = df.a$LOR,
                 "College Grade Point Average" = df.a$CGPA,
                 "Undergraduate Research" = df.a$Research
                 )
  
  p1 <- qplot(x, Chance.of.Admit, data=df.a) + geom_point(colour = "#3366FF", size = 3) + ylim(input$yrange[1],input$yrange[2]) + xlab(input$xcol)
  fit <- lm(df.a$Chance.of.Admit ~ x)
  p1 + geom_abline(intercept = fit[1]$coefficients[1], slope = fit[1]$coefficients[2], color="red")
  
  
  
  })
  
  output$lmSum <- renderPrint({
    x <- switch(input$xcol,
                "GRE Score" = df.a$GRE.Score,
                "TOEFL Score" = df.a$TOEFL.Score,
                "University Rating" = df.a$University.Rating,
                "Statement of Purpose Rating" = df.a$SOP,
                "Letter(s) of Recommendation Rating" = df.a$LOR,
                "College Grade Point Average" = df.a$CGPA,
                "Undergraduate Research" = df.a$Research
    )
 
 fit <- lm(df.a$Chance.of.Admit ~ x)
 names(fit$coefficients) <- c("Intercept",input$var2)
 summary(fit)
 
  
  })
  
  output$cor <- renderText({
    x <- switch(input$xcol,
                "GRE Score" = df.a$GRE.Score,
                "TOEFL Score" = df.a$TOEFL.Score,
                "University Rating" = df.a$University.Rating,
                "Statement of Purpose Rating" = df.a$SOP,
                "Letter(s) of Recommendation Rating" = df.a$LOR,
                "College Grade Point Average" = df.a$CGPA,
                "Undergraduate Research" = df.a$Research
    )
  paste("The correlation between", input$xcol, "and the chance of admittance is",
  round(cor(x,df.a$Chance.of.Admit),2))
  
  })
  
  output$writeup <- renderText({
    paste({
    "The data collected in this study is from the dataset found here, https://www.kaggle.com/mohansacharya/graduate-admissions. 
    This data was of interest because it is a dataset that a user could use to 'guess' at the future. If this app were expanded, 
    it could allow users to input only some of their information (such as college GPA) and make a determination if they would get admitted.
    Currently the application requires users to input all of their data, and not just some of their data.  
    This is accomplished using linear regression modelling. These models are generated using R's Linear Regression modelling. 
    Another potential way to expand this application would be to highlight where the users data point falls on each plot, allowing the user to compare themselves
    to their peers. This data shows that the most important variable is the undergraduate GPA. Somethings that were not inlcuded in the data, but could perhaps paint 
    a clearer picture of the applicant would be a measure of work experience (perhaps binary - they have it or they don't). One thing that is always tricky is dealing with
      subjective analysis (i.e. the strength of the Letters of Recommendation)"})
  })
  
  
}  
shinyApp(ui = ui, server = server)
options(rsconnect.max.bundle.size=3145728000)
deployApp()
