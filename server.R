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