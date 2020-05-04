library(shiny)
library(DT)


server <- function(input, output){
  
  # Get new data set upon clicking on the activeButton
  # by getting a new random seed
  # id <- eventReactive(input$newdata, {
  #   sample(1:10^6,1)
  # })
  
  cl <- reactive({
    if (is.na(input$id)){
      return(NA)
    }else{
      # Plant the random number seed
      set.seed(input$id)
      cl <- sample(88:96, 1)
      return(cl)
    }
  })
  
  # Set the mean for numerical data
  mu <- reactive({
    
    # Plant the random number seed
    if (is.na(input$id)){
      return(NULL)
    }else{
      set.seed(input$id) 
      mu <- 1.671
    }})
  
  cat <- reactive({
    if (is.na(input$id)){
      return(NULL)
    }else{
      # Plant the random number seed
      set.seed(input$id) 
      cl <- sample(88:96, 1)
      n <- sample(100:200, 1)
      
      group_one <- rep(c('COVID-19'), times = 7)
      group_two <- rep(c('Economy'), times = 3)
      population <- c(group_one, group_two)
      a <- sample(population, n, replace = TRUE)
      A <- as.data.frame(table(a))
      names(A) <- c('concern', 'freq')
      x <- with(A[which(A$concern == 'COVID-19'),], freq)
      return(c(prop.test(x, n, 0.6, conf.level = cl/100, alternative = "greater", correct = FALSE), x, n))
    }
  }) # end reactive

    
  num <- reactive({
    
    if (is.na(input$id)){
      return(NULL)
    }else{
      set.seed(input$id) 
      n <- sample(30:40, 1)
      
      A_two <- sample(rnorm(1000, 1.671, 0.12), n)

      return(c(t.test(A_two, mu = 1.671, conf.level = cl()/100), sd(A_two)))
    }
    
  }) # end reactive
  
  
  # Output: Textblock 1 ----
  output$textblock <- renderText({
    HTML(
    paste("<font size = 4pt>Data set id:",
          "<font color=\"#FF0000\"><b>", 
          input$id,
          "</b></font>",
          "<ul>",
           "<li>Question 7:",
               "<ul>",
                   "<li>Sample mean: ", 
                        "<font color=\"#FF0000\"><b>",
                         sprintf('%.3f', num()$estimate),
                        "</b></font>",
                 "</li>",
                  "<li>Sample standard deviation: ", 
                          "<font color=\"#FF0000\"><b>",
                        sprintf('%.3f', num()[10]),
                         "</b></font>",
                  "</li>",
                 "<li>t<sub>&alpha;/2</sub> = ", qt((1-cl()/100)/2, as.integer(num()$parameter), lower.tail = FALSE), "</li>",
                   "<li>",
                         sprintf('%.2f confidence interval:', attr(num()$conf.int, "conf.level")),
                         "<font color=\"#FF0000\"><b>",
                         sprintf('(%.3f, %.3f)', num()$conf.int[1], num()$conf.int[2]),
                         "</b></font>",
                   "</li>",
                  "<li>Null value:", mu(), "</li>",
                  "<li>DF:<font color=\"#FF0000\"><b>", sprintf('%.0f', num()$parameter), "</b></font></li>",
                  "<li>Test statistic:<font color=\"#FF0000\"><b>", sprintf('%.3f', num()$statistic), "</b></font></li>",
                  "<li>Two-sided P-value:<font color=\"#FF0000\"><b>", sprintf('%.3f', num()$p.value), "</b></font></li>",
                  "</ul>",
          "</li>",
          "<li>Question 8:",
          "<ul>",
          "<li>Sample proportion: ", 
          "<font color=\"#FF0000\"><b>",
          sprintf('%.0f / %.0f = %.3f', cat()[10], cat()[11], cat()$estimate),
          "</b></font>",
          "</li>",
          "<li>z<sub>&alpha;/2</sub> = ", qnorm((1-cl()/100)/2, lower.tail = FALSE), "</li>",
          "<li>",
          sprintf('%.2f confidence interval:', attr(cat()$conf.int, "conf.level")),
          "<font color=\"#FF0000\"><b>",
          sprintf('(%.3f, %.3f)', cat()$conf.int[1], cat()$conf.int[2]),
          "</b></font>",
          "</li>",
          "<li>Test statistic: ", 
          sprintf('%.3f',  (cat()$estimate - cat()$null.value) / sqrt(cat()$null.value * (1 - cat()$null.value) / as.integer(cat()[11]) )), 
          "</li>",
          "<li>Two-sided P-value: ", sprintf('%.3f', cat()[3]), "</li>",
          "</ul>",
          "</li>",
          "</ul></font>"))
  })
  
} #end server