#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a Normal density function + Probabilities
ui <- fluidPage(

    # Application title
    titlePanel(""),
    withMathJax(),
    # Sidebar
    sidebarLayout(
        sidebarPanel(
          selectInput("dist",
                      label = "Distribution",
                      choices = list("Normal", "T")),
        
          
          conditionalPanel(
            condition = "input.dist == 'Normal' ", 
            
            numericInput("mu",
                         h3("\u03BC"),
                         value = 0),
            numericInput("sig",
                         h3("\u03C3"),
                         value = 1),
            
            
            
            selectInput("Pro", # name to access the widget’s value
                        label = "Probability Area",
                        choices = list("Less Than",
                                       "Greater Than",
                                       "Between two numbers",
                                       "Less Than or Greater Than"),
                        multiple = FALSE,
                        selected = "Less Than"),
            
            
            conditionalPanel(
              condition = "input.Pro == 'Less Than'",
              numericInput("xp",
                           "Input a value more than \u03BC -3*\u03C3",
                           value = 0)),
            
            conditionalPanel(
              condition = "input.Pro == 'Greater Than'",
              numericInput("xpG",
                           "Input a value less than \u03BC + 3*\u03C3 ",
                           value = 0)),
            #conditionalPanel(
            #condition = "input.Pro == 'Greater Than'",
            #numericInput("xpB",
            #   "Input two value between (\u03BC - 3*sigma and mu + 3*sigma)",
            #  value = 0)),
            conditionalPanel(
              condition = "input.Pro == 'Between two numbers'",
              numericInput("xpBL",
                           "Input the Lower value (Should be more than \u03BC - 3*\u03C3)",
                           value = -1),
              
              numericInput("xpBU",
                           "Input the Upper value (Should be Less than \u03BC + 3*\u03C3)",
                           value = +1)
            ),
            conditionalPanel(
              condition = "input.Pro == 'Less Than or Greater Than'",
              numericInput("xpOL",
                           "Input the Lower value (Should be more than \u03BC - 3*\u03C3)",
                           value = -1),
              
              numericInput("xpOU",
                           "Input the Upper value (Should be Less than \u03BC + 3*\u03C3)",
                           value = +1)
            )
            
          ),
          
          
            conditionalPanel(
            condition = "input.dist == 'T' ", 
            numericInput("df", 
                         "Input degree of freedom ", 
                         value = +1), 
            selectInput("ProT", # name to access the widget’s value
                        label = "Probability Area",
                        choices = list("Less Than",
                                       "Greater Than",
                                       "Between two numbers",
                                       "Less Than or Greater Than"),
                        multiple = FALSE,
                        selected = "Less Than"),
            
            
            conditionalPanel(
              condition = "input.ProT == 'Less Than'",
              numericInput("xpT",
                           "Input a value",
                           value = 0)),
            
            conditionalPanel(
              condition = "input.ProT == 'Greater Than'",
              numericInput("xpGT",
                           "Input a value ",
                           value = 0)),
            #conditionalPanel(
            #condition = "input.Pro == 'Greater Than'",
            #numericInput("xpB",
            #   "Input two value between",
            #  value = 0)),
            conditionalPanel(
              condition = "input.ProT == 'Between two numbers'",
              numericInput("xpBLT",
                           "Input the Lower value",
                           value = -1),
              
              numericInput("xpBUT",
                           "Input the Upper value",
                           value = +1)
            ),
            conditionalPanel(
              condition = "input.ProT == 'Less Than or Greater Than'",
              numericInput("xpOLT",
                           "Input the Lower value",
                           value = -1),
              
              numericInput("xpOUT",
                           "Input the Upper value",
                           value = +1)
            )
            
          )
          
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           #plotOutput("distPlot2"),
           uiOutput("probability"),
           uiOutput('ex4'),
          # tags$head(
           tags$style("#probability{color: blue;
                                 font-size: 30px;
            font-style: bold;
            }"),

          tags$style("#ex4{color: red;
                                 font-size: 30px;
            font-style: bold;
            }"),
          tags$style("#xp{color: blue;
                                 font-size: 30px;
            font-style: bold;
            }")
          #)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  ## Plot

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
      if(input$dist == "Normal"){
        
        xmin    <- input$mu - 3*input$sig
        xmax    <- input$mu + 3*input$sig
        x <- seq(xmin, xmax, length = 101)
        y = dnorm(x,input$mu,input$sig)
        
        # draw the density plot
        layout_matrix = matrix(c(1, 2), ncol = 1)
        layout(layout_matrix,
               widths = .5,
               heights = rep(1,2))
        plot(x, y, type = 'l', lwd = 4, col = 3, main = "Normal Distribution")
        
        title(sub = paste('\u03BC = ',input$mu))
        #Probability Area
        if (input$Pro == "Less Than") {
          xmaxp    <- input$xp
          xpr <- seq(xmin, xmaxp, length = 101)
          ypr = dnorm(xpr,input$mu,input$sig)
          plot(x, y, type = 'l', lwd = 4, col = 3)
          polygon(x= c(xpr[1],xpr, xpr[length(xpr)]),y =c(0,ypr,0), col = "#1b98e0")
          
          title(sub = paste('\u03BC = ',input$mu)) } else if (input$Pro == "Greater Than") {
            xminp    <- input$xpG
            xmax    <- input$mu + 3*input$sig
            xpr <- seq(xminp, xmax, length = 101)
            ypr = dnorm(xpr, input$mu, input$sig)
            plot(x, y, type = 'l', lwd = 4, col = 3)
            polygon(x= c(xpr[1],xpr), y =c(0, ypr), col = "#1b98e0")
            
            title(sub = paste('\u03BC = ',input$mu))
            
          } else if(input$Pro == "Between two numbers") {
            xBL    <- input$xpBL
            xBU    <- input$xpBU
            xpr <- seq(xBL, xBU, length = 101)
            ypr = dnorm(xpr, input$mu, input$sig)
            plot(x, y, type = 'l', lwd = 4, col = 3)
            polygon(x= c(xpr[1], xpr, xpr[length(xpr)]), y =c(0, ypr, 0), col = "#1b98e0")
            
            title(sub = paste('\u03BC = ',input$mu))
            
          }
        else if(input$Pro == "Less Than or Greater Than") {
          plot(x, y, type = 'l', lwd = 4, col = 3)
          xOL    <- input$xpOL
          xOG    <- input$xpOU
          xprL <- seq(xmin, xOL, length = 101)
          xprG <- seq(xOG, xmax, length = 101)
          
          yprL = dnorm(xprL, input$mu, input$sig)
          yprG = dnorm(xprG, input$mu, input$sig)
          polygon(x= c(xprL[1], xprL, xprL[length(xprL)]), y =c(0, yprL, 0), col = "#1b98e0")
          yprL = dnorm(xprL, input$mu, input$sig)
          polygon(x= c(xprG[1], xprG, xprL[length(xprG)]), y =c(0, yprG, 0), col = "#1b98e0")
          title(sub = paste('\u03BC = ',input$mu))
          
        }
      } else if(input$dist == "T"){
        x = seq(qt(0.01, input$df), qt(0.99, input$df), length = 101)
        y = dt(x,input$df)
        
        # draw the density plot
        layout_matrix = matrix(c(1, 2), ncol = 1)
        layout(layout_matrix)
        plot(x, y, type = 'l', lwd = 4, col = 3, main = "T Distribution")
        
        title(sub = paste('df = ',input$df))
        #Probability Area
        if (input$ProT == "Less Than") {
          xmaxp    <- input$xpT
          xpr <- seq(qt(0.01, input$df), xmaxp, length = 101)
          ypr = dt(xpr,input$df)
          plot(x, y, type = 'l', lwd = 4, col = 3)
          polygon(x= c(xpr[1],xpr, xpr[length(xpr)]),y =c(0,ypr,0), col = "#1b98e0")
          
          title(sub = paste('df = ',input$df)) 
          } else if(input$ProT == "Greater Than"){
            xmin    <- input$xpGT
            xpr <- seq(xmin, qt(0.99, input$df), length = 101)
            ypr = dt(xpr,input$df)
            plot(x, y, type = 'l', lwd = 4, col = 3)
            polygon(x= c(xpr[1],xpr), y =c(0, ypr), col = "#1b98e0")
            
            title(sub = paste('df = ',input$df))
          } else if(input$ProT == "Between two numbers"){
            xBL    <- input$xpBLT
            xBU    <- input$xpBUT
            xpr <- seq(xBL, xBU, length = 101)
            ypr = dt(xpr,input$df)
            plot(x, y, type = 'l', lwd = 4, col = 3)
            polygon(x= c(xpr[1],xpr, xpr[length(xpr)]),y =c(0,ypr,0), col = "#1b98e0")
            title(sub = paste('df = ',input$df))
          } else if(input$ProT == "Less Than or Greater Than"){
            xOL    <- input$xpOLT
            xOG    <- input$xpOUT
            xprL <- seq(qt(0.01, input$df), xOL, length = 101)
            xprG <- seq(xOG, qt(0.99, input$df), length = 101)
            
            yprL = dt(xprL, input$df)
            yprG = dt(xprG, input$df)
            plot(x, y, type = 'l', lwd = 4, col = 3)
            polygon(x= c(xprL[1], xprL, xprL[length(xprL)]), y =c(0, yprL, 0), col = "#1b98e0")
            polygon(x= c(xprG[1], xprG, xprL[length(xprG)]), y =c(0, yprG, 0), col = "#1b98e0")
            title(sub = paste('df = ',input$df)) 
          }
          
        
      }
      

    })

    ### Text
    ## First
    output$probability <- renderUI({
      if(input$dist == "Normal"){
      if (input$Pro == "Less Than") {
      pr1 = pnorm(input$xp, input$mu, input$sig)
      temp1 = 'When X has a normal distribution with mean, '
      temp2 = ', and variance, '
      temp3 = ". Then the probability of "
      temp4 = ' less than '
      withMathJax(
        paste0(temp1,"\\( \\mu = \\)",input$mu, temp2, "\\( \\sigma^2 = \\) ",input$sig^2,
               temp3, "\\( X \\)", temp4,input$xp,"  is : ",round(pr1,2)
            , ', or in other words'))

      } else if (input$Pro == "Greater Than"){
        pr1    = 1- pnorm(input$xpG, input$mu, input$sig)
        temp1 = 'When X has a normal distribution with mean, '
        temp2 = ', and variance, '
        temp3 = ". Then the probability of "
        temp4 = ' greater than '
        withMathJax(
          paste0(temp1,"\\( \\mu = \\)",input$mu, temp2, "\\( \\sigma^2 = \\) ",input$sig^2,
                 temp3, "\\( X \\)", temp4,input$xpG,"  is : ",round(pr1,2)
                 , ', or in other words')) 
      } else if (input$Pro == "Between two numbers"){
        xBL    <- input$xpBL
        xBU    <- input$xpBU
        pr1 = pnorm(xBU, input$mu, input$sig) - pnorm(xBL, input$mu, input$sig)
        temp1 = 'When X has a normal distribution with mean, '
        temp2 = ', and variance, '
        temp3 = ". Then the probability of "
        temp4 = ' between '
        withMathJax(
          paste0(temp1,"\\( \\mu = \\)",input$mu, temp2, "\\( \\sigma^2 = \\) ",input$sig^2,
                 temp3, "\\( X \\)", temp4,input$xpBL, " and ", input$xpBU,  " is : ",round(pr1,2)
                 , ', or in other words'))
      } else if (input$Pro == "Less Than or Greater Than"){
        xOL    <- input$xpOL
        xOG    <- input$xpOU
        pr1 = 1 - pnorm(xOG, input$mu, input$sig) + pnorm(xOL, input$mu, input$sig)
        temp1 = 'When X has a normal distribution with mean, '
        temp2 = ', and variance, '
        temp3 = ". Then the probability of "
        temp4 = " less than "
        temp5 = " or greater than "
        withMathJax(
          paste0(temp1,"\\( \\mu = \\)",input$mu, temp2, "\\( \\sigma^2 = \\) ",input$sig^2,
                 temp3, "\\( X \\)", temp4,input$xpOL, temp5, input$xpOU, " is : ",round(pr1,2)
                 , ', or in other words'))
      }
      }else if(input$dist == "T"){
        if (input$ProT == "Less Than") {
          pr1 = pt(input$xp, input$df)
          temp1 = 'When X has a T distribution with degree of freedom, '
          temp2 = ". Then the probability of "
          temp3 = ' less than '
          withMathJax(
            paste0(temp1,  "\\( df= \\)",input$df,
                   temp2, "\\( X \\)", temp3,input$xpT,"  is : ",round(pr1,2)
                   , ', or in other words'))
          
        } else if (input$ProT == "Greater Than"){
          pr1    = 1- pt(input$xpG, input$df)
          temp1 = 'When X has a T distribution with with degree of freedom, '
          temp2 = ". Then the probability of "
          temp3 = ' less than '
          withMathJax(
            paste0(temp1,  "\\( df= \\)",input$df,
                   temp2, "\\( X \\)", temp3,input$xpGT,"  is : ",round(pr1,2)
                   , ', or in other words')) 
        } else if (input$ProT == "Between two numbers"){
          xBL    <- input$xpBL
          xBU    <- input$xpBU
          pr1 = pt(xBU, input$df) - pt(xBL, input$df)
          temp1 = 'When X has a T distribution with with degree of freedom, '
          temp2 = ". Then the probability of "
          temp3 = ' between '
          withMathJax(
            paste0(temp1,  "\\( df= \\)",input$df,
                   temp2, "\\( X \\)", temp3,input$xpBLT, " and ", input$xpBUT,  " is : ",round(pr1,2)
                   , ', or in other words'))
        } else if (input$ProT == "Less Than or Greater Than"){
          xOL    <- input$xpOL
          xOG    <- input$xpOU
          pr1 = 1 - pt(xOG, input$df) + pt(xOL, input$df)
          temp1 = 'When X has a T distribution with degree of freedom, '
          temp2 = ". Then the probability of "
          temp3 = ". Then the probability of "
          temp4 = " less than "
          temp5 = " or greater than "
          withMathJax(
            paste0(temp1,  "\\( df= \\)",input$df, temp2, "\\( X \\)",
                   temp3, "\\( X \\)", temp4,input$xpOLT, temp5, input$xpOUT, " is : ",round(pr1,2)
                   , ', or in other words'))
        }
      }
    })
    ## Second
      output$ex4 <- renderUI({
        if(input$dist == "Normal"){
          if (input$Pro == "Less Than") {
            #invalidateLater(5000, session)
            pr1 = pnorm(input$xp, input$mu, input$sig)
            pr1 = round(pr1,2)
            withMathJax( paste0('\\(P(X \\leq \\)', input$xp,
                                '\\( ) = \\)', pr1))
          } else if (input$Pro == "Greater Than") {
            #invalidateLater(5000, session)
            pr1 = 1- pnorm(input$xpG, input$mu, input$sig)
            pr1 = round(pr1, 2)
            withMathJax( paste0('\\(P(X \\geq \\)', input$xpG,
                                '\\( ) = \\)', pr1))
          }else if(input$Pro == "Between two numbers") {
            xBL    <- input$xpBL
            xBU    <- input$xpBU
            pr1 = pnorm(xBU, input$mu, input$sig) - pnorm(xBL, input$mu, input$sig)
            pr1 = round(pr1, 2)
            withMathJax( paste0('\\(P(\\)', xBL,'\\(\\leq X \\leq \\)', xBU,
                                '\\( ) = \\)', pr1))
          }else if(input$Pro == "Less Than or Greater Than") {
            xOL    <- input$xpOL
            xOG    <- input$xpOU
            pr1 = 1 - pnorm(xOG, input$mu, input$sig) + pnorm(xOL, input$mu, input$sig)
            pr1 = round(pr1, 2)
            withMathJax( paste0('\\(P(X \\leq \\)', xOL,  '\\( ) + \\)',
                                '\\(P(X \\geq \\)', xOG, '\\( ) = \\)', pr1))
          }  
        } else if(input$dist == "T"){
          if (input$ProT == "Less Than") {
            #invalidateLater(5000, session)
            pr1 = pt(input$xpT, input$df)
            pr1 = round(pr1,2)
            withMathJax( paste0('\\(P(X \\leq \\)', input$xp,
                                '\\( ) = \\)', pr1))
          } else if (input$ProT == "Greater Than") {
            #invalidateLater(5000, session)
            pr1 = 1- pt(input$xpGT, input$df)
            pr1 = round(pr1, 2)
            withMathJax( paste0('\\(P(X \\geq \\)', input$xpG,
                                '\\( ) = \\)', pr1))
          }else if(input$ProT == "Between two numbers") {
            xBL    <- input$xpBLT
            xBU    <- input$xpBUT
            pr1 = pt(xBU, input$df) - pt(xBL, input$df)
            pr1 = round(pr1, 2)
            withMathJax( paste0('\\(P(\\)', xBL,'\\(\\leq X \\leq \\)', xBU,
                                '\\( ) = \\)', pr1))
          }else if(input$ProT == "Less Than or Greater Than") {
            xOL    <- input$xpOLT
            xOG    <- input$xpOUT
            pr1 = 1 - pt(xOG, input$df) + pt(xOL, input$df)
            pr1 = round(pr1, 2)
            withMathJax( paste0('\\(P(X \\leq \\)', xOL,  '\\( ) + \\)',
                                '\\(P(X \\geq \\)', xOG, '\\( ) = \\)', pr1))
          }  
        }
        
        
      
      })



}

# Run the application
shinyApp(ui = ui, server = server)
