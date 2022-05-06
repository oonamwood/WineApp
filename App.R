library(shiny)
library(RCurl)
library(knitr)
library(plyr)
library(qcc)
library(threejs)
library(rgl)
library(pca3d)
library(gridExtra)
library(ggplot2)
library(ggvis)
library(tidyverse)
library(lubridate)
library(shinyWidgets)
library(markdown)

data = read.csv('wine-quality-classified.csv', header=T)

ui <- navbarPage('Wine Quality Modeling',
  # First tab: landing page to give an overview of the app               
  tabPanel('Welcome',
           fluidRow(
             column(6,
                    includeMarkdown("about.Rmd")
             ),
             column(6,
                    img(src="vineyard.jpg", height="100%", width="100%"))
           )
           ),
  # Second tab: density plots: explore the data by selecting an independent
  # variable plus the option to filter the data
  tabPanel('Density Plots',
    # This plot will change based on the selection of the independent 
    # variable and the filter options
    plotOutput('plot1'),
  
    hr(),
    # Set the color to green
    setSliderColor(rep("#84ad32",(24)), sliderId = c(1:24)),
    
    # In this section are the options for the user
    fluidRow(
      column(3,
      selectInput(inputId = 'xcol', 'Select An Independent Variable', 
                names(data[,(0:11)])),
      sliderInput(inputId = 'fixedacidity', 'Filter Fixed Acidity', 
                3, 16, c(3,16), step = 1),
      sliderInput(inputId = 'volatileacidity', 'Filter Volatile Acidity', 
                0, 2, c(0,2), step = .1)
      ),
      column(3,
           
      sliderInput(inputId = 'citricacid', 'Filter Citric Acid', 
                0, 2, c(0,2), step = .1),
      sliderInput(inputId = 'residsugar', 'Filter Residual Sugar', 
                0, 66, c(0,66), step = 1),
      sliderInput(inputId = 'chlorides', 'Filter Chlorides', 
                0, .7, c(0,.7), step = .01)
      ),
      column(3,
      sliderInput(inputId = 'freesulf', 'Filter Free Sulfur Dioxides', 
                1, 289, c(1,289), step = 1),
      sliderInput(inputId = 'totsulf', 'Filter Total Sulfur Dioxides', 
                6, 440, c(6,440), step = 1),
      sliderInput(inputId = 'density', 'Filter Density', 
                .98, 1.04, c(.98, 1.04), step = .01)
      ),
      column(3,
      sliderInput(inputId = 'pH', 'Filter pH', 
                2.72, 4.01, c(2.72, 4.01), step = .01),
      sliderInput(inputId = 'sulphates', 'Filter Sulphates', 
                0.2, 2, c(0.2, 2), step = .1),
      sliderInput(inputId = 'alcohol', 'Filter Alcohol', 
                8, 15, c(8, 15), step = 1)
    )
  )
  ),
  # This page has two boxplots: one for zeroing in on a single variable at a
  # time and another for looking at the spread of the data altogether
  tabPanel('Boxplots',
           # Sidebar is for the inputs: selecting a variable and filtering
      sidebarBottomPanel(
        selectInput(inputId = 'xcol2', 'Select An Independent Variable', 
                    names(data[,(0:11)])),
        sliderInput(inputId = 'fixedacidity2', 'Filter Fixed Acidity', 
                       3, 16, c(3,16), step = 1),
        sliderInput(inputId = 'volatileacidity2', 'Filter Volatile Acidity', 
                       0, 2, c(0,2), step = .1),
         sliderInput(inputId = 'citricacid2', 'Filter Citric Acid', 
                     0, 2, c(0,2), step = .1),
         sliderInput(inputId = 'residsugar2', 'Filter Residual Sugar', 
                     0, 66, c(0,66), step = 1),
         sliderInput(inputId = 'chlorides2', 'Filter Chlorides', 
                     0, .7, c(0,.7), step = .01),
         sliderInput(inputId = 'freesulf2', 'Filter Free Sulfur Dioxides', 
                     1, 289, c(1,289), step = 1),
         sliderInput(inputId = 'totsulf2', 'Filter Total Sulfur Dioxides', 
                     6, 440, c(6,440), step = 1),
         sliderInput(inputId = 'density2', 'Filter Density', 
                     .98, 1.04, c(.98, 1.04), step = .01),
         sliderInput(inputId = 'pH2', 'Filter pH', 
                     2.72, 4.01, c(2.72, 4.01), step = .01),
         sliderInput(inputId = 'sulphates2', 'Filter Sulphates', 
                     0.2, 2, c(0.2, 2), step = .1),
         sliderInput(inputId = 'alcohol2', 'Filter Alcohol', 
                     8, 15, c(8, 15), step = 1)
  ),
  mainPanel(
    # Main panel displays the boxplots
    plotOutput("plot2"),
    plotOutput("plot3")
  )

           ),
  # This page is for the application of the K nearest neighbors algorithm
  # the user has the option of two methods of the wine quality as well as
  # selecting the number of neighbors the algorithm will evaluate
  tabPanel('K-Nearest Neighbors',
           sidebarPanel(
           selectInput('dataset', "Choose a Method",
                       choices = c('Quality Scores', 'Quality Classes'), 
                       ),
           helpText("Quality Scores uses the integer scale 1 to 10
                                with 1 being terrible, 10 being excellent. 
                                The Quality Classes options breaks the scores
                                into three groups: low, normal, and high."),
           sliderInput(inputId = 'K', 'Choose the number of neighbors to evaluate', 
                       1, 20,11 , step = 1)),
           mainPanel(
             # The output of the KNN algorithm will be a confusion matrix to 
             # display how accurate the model was as well as a breakdown of
             # where it was accurate or inaccurate
             plotOutput("plot4")
           )
           ),
    # This page is just for general exploration of the data used in this app
    tabPanel('Table',
             dataTableOutput("table")
             
  )
)

server <- function(input, output) {
  
  # Read in the main data
  data = read.csv('wine-quality-classified.csv', header=T)
  
  # Based on the independent variable and optional filters, the data will first
  # be filtered and then the specified variable colume will be selected and
  # be plotted
  selectedData1 <- reactive({
    req(input$xcol)
    
    # Set filters from inputs
    minFA <- input$fixedacidity[1]
    maxFA <- input$fixedacidity[2]
    minVA <- input$volatileacidity[1]
    maxVA <- input$volatileacidity[2]
    minCA <- input$citricacid[1]
    maxCA <- input$citricacid[2]
    minRS <- input$residsugar[1]
    maxRS <- input$residsugar[2]
    minCL <- input$chlorides[1]
    maxCL <- input$chlorides[2]
    minFS <- input$freesulf[1]
    maxFS <- input$freesulf[2]
    minTS <- input$totsulf[1]
    maxTS <- input$totsulf[2]
    minD <- input$density[1]
    maxD <- input$density[2]
    minPH <- input$pH[1]
    maxPH <- input$pH[2]
    minSU <- input$sulphates[1]
    maxSU <- input$sulphates[2]
    minA <- input$alcohol[1]
    maxA <- input$alcohol[2]
    
    
    # Apply filters
    data_filtered <- data %>%
      filter(
        fixed.acidity >= minFA,
        fixed.acidity <= maxFA,
        volatile.acidity >= minVA,
        volatile.acidity <= maxVA,
        citric.acid >= minCA,
        citric.acid <= maxCA,
        residual.sugar >= minRS,
        residual.sugar <= maxRS,
        chlorides >= minCL,
        chlorides <= maxCL,
        free.sulfur.dioxide >= minFS,
        free.sulfur.dioxide <= maxFS,
        total.sulfur.dioxide >= minTS,
        total.sulfur.dioxide <= maxTS,
        density >= minD,
        density <= maxD,
        pH >= minPH,
        pH <= maxPH,
        sulphates >= minSU,
        sulphates <= maxSU,
        alcohol >= minA,
        alcohol <= maxA
      )
  
    # Select the input column and the quality class
    # Provide names for the data frame and save as a data frame
    df <- data_filtered[, c(input$xcol, 'quality_class')]
    names(df)<-c("X","quality_class")
    dff<- as.data.frame(df)
  })
  
  # This will plot density curves for low, normal, and high quality wines
  # specifically using the the specified input xcol, the independent variable
  output$plot1 <- renderPlot({
    ggplot(selectedData1(), 
           aes(x = X, color = quality_class )) +
      geom_density( alpha=0.8) +
      theme_minimal()
         
  })
  
  # Using the same structure of input data as before, the data frame will be 
  # filtered based on the specified variable and filters
  # For this plot, only the selected variable can be filtered for the resulting
  # boxplot
  output$plot2 <- renderPlot({
    # Set filter variables
    minFA <- input$fixedacidity2[1]
    maxFA <- input$fixedacidity2[2]
    minVA <- input$volatileacidity2[1]
    maxVA <- input$volatileacidity2[2]
    minCA <- input$citricacid2[1]
    maxCA <- input$citricacid2[2]
    minRS <- input$residsugar2[1]
    maxRS <- input$residsugar2[2]
    minCL <- input$chlorides2[1]
    maxCL <- input$chlorides2[2]
    minFS <- input$freesulf2[1]
    maxFS <- input$freesulf2[2]
    minTS <- input$totsulf2[1]
    maxTS <- input$totsulf2[2]
    minD <- input$density2[1]
    maxD <- input$density2[2]
    minPH <- input$pH2[1]
    maxPH <- input$pH2[2]
    minSU <- input$sulphates2[1]
    maxSU <- input$sulphates2[2]
    minA <- input$alcohol2[1]
    maxA <- input$alcohol2[2]
    
    # Filter but only if the variable is the selected variable
    # otherwise, the filtering will not be applied
    
    if(as.character(input$xcol2) == 'fixed.acidity') {
      data_filtered <- data %>% 
        filter(
          fixed.acidity <= maxFA,
          fixed.acidity >= minFA)
      
    }
    else if (as.character(input$xcol2) == 'volatile.acidity') {
      data_filtered <- data %>% 
        filter(
          volatile.acidity <= maxVA,
          volatile.acidity >= minVA)
    }
    else if (as.character(input$xcol2) == 'citric.acid') {
      data_filtered <- data %>% 
        filter(
          citric.acid >= minCA,
          citric.acid <= maxCA)
    }
    else if (as.character(input$xcol2) == 'residual.sugar') {
      data_filtered <- data %>% 
        filter(
           residual.sugar >= minRS, 
           residual.sugar <= maxRS)
    }
    else if (as.character(input$xcol2) == 'chlorides') {
      data_filtered <- data %>% 
        filter(
           chlorides >= minCL,
           chlorides <= maxCL)
    }
    else if (as.character(input$xcol2) == 'free.sulfur.dioxide') {
      data_filtered <- data %>% 
        filter(
           free.sulfur.dioxide >= minFS,
           free.sulfur.dioxide <= maxFS)
    }
    else if (as.character(input$xcol2) == 'total.sulfur.dioxide') {
      data_filtered <- data %>% 
        filter(
           total.sulfur.dioxide >= minTS,
           total.sulfur.dioxide <= maxTS)
    }
    else if (as.character(input$xcol2) == 'density') {
      data_filtered <- data %>% 
        filter(
           density >= minD,
           density <= maxD)
    }
    else if (as.character(input$xcol2) == 'pH') {
      data_filtered <- data %>% 
        filter(
           pH >= minPH,
           pH <= maxPH)
    }
    else if (as.character(input$xcol2) == 'sulphates') {
      data_filtered <- data %>% 
        filter(
           sulphates >= minSU,
           sulphates <= maxSU)
    }
    else if (as.character(input$xcol2) == 'alcohol') {
      data_filtered <- data %>% 
        filter(
           alcohol >= minA,
           alcohol <= maxA)
    }
    # Single boxplot of the specified variable
    xlabel <- as.character(input$xcol2)
    boxplot(x = data_filtered[,input$xcol2], col='#84ad32',
            main = "Selected Independent Variable Only",
            xlab=xlabel)
  })
  
  # Using a similar structure as above, this will plot a boxplot with all the
  # variables and allow for filtering of any of the variables to influence
  # other variables' boxplot
  selectedData3 <- reactive({
    # Set filter variables
    minFA <- input$fixedacidity2[1]
    maxFA <- input$fixedacidity2[2]
    minVA <- input$volatileacidity2[1]
    maxVA <- input$volatileacidity2[2]
    minCA <- input$citricacid2[1]
    maxCA <- input$citricacid2[2]
    minRS <- input$residsugar2[1]
    maxRS <- input$residsugar2[2]
    minCL <- input$chlorides2[1]
    maxCL <- input$chlorides2[2]
    minFS <- input$freesulf2[1]
    maxFS <- input$freesulf2[2]
    minTS <- input$totsulf2[1]
    maxTS <- input$totsulf2[2]
    minD <- input$density2[1]
    maxD <- input$density2[2]
    minPH <- input$pH2[1]
    maxPH <- input$pH2[2]
    minSU <- input$sulphates2[1]
    maxSU <- input$sulphates2[2]
    minA <- input$alcohol2[1]
    maxA <- input$alcohol2[2]
    
    
    # Apply filters
    data_filtered <- data %>%
      filter(
        fixed.acidity >= minFA,
        fixed.acidity <= maxFA,
        volatile.acidity >= minVA,
        volatile.acidity <= maxVA,
        citric.acid >= minCA,
        citric.acid <= maxCA,
        residual.sugar >= minRS,
        residual.sugar <= maxRS,
        chlorides >= minCL,
        chlorides <= maxCL,
        free.sulfur.dioxide >= minFS,
        free.sulfur.dioxide <= maxFS,
        total.sulfur.dioxide >= minTS,
        total.sulfur.dioxide <= maxTS,
        density >= minD,
        density <= maxD,
        pH >= minPH,
        pH <= maxPH,
        sulphates >= minSU,
        sulphates <= maxSU,
        alcohol >= minA,
        alcohol <= maxA
      )
    # Save filtered dataframe
    df <- data_filtered[,c(1:12)]
    dff<- as.data.frame(df)
  })
  
  # Print all the boxplots of all the variables
  output$plot3 <- renderPlot({
    par(mar=c(10,4,1,1))
    boxplot(selectedData3(), col='#84ad32',
            main = "All Variables", las = 2)
    })

  # This whole section runs the KNN algorithm and produces a confusion matrix
  # reporting the accuracy of the model
  output$plot4 <- renderPlot({
    # Create a variable for random 80-20 splitting 
    ran <- sample(1:nrow(data), 0.8 * nrow(data))
    # Function for normalizing the data
    nor <-function(x) { (x -min(x))/(max(x)-min(x))   }
    # Apply the normalizing function
    norm <- as.data.frame(lapply(data[,c(1:11)], nor))
    
    # Apply the 80-20 split variable to produce the train and test sets
    train <- norm[ran,]
    test <- norm[-ran,] 
    
    # Depending on the input (scores or quality), the model will use different
    # columns from the data set for the dependent variable for classification
    if(input$dataset == 'Quality Scores') {
      target_category <- data[ran,12]
      test_category <- data[-ran,12]
    } else if (input$dataset == 'Quality Classes') {
      target_category <- data[ran,13]
      test_category <- data[-ran,13]}
    
    # import library
    library(class)
    
    # store predictions based on input number of K neighbors
    pr <- knn(train,test,cl=target_category,k=input$K)
    
    # create confusion matrix table
    tab <- table(pr,test_category)
    
    # Calculate the accuracy
    accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
    acc<-accuracy(tab)
    
    # take the confusion matrix table and convert to data frame
    confusion_matrix <-as.data.frame(tab)
    
    # Plot the confusion matrix data frame to see the results of the KNN
    ggplot(data = confusion_matrix,
           mapping = aes(x = pr,
                         y = test_category)) +
      geom_tile(aes(fill = Freq)) +
      geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
      scale_fill_gradient(low = "yellowgreen",
                          high = "darkgreen",
                          trans = "log") +
      ggtitle('Confusion Matrix \n Accuracy:', acc) +
      xlab('Predicted') + ylab('Actual')
    
  })
  
  
  output$table <- renderDataTable(data)
  
  
  
}
  

shinyApp(ui = ui, server = server)