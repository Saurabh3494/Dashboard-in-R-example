#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(ggplot2)
library(readr)
library(corrplot)
library(plotly)


# Load the dataset
dataset <- read_csv("Sleep_health_and_lifestyle_dataset.csv")


# Convert specific columns to categorical or numerical
categorical_vars <- c("Person ID","Gender", "Occupation","BMI Category","Blood Pressure",'Sleep Disorder') # rcategorical variable names
numerical_vars <- c("Age", "Sleep Duration","Quality of Sleep","Physical Activity Level","Stress Level","Heart Rate","Daily Steps") # numerical variable names

dataset[categorical_vars] <- lapply(dataset[categorical_vars], as.factor)
dataset[numerical_vars]   <- lapply(dataset[numerical_vars], as.numeric)


# Define UI for the application
ui <- navbarPage(
  "Sleep Health and Lifestyle Dashboard",
  tabPanel("Background",
           fluidPage(
             titlePanel("Quantifying Sleep Quality: An Analytical Perspective"),
             tags$div(
               tags$h3("ANLY 512 Group Project"),
               tags$p(tags$em("Saurabh Shirish Prabhu, Nikhil Dinesh Patil, Muhammad Shapiai, Srijitha Mattookkaran, Venkat Srikanth Ayyagari")),
               tags$h4(tags$strong("Importance of Sleep Quality Analysis")),
               tags$p("Understanding the importance of sleep quality can help individuals take necessary steps to improve their sleep habits and, consequently, their quality of life."),
               tags$h4(tags$strong("Objectives")),
               tags$ul(
                 tags$li("Present Findings of our Sleep Quality Data Analysis"),
                 tags$li("Encourage Self-Reflection"),
                 tags$li("Highlight the Impact of Quality Sleep and exercise on Daily Life")
               ),
               tags$h4(tags$strong("Background")),
               tags$h5(tags$strong("What is Sleep Quality?")),
               tags$p("It is how well you sleep. It is not just about the amount of sleep you get, but also about how restful and restorative that sleep is."),
               tags$h5(tags$strong("Health Implications")),
               tags$p("Good sleep quality is crucial for overall health, as poor sleep is linked to cardiovascular diseases, diabetes, obesity, and mental health disorders."),
               tags$h5(tags$strong("Cognitive Function")),
               tags$p("Sleep quality affects cognitive functions like memory, attention, and decision-making, with poor sleep impairing these abilities and reducing productivity."),
               tags$h5(tags$strong("Emotional Well-being")),
               tags$p("Quality sleep is essential for emotional regulation, as lack of sleep can cause mood swings, anxiety, and depression."),
               tags$h4(tags$strong("Factors Influencing Sleep Quality")),
               tags$ul(
                 tags$li(tags$strong("Sleep Environment:"), " Noise, light, temperature, and comfort."),
                 tags$li(tags$strong("Stress and Anxiety:"), " Mental health and emotional stress."),
                 tags$li(tags$strong("Exercise:"), " Physical activity levels and timing."),
                 tags$li(tags$strong("Sleep Schedule:"), " Consistency and timing of sleep and wake times."),
                 tags$li(tags$strong("Medical Conditions:"), " Chronic pain, sleep disorders, and other health issues."),
                 tags$li(tags$strong("Substance Use:"), " Alcohol, nicotine, and medication effects."),
                 tags$li(tags$strong("Technology Use:"), " Screen time and electronic device usage before bed.")
               ),
               tags$h4(tags$strong("Research Questions")),
               tags$ul(
                 tags$li("What is the relationship between sleep quality and steps taken?"),
                 tags$li("How does sleep quality correlate with stress levels and are there differences based on gender?"),
                 tags$li("How is the relationship between stress and sleep quality affected by age?"),
                 tags$li("How does heart rate correlate with stress levels, and does this vary by occupation?"),
                 tags$li("What is the association between gender and sleep duration?"),
                 tags$li("What is the distribution of any given numerical variable in the dataset?"),
                 tags$li("Which variables have the highest correlations with each other?")
               ),
               tags$h4(tags$strong("Data Source")),
               tags$p(tags$strong("Kaggle: "), "https://www.kaggle.com/datasets/uom190346a/sleep-health-and-lifestyle-dataset/data")
             )
           )
  ),
  tabPanel("Dataset Information",
           fluidPage(
             titlePanel("Sleep Dataset Information"),
             tags$div( 
               tags$h4(tags$p(tags$strong("Key Features of the Dataset:"))),
               tags$ul(
                       tags$li(tags$p("Comprehensive Sleep Metrics: Explore sleep duration, quality, and factors influencing sleep patterns.")),
                       tags$li(tags$p("Lifestyle Factors: Analyze physical activity levels, stress levels, and BMI categories.")),
                       tags$li(tags$p("Cardiovascular Health: Examine blood pressure and heart rate measurements.")),
                       tags$li(tags$p("Sleep Disorder Analysis: Identify the occurrence of sleep disorders such as Insomnia and Sleep Apnea."))
               )
               ),
             
             tags$div(
               tags$h4(tags$p(tags$strong("Variable Details"))),
               tags$ul(
                 tags$li(tags$p(tags$strong("Person ID:")," An identifier for each individual.")),
                 tags$li(tags$p(tags$strong("Gender:")," The gender of the person (Male/Female).")),
                 tags$li(tags$p(tags$strong("Age:")," The age of the person in years.")),
                 tags$li(tags$p(tags$strong("Occupation:"),"  The occupation or profession of the person.")),
                 tags$li(tags$p(tags$strong("Sleep Duration (hours):")," The number of hours the person sleeps per day.")),
                 tags$li(tags$p(tags$strong("Quality of Sleep (scale: 1-10):")," A subjective rating of the quality of sleep, ranging from 1 to 10.")),
                 tags$li(tags$p(tags$strong("Physical Activity Level (minutes/day):")," The number of minutes the person engages in physical activity daily.")),
                 tags$li(tags$p(tags$strong("Stress Level (scale: 1-10):")," A subjective rating of the stress level experienced by the person, ranging from 1 to 10.")),
                 tags$li(tags$p(tags$strong("BMI Category:")," The BMI category of the person (e.g., Underweight, Normal, Overweight).")),
                 tags$li(tags$p(tags$strong("Blood Pressure (systolic/diastolic):")," The blood pressure measurement of the person, indicated as systolic pressure over diastolic pressure.")),
                 tags$li(tags$p(tags$strong("Heart Rate (bpm):")," The resting heart rate of the person in beats per minute.")),
                 tags$li(tags$p(tags$strong("Daily Steps:")," The number of steps the person takes per day.")),
                 tags$li(tags$p(tags$strong("Sleep Disorder:")," The presence or absence of a sleep disorder in the person (None, Insomnia, Sleep Apnea).")),
               ),
             ),
             tags$h4(tags$p(tags$strong("Summary of variables"))),
             verbatimTextOutput("summary"),
             tags$p(""),
             tags$p(""),
             tags$div(
               tags$h4(tags$p(tags$strong("Methods"))),
               tags$p(tags$li("374 rows and 13 columns, covering a wide range of variables related to sleep and daily habits")),
               tags$p(tags$li("Data was cleaned and analyzed utilizing R version 4.2.2 and dashboards were created using R Shiny Version 1.7.4 ")),
               tags$p(tags$li("Dataset has several continuous and discrete variable which were utilized for analysis")),
               tags$p(tags$li("Userinput were used generate interactive user experience")),
               tags$p(tags$li("Conditional loops were used generate interactive plots")),
               tags$p(tags$li("Analyzed impact of exercise, BMI on sleep patterns and health"))
             )
             )
           ),
  tabPanel("Dataset Exploration",
           fluidPage(
             titlePanel("Dataset Exploration"),
             
             DTOutput("data_table")
           )
  ) ,
  tabPanel("Static Exploratory Plots",
           fluidPage(
             titlePanel("Data Exploration Plots"),
             mainPanel(
               tags$h4(tags$p(tags$li(tags$strong("What is the distribution of any given numerical variable in the dataset?")))),
               sliderInput("binwidth_stress", "Binwidth for Stress Level:", min = 0.1, max = 5, value = 0.75),
               plotOutput("hist1"),
               sliderInput("binwidth_age", "Binwidth for Age:", min = 0.1, max = 5, value = 0.5),
               plotOutput("hist2"),
               tags$h4(tags$p(tags$li(tags$strong("Stress and its relationship with Heat Rate classified by Professions - Positive relationship between Stress and Heart rate. Sales Rep with least stress with Doctors/Managers having highest stress")))),
               plotOutput("scatterPlot1"),
               tags$h4(tags$p(tags$li(tags$strong("Quality of Sleep is directly proportional to Number of Steps across both Males and Females")))),  
               plotOutput("scatterPlot2"),
               tags$h4(tags$p(tags$li(tags$strong("Stress and its relationship with Sleep by Age - Inversely proportional to each other")))), 
               plotOutput("scatterPlot3"),
               tags$h4(tags$p(tags$li(tags$strong("What is the association between gender and sleep duration? Sleep duration is same for both genders with a Median of 7.25 (approx)")))),
               plotOutput("boxplot1"),
               tags$h4(tags$p(tags$li(tags$strong("Correlation of various variables")))),
               plotOutput("corrplt")
             )
           )
  ),
  tabPanel("Interactive Plots",
           fluidPage(
             titlePanel("Data Exploration Plots"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("xcol", "X Variable", choices = names(dataset)),
                 selectInput("ycol", "Y Variable", choices = names(dataset)),
                 selectInput("colorVar", "Color by", choices = c("None", names(dataset))),
                 selectInput("plotType", "Plot Type", choices = c("Scatter Plot", "Box Plot", "Histogram")),
                 tags$p(tags$strong("Notes:")),
                 tags$p("Use the dropdown menus to select the X and Y variables, and optionally a variable to color by. Color option currently works for scatter plot only. Choose the plot type to visualize the data accordingly. Color option can be used to color by any categorical feature."),
                 tags$p(tags$strong("Scatter Plot:"), "Displays a scatter plot of the selected variables."),
                 tags$p("Use the dropdown menus to select the X and Y variables, and optionally a variable to color by. Choose the plot type to visualize the data accordingly."),
                 tags$p(tags$strong("Box Plot:"), "Displays a box plot of the selected variables."),
                 tags$p(tags$strong("Histogram:"), "Displays a histogram of the selected X variable.")
               ),
               mainPanel(
                 plotOutput("plot")
               )
             )
           )
    )
)

# Define server logic required to draw the plots
server <- function(input, output) {
  # Summary of the dataset
  output$summary <- renderPrint({
    summary(dataset)
  })
  
  # Data table of the dataset
  output$data_table <- renderDT({
    datatable(dataset)
  })
  
  # Scatter plot 1: Age vs. Sleep Duration, colored by Gender
  output$scatterPlot1 <- renderPlot({
    ggplot(dataset, aes(x = `Stress Level`, y = `Heart Rate`, color = Occupation)) +
      geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
      labs(title = "Stress level vs. heart rate", x = "Stress Level", y = "Heart Rate") +
      theme_minimal()
  })
  
  # Scatter plot 2: Sleep Duration vs. Age, colored by Gender
  output$scatterPlot2 <- renderPlot({
    ggplot(dataset, aes(x = `Sleep Duration`, y = `Daily Steps`, color = Gender)) +
      geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
      labs(title = "Sleep Duration vs. Daily Steps", x = "Sleep Duration (hours)", y = "Daily Steps") +
      theme_minimal()
  })
  
  # Scatter plot 3: Sleep Duration vs. Stress, colored by Age
  output$scatterPlot3 <- renderPlot({
    ggplot(dataset, aes(x = `Sleep Duration`, y = `Stress Level`, color = Gender)) +
      geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
      labs(title = "Sleep Duration vs. Stress Level", x = "Sleep Duration (hours)", y = "Stress Level") +
      theme_minimal()
  })
  
  # Box plot 1: Sleep Duration by Gender
  output$boxplot1 <- renderPlot({
    ggplot(dataset, aes(x = `Sleep Duration`, y = Gender, color = Gender)) +
      geom_boxplot() +
      labs(title = "Sleep Duration by Gender", x = "Sleep Duration (hours)", y = "Daily Steps") +
      theme_minimal()
  })
  
  # hist 1: stress level
  output$hist1 <- renderPlot({
      ggplot(dataset, aes(x = `Stress Level`)) +
      geom_histogram(binwidth = input$binwidth_stress, position = "dodge") +
      theme_minimal()
  })
  
  # hist 2: Age
  output$hist2 <- renderPlot({
    ggplot(dataset, aes(x = `Age`)) +
      geom_histogram(binwidth = input$binwidth_age, position = "dodge") +
      theme_minimal()
  })
  
  # corrplot 1: Sleep Duration by Gender
  output$corrplt <- renderPlot({
    corrplot(cor(dataset[, numerical_vars]) ,  tl.cex = 0.75)
  })
  
  
  # Reactive expression for the selected data
  selectedData <- reactive({
    dataset[, c(input$xcol, input$ycol, input$colorVar)]
  })
  
  # Plot output
  output$plot <- renderPlot({
    x_var <- input$xcol
    y_var <- input$ycol
    color_var <- input$colorVar
    
    if (input$plotType == "Scatter Plot") {
      p <- ggplot(selectedData(), aes_string(x = paste0("`", x_var, "`"), y = paste0("`", y_var, "`")))
      if (color_var != "None") {
        p <- p + aes_string(color = paste0("`", color_var, "`"))
        }
      p + geom_point(position = position_jitter(width = 0.1, height = 0.1)) + theme_minimal()
    }
    
    
    
    else if (input$plotType == "Box Plot") {
      ggplot(dataset, aes_string(x = paste0("`", x_var, "`"), y = paste0("`", y_var, "`"))) +
        geom_boxplot() +
        theme_minimal()
    } 
    
    
    else if (input$plotType == "Histogram") {
      ggplot(dataset, aes_string(x = paste0("`", x_var, "`"))) +
        geom_histogram(binwidth = 0.2, position = "dodge") +
        theme_minimal()
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
