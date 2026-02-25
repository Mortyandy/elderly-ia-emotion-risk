readRDS("MLP_Champion_Model.rds")


library(shiny)
library(caret)
library(shinythemes)

# 1. Load model
final_model <- readRDS("MLP_Champion_Model.rds")

# 2. UI (English version)
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Elderly Internet Addiction & Emotion Regulation Risk Tool "),
  
  sidebarLayout(
    sidebarPanel(
      tags$h4("Select patient encoded indicators:"),
      hr(),
      
      # Wellbeing: numeric input
      sliderInput("well", "Wellbeing Score :", min = 0, max = 200, value = 130),
      
      # Age group: encoded as 1,2,3
      selectInput("age", "Age Group:", 
                  choices = list("60–70 years" = 1, "71–80 years" = 2, "81 years and older" = 3)),
      
      # Gender: 1=Male, 2=Female
      selectInput("gen", "Gender:", choices = list("Male" = 1, "Female" = 2)),
      
      # Education: 1,2,3,4
      selectInput("edu", "Education Level:", 
                  choices = list("Primary school" = 1, "Middle school" = 2, 
                                 "High school" = 3, "College or above" = 4)),
      
      # Support utilization: numeric input
      numericInput("supp", "Social Support Utilization Score:", value = 15),
      
      # Marital status: 1=Married/Partnered, 2=Single/Unpartnered
      selectInput("mar", "Marital Status:", 
                  choices = list("Married/Partnered" = 1, "Single/Unpartnered" = 2)),
      
      hr(),
      actionButton("calc", "Run Prediction", class = "btn-primary btn-lg", width = "100%")
    ),
    
    mainPanel(
      h3("Prediction Results:"),
      hr(),
      # Predicted probability display
      div(style = "font-size: 35px; font-weight: bold; color: #2c3e50; text-align: center;",
          textOutput("result_text")),
      
      # Risk alert display
      uiOutput("alert_box"),
      
      hr(),
      tags$h5("Data Alignment (Debug):"),
      tableOutput("debug_view")
    )
  )
)

# 3. Server logic (unchanged except for output texts)
server <- function(input, output) {
  
  observeEvent(input$calc, {
    # Build data frame exactly matching training format
    input_df <- data.frame(
      Wellbeing = as.numeric(input$well),
      Support_Utilization = as.numeric(input$supp),
      Age = as.numeric(input$age),
      Gender = as.numeric(input$gen),
      Education = as.numeric(input$edu),
      Marital_Status = as.numeric(input$mar)
    )
    
    # Prediction
    prob <- predict(final_model, newdata = input_df, type = "prob")[, "Comorbidity"]
    
    # Update probability display (English)
    output$result_text <- renderText({
      paste0("Predicted risk of comorbidity: ", round(prob * 100, 2), "%")
    })
    
    # Update risk alert (English, threshold 0.307)
    output$alert_box <- renderUI({
      if(prob >= 0.307) {
        wellPanel(style = "background: #f2dede; color: #a94442;", 
                  h4("⚠️ High Risk (Clinical intervention recommended)"))
      } else {
        wellPanel(style = "background: #dff0d8; color: #3c763d;", 
                  h4("✅ Low Risk (Continued observation)"))
      }
    })
    
    # Debug table: verify numeric encoding
    output$debug_view <- renderTable({ input_df })
  })
}

shinyApp(ui, server)
