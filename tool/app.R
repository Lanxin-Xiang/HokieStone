library(shiny)

# Load your trained model
model <- readRDS("model_final.rds")

# Define UI
ui <- fluidPage(
  titlePanel("Diamond Price Estimator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("carat", "Carat Size:", value = 1, step = 0.01),
      selectInput("color", "Color:", choices = c("D", "E", "F", "G", "H", "I", "J", "K")),
      selectInput("clarity", "Clarity:", choices = c("SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1", "IF")),
      selectInput("cut", "Cut:", choices = c("Good", "Very Good", "Excellent", "Ideal")),
      actionButton("predict", "Estimate Price")
    ),
    
    mainPanel(
      h3("Predicted Price:"),
      textOutput("price"),
      h3("95% Prediction Interval:"),
      textOutput("interval")
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  observeEvent(input$predict, {
    # Create a new data frame with user inputs
    new_data <- data.frame(
      Carat.Size = input$carat,
      Color = input$color,
      Clarity = input$clarity,
      Cut = input$cut
    )
    
    # Get prediction and 95% confidence interval
    log_pred <- predict(model, newdata = new_data, interval = "prediction", level = 0.95)
    
    # Convert log-transformed predictions back to actual price
    pred_price <- exp(log_pred[1])
    lower_bound <- exp(log_pred[2])
    upper_bound <- exp(log_pred[3])
    
    # Update outputs
    output$price <- renderText(paste0("$", round(pred_price, 2)))
    output$interval <- renderText(paste0("[$", round(lower_bound, 2), " - $", round(upper_bound, 2), "]"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
