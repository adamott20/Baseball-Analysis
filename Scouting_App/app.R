library(shiny)
library(tidyverse)
library(baseballr)

source("helper_functions.R")

# May take a few seconds to run first player
ui <- fluidPage(
  
  # Application title
  titlePanel("Player Report"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(4,wellPanel(
      textInput("last_name",
                "Last Name:"),
      textInput("first_name",
                "First Name:"),
      dateRangeInput("dates", "Date:"),
      selectInput("chart_type", "Chart Type:", c("Radial Chart", "Spray Chart", "Hot Zones: Slugging", "Hot Zones: Exit Velocity")),
      actionButton("change_player", "Update Player/Dates"),
      actionButton("change_chart", "Update Chart")
    )),
    
    # Show a plot of the generated distribution
    
    column(6,plotOutput("distPlot")),
    column(2,tableOutput("dataTable"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  df <- NULL
  observeEvent(input$change_player, {
    my_id <- playerid_lookup(paste(input$last_name), paste(input$first_name))$mlbam_id
    
    df <<- scrape_statcast_savant(start_date = paste(input$dates[1]), end_date = paste(input$dates[2]), playerid = my_id)
  })
  
  observeEvent(input$change_chart, {output$distPlot <- renderPlot({
    if (input$chart_type == "Radial Chart"){
      radial_chart(df)
    } else if(input$chart_type == "Spray Chart"){
      ggspraychart(df)
    } else if(input$chart_type == "Hot Zones: Slugging"){
      hotzones(df)
    } else if(input$chart_type == "Hot Zones: Exit Velocity"){
      hotzones_ev(df)
    }
  })})
  observeEvent(input$change_chart, {output$dataTable <- renderTable({((t(basic_table(df))))},rownames = TRUE, colnames = FALSE, striped = TRUE, bordered = TRUE)})
}

# Run the application 
shinyApp(ui = ui, server = server)
