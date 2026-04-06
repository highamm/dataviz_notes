library(shiny)
library(gapminder)
library(tidyverse)
continents <- unique(gapminder$continent)

ui <- fluidPage(
  selectInput("continent", "Continent", choices = continents),
  selectInput("country", "Country", choices = NULL),
  actionButton("runappbutton", label = "Update Data"),
  tableOutput("data")
  
)


server <- function(input, output, session) {
  
  observeEvent(input$continent, {
    country_choices <- gapminder |> filter(continent == input$continent) |>
      distinct(country) |> pull(country)
    
    updateSelectInput(inputId = "country", choices = country_choices)
  })
  
  gapminder_react <- eventReactive(input$runappbutton, {
    gapminder |> filter(country == input$country)
  })
  
  output$data <- renderTable({
    gapminder_react()
  })
}

shinyApp(ui, server)

## part c: skip