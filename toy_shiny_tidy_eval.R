library(tidyverse)
toy_df <- tibble(xvar = c(2, 4, 5, 4),
                 yvar = c(6, 7, 5, 9),
                 zvar = c("a", "b", "d", "c"))

toy_df

ggplot(data = toy_df, aes(x = "xvar")) +
  geom_histogram()
toy_df %>% filter(xvar < 3)

env_var <- 3

toy_df %>% filter(.data[[input$varhoices]] < 3)


library(shiny)

ui <- fluidPage(
  radioButtons(inputId = "varchoices",
               label = "Choose a Variable",
               choices = names(toy_df)),
  plotOutput("varhist")
)

server <- function(input, output, session) {
  output$varhist <- renderPlot({
    ggplot(data = toy_df, aes_string(x = input$varchoices)) +
      geom_histogram(colour = "black", fill = "white")
  })
}

shinyApp(ui, server)
