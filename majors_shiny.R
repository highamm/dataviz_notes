library(tidyverse)
library(readxl)
theme_set(theme_minimal())

df <- read_excel("data/slu_graduates_17_25.xlsx")

## fixes error in the data
df <- df |> mutate(across(everything(),
                          .fns = ~replace(., . ==  "STATS" , "STAT")))

df_long <- df |> pivot_longer(4:9, names_to = "type",
                              values_to = "discipline")

df_major <- df_long |>
  filter(type == "major1" | type == "major2" | type == "major3")

df_stat <- df_major |> filter(discipline == "STAT") 

df_stat |> group_by(sex) |>
  summarise(n_gender = n())

df_statfull <- semi_join(df_long, df_stat, by = "adm_id") |>
  filter(type == "major1" |
           type == "major2" | 
           type == "major3")

df_nostat <- df_statfull |> filter(discipline != "STAT" &
                                     !is.na(discipline)) |>
  group_by(discipline) |>
  summarise(nstudent = n()) |>
  mutate(discipline = fct_reorder(discipline, nstudent))

ggplot(data = df_nostat, aes(x = discipline, y = nstudent)) +
  geom_segment(aes(y = 0, yend = nstudent, xend = discipline),
               colour = "brown") +
  geom_point(colour = "darkred") + 
  coord_flip() +
  theme_minimal() +
  labs(y = "number of majors")


library(shiny)

## exercises 1, 3, 4, 2
choices <- unique(df_major$discipline)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("major_id",
                  label = "Choose a Major: ",
                  choices = choices,
                  selected = "STAT"),
      sliderInput("min_num",
                  label = "Choose a Minimum Number",
                  min = 1, max = 15, value = 1)),
    mainPanel(
      plotOutput("major_lolli"),
      tableOutput("gender_table")
    )
  )
)


server <- function(input, output, session) {
  

  
  major_reactive_df <- reactive({
    df_stat <- df_major |> filter(discipline == input$major_id) 
  
  df_statfull <- semi_join(df_long, df_stat, by = "adm_id") |>
    filter(type == "major1" |
             type == "major2" | 
             type == "major3")
  
  df_nostat <- df_statfull |> filter(discipline != input$major_id &
                                       !is.na(discipline)) |>
    group_by(discipline) |>
    summarise(nstudent = n()) |>
    mutate(discipline = fct_reorder(discipline, nstudent)) |>
    filter(nstudent >= input$min_num)
  
  })
  
  output$gender_table <- renderTable({
    sex_reactive_df()
  })
  
  sex_reactive_df <- reactive({
    df_stat <- df_major |> filter(discipline == input$major_id) 
    
    df_stat |> group_by(sex) |>
      summarise(n_gender = n())
  })
  

  
  output$major_lolli <- renderPlot({ 
    ggplot(data = major_reactive_df(), aes(x = discipline, y = nstudent)) +
    geom_segment(aes(y = 0, yend = nstudent, xend = discipline),
                 colour = "brown") +
    geom_point(colour = "darkred") + 
    coord_flip() +
    theme_minimal() +
    labs(y = "number of majors")
  })
}

shinyApp(ui, server)
