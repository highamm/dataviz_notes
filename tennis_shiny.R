library(tidyverse)

theme_set(theme_minimal())

atp_df <- read_csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2024.csv") |>
  mutate(tour = "atp")
wta_df <- read_csv("https://raw.githubusercontent.com/JeffSackmann/tennis_wta/master/wta_matches_2024.csv") |>
  mutate(tour = "wta")

both_df <- bind_rows(atp_df, wta_df)

both_long <- both_df |> pivot_longer(c(winner_name, loser_name))

## only keep players who have player over 50 matches
both_n50 <- both_long |> group_by(value) |> summarise(n = n()) |>
  filter(n > 50)

## construct various statistics
major_tennis <- semi_join(both_long, both_n50, by = c("value"))
major_tennis <- major_tennis |> mutate(w_svperc = 100 * w_1stIn / w_svpt,
                                       l_svperc = 100 * l_1stIn / l_svpt,
                                       w_firstwon = 100 * w_1stWon / w_1stIn,
                                       l_firstwon = 100 * l_1stWon / l_1stIn,
                                       w_secondwon = 100 * w_2ndWon / (w_svpt - w_1stIn),
                                       l_secondwon = 100 * l_2ndWon / (l_svpt - l_1stIn))

major_tennis_w <- major_tennis |> filter(name == "winner_name")
major_tennis_l <- major_tennis |> filter(name == "loser_name")

w_small <- major_tennis_w |> select(value, winner_seed, w_ace, w_df,
                                    w_svperc, w_firstwon, w_secondwon,
                                    tour) |>
  rename(seed = winner_seed, ace = w_ace, df = w_df, svperc = w_svperc,
         firstwon = w_firstwon, secondwon = w_secondwon)

l_small <- major_tennis_l |> select(value, loser_seed, l_ace, l_df,
                                    l_svperc, l_firstwon, l_secondwon, tour) |>
  rename(seed = loser_seed, ace = l_ace, df = l_df, svperc = l_svperc,
         firstwon = l_firstwon, secondwon = l_secondwon)

df <- bind_rows(w_small, l_small) |>
  rename(player = "value")
df

df_oneplayer <- df |> filter(player == "Grigor Dimitrov")
ggplot(data = df_oneplayer, aes(x = firstwon)) +
  geom_histogram(fill = "#CCFF00", colour = "grey")

library(shiny)

player_choices <- df |> distinct(player) |>
  pull(player)

unique(df$player)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons("tour_choose", label = "Choose a Tour: ",
                   choices = c("atp", "wta"), selected = "wta"),
      selectInput("player_choose", label = "Choose a Player: ",
                  choices = player_choices),
      radioButtons("variable_choose", label = "Choose a Variable: ",
                   choices = c("ace", "df", "svperc", "firstwon", "secondwon")),
      sliderInput("bins_choose", label = "Choose Number of Bins",
                  min = 7, max = 35, value = 15)
    ),
    mainPanel(
      plotOutput("hist_player")
    )
  )
)

## steps 6 and 7 (and 8 if you have time)
server <- function(input, output, session) {
  
  observeEvent(input$tour_choose, {
    player_choices <- df |> filter(tour == input$tour_choose) |> distinct(player) |>
      pull(player)
    updateSelectInput(inputId = "player_choose", choices = player_choices)
    
  })
  output$hist_player <- renderPlot({
    df_oneplayer <- df |> filter(player == input$player_choose)
    ggplot(data = df_oneplayer, aes(x = .data[[input$variable_choose]])) +
      geom_histogram(fill = "#CCFF00", colour = "grey",
                     bins = input$bins_choose) +
      labs(title = glue::glue(input$variable_choose, " statistic for ", input$player_choose)) +
      theme_minimal(base_size = 22)
  })
}

shinyApp(ui, server)

