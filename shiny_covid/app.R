library(shiny)
library(tidyverse)

covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
states <- covid19 %>%
  select(state) %>%
  distinct() %>%
  arrange(state) %>%
  pull(state)

ui <- fluidPage(selectInput(inputId = "pick_state",
                 label = "Select a State",
                 choices = states,
                 multiple = TRUE),
                submitButton(text = "Submit"),
                plotOutput(outputId = "uh"))
server <- function(input, output) {
output$uh <- renderPlot(
  covid19 %>%
    mutate(post_20 = cases >= 20) %>%
    filter(post_20 == TRUE,
           state %in% input$pick_state) %>%
    ggplot(aes(x = date, y = cases, color = state)) +
    scale_y_log10() +
    geom_line())
}
shinyApp(ui = ui, server = server)