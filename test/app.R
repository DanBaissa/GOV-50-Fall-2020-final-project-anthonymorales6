#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(ggforce)


EPLStandings <- read_csv("EPLStandings.csv",
                         col_types = 
                           cols(
                             Team = col_character(),
                             `2000` = col_double(),
                             `2001` = col_double(),
                             `2002` = col_double(),
                             `2003` = col_double(),
                             `2004` = col_double(),
                             `2005` = col_double(),
                             `2006` = col_double(),
                             `2007` = col_double(),
                             `2008` = col_double(),
                             `2009` = col_double(),
                             `2010` = col_double(),
                             `2011` = col_double(),
                             `2012` = col_double(),
                             `2013` = col_double(),
                             `2014` = col_double(),
                             `2015` = col_double(),
                             `2016` = col_double())
) 

d<- EPLStandings %>%
  pivot_longer(!Team, names_to = "Year", values_to = "Place")
d$Year <- as.numeric(d$Year)
d$Place <- factor(d$Place, levels = rev(levels(factor(d$Place))))

select_team <- unique(d$Team)


# Define UI for application that draws a histogram
ui <-  tabPanel("Model",
                fluidPage(
                  selectInput("team", "Team", choices = unique(d$Team)),
                  # selectInput("y", "Y variable", choices = names(EPLStandings)),
                  selectInput("geom", "Select Viewing Option", c("Point", "Line", "Smooth")),
                  plotOutput("plot")
                )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  plot_geom <- reactive({
    switch(input$geom,
           Point = geom_point(),
           Smooth = geom_smooth(se = TRUE, na.rm = TRUE),
           # jitter = geom_jitter(),
           Line = geom_line() 
    )
  })
  output$plot <- renderPlot({
    
  d %>% 
    filter(Team == input$team & !is.na(Place)) %>%
    ggplot(aes(x = Year, y = Place, group = 1)) + 
    xlim(2000, 2016) +
      plot_geom()
  }
)
}
# Run the application 
shinyApp(ui = ui, server = server)

