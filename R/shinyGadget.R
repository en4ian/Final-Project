library("COVID19")
library(shiny)
library(miniUI)
library(ggplot2)
library(dplyr)

indiana_covid_data = covid19("US", level=2)
indiana_covid_data <- indiana_covid_data %>% filter(key_alpha_2 == "IN")
indiana_covid_deaths_dataFrame <- data.frame(date=indiana_covid_data$date,deaths=indiana_covid_data$deaths)


selectData <- function(data, xvar, yvar) {

  ui <- miniPage(
    gadgetTitleBar("Drag to select points"),
    miniContentPanel(
      # The brush="brush" argument means we can listen for
      # brush events on the plot using input$brush.
      plotOutput("plot", height = "100%", brush = "brush")
    )
  )

  server <- function(input, output, session) {

    # Render the plot
    output$plot <- renderPlot({
      # Plot the data with x/y vars indicated by the caller.
      ggplot(data, aes_string(xvar, yvar)) +
        geom_point(stat="identity", size=1) +
        geom_line(stat = "identity", color = "darkblue") +
        theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
        labs(title = "COVID Deaths in Indiana") +
        xlab("Date") +
        scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
        ylab("Deaths") +
        theme_light()
    })

    # Handle the Done button being pressed.
    observeEvent(input$done, {
      # Return the brushed points. See ?shiny::brushedPoints.
      stopApp(brushedPoints(data, input$brush))
    })
  }

  runGadget(ui, server)
}

#selectData(indiana_covid_deaths_dataFrame, "date", "deaths")
