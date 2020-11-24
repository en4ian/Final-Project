library(readxl)
library("COVID19")
#Download COVID-19 data across governmental sources at national, regional, and city level, as described in Guidotti and Ardia (2020) <doi:10.21105/joss.02376>
library(ggplot2)
library(dplyr)

#file/statistics obtained from http://www.stats.indiana.edu/vitals/
indiana_deaths <- read_excel("./stats_indiana_deaths_data.xls")

indiana_covid_data = covid19("US", level=2)
indiana_covid_data <- indiana_covid_data %>% filter(key_alpha_2 == "IN")

#goal was to create a simple regression to predict the 2020 deaths from this data. and then compare that with our previous covid death plot

year <- pull(indiana_deaths %>% filter(Area=="Indiana"), Year)
deaths <- pull(indiana_deaths %>% filter(Area=="Indiana"),Total)

#linear regression
regression <- lm(deaths ~ year)
coeffs = coefficients(regression)

#y = -858090.0  + 456.1x
year = 2020           # the waiting time
predicted_deaths = coeffs[1] + coeffs[2]*year

predicted_deaths

#' Show plot of Deaths from Covid in Indiana since March compared to
#' the deaths we expected to see in 2020 prior to Covid
#'
#' @examples
#' plot_death_difference()
plot_death_difference <- function()
{
  #looking at deaths over time
  indiana_covid_deaths_dataFrame <- data.frame(date=indiana_covid_data$date,deaths=indiana_covid_data$deaths)
  a= c(as.integer(as.POSIXct("2020-03-14")),0)
  b= c(as.integer(as.POSIXct("2020-11-14")), predicted_deaths/365*245)

  plot <- ggplot(data=indiana_covid_deaths_dataFrame, aes(x=date, y=deaths)) +
    geom_point(stat="identity", size=1) +
    geom_line(stat = "identity", color = "darkblue") +
    #geom_abline(intercept = (-1)*as.integer(as.POSIXct("2020-03-14")), slope =(predicted_deaths/365), size=5, color = "blue") +
    geom_segment(aes(x=as.Date("2020-03-14", "%Y-%m-%d"), y=0, xend=as.Date("2020-11-14", "%Y-%m-%d"), yend=(predicted_deaths/365)*245)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    labs(title = "COVID Deaths in Indiana compared to rate of all cause deaths predicted for 2020") +
    xlab("Date") +
    scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
    ylab("Deaths") +
    theme_light()

  #show plot
  plot
}

plot_death_difference()
