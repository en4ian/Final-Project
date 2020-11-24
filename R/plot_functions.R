library(usethis)
library("COVID19")
#Download COVID-19 data across governmental sources at national, regional, and city level, as described in Guidotti and Ardia (2020) <doi:10.21105/joss.02376>
library(ggplot2)
library(dplyr)
library(maps)
library(htmltab)

#paper for package is going to be #vignette from package
#use_vignette()
indiana_covid_data = covid19("US", level=2)
indiana_covid_data <- indiana_covid_data %>% filter(key_alpha_2 == "IN")

#' Show plot of Deaths from Covid in Indiana since March
#'
#' @examples
#' plot_deaths()
plot_deaths <- function()
{
  #looking at deaths over time
  indiana_covid_deaths_dataFrame <- data.frame(date=indiana_covid_data$date,deaths=indiana_covid_data$deaths)

  plot <- ggplot(data=indiana_covid_deaths_dataFrame, aes(x=date, y=deaths)) +
  geom_point(stat="identity", size=1) +
  geom_line(stat = "identity", color = "darkblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  labs(title = "COVID Deaths in Indiana") +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  ylab("Deaths") +
  theme_light()

  #show plot
  plot
}

#' Show plot of number of tests performed in Indiana since March
#'
#' @examples
#' plot_tests()
plot_tests <- function()
{
  #looking at data vs number of tests performed
  indiana_covid_tests_dataFrame <- data.frame(date=indiana_covid_data$date,tests=indiana_covid_data$tests)

  plot <- ggplot(data=indiana_covid_tests_dataFrame, aes(x=date, y=tests)) +
  geom_point(stat="identity", size=1) +
  geom_line(stat = "identity", color = "darkblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  labs(title = "COVID Tests Performed in Indiana") +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  ylab("Tests") +
  scale_y_continuous(labels = scales::comma) +
  theme_light()

  #show plot
  plot
}

#' Show plot of number of positive tests performed in Indiana since March
#'
#' @examples
#' plot_positiveTests()
plot_positiveTests <- function()
{
  #looking at data vs confirmed cases
  indiana_covid_positives_dataFrame <-data.frame(date=indiana_covid_data$date, positives=indiana_covid_data$confirmed)

  plot <- ggplot(data=indiana_covid_positives_dataFrame, aes(x=date, y=positives)) +
  geom_point(stat = "identity", size=1) +
  geom_line(stat = "identity", color = "darkblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  labs(title = "Total Positive COVID Tests in Indiana") +
  #x axis
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  #y axis
  ylab("Positives") +
  scale_y_continuous(labels = scales::comma) +
  theme_light()

  #show plot
  plot
}

plot_positivityOfTest <- function()
{
  #This function takes a dataset that includes both tests and confirmed cases of the tests. A new data frame is returned that stores the resulting positivity rate
  indiana_covid_positivity_dataFrame <-data.frame(date=indiana_covid_data$date, positivity=(indiana_covid_data$confirmed/indiana_covid_data$tests))

  plot <- ggplot(data=indiana_covid_positivity_dataFrame, aes(x=date, y=positivity)) +
    geom_area(stat = "identity", color="darkblue", fill="lightblue") +
    #this line doesn't work for some reason
    #geom_hline(yintercept=mean(indiana_covid_data$confirmed/indiana_covid_data$tests)) +
    geom_hline(yintercept=0.06) + #goal 5% positivity rate
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    labs(title = "Positivity rate of tests in Indiana") +
    xlab("Date") +
    scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
    ylab("Positivity Rate") +
    scale_y_continuous(labels = scales::comma) +
    theme_light()

    #show plot
    plot
}

plot_positivityOfTest()


plot_countyDeaths <- function()
{
  # This function is written from the tutorial on
  # https://books.psychstat.org/rdata/data-visualization.html

  uscounty <- map_data('county')
  indiana <- subset(uscounty, region == "indiana")

  map_theme <- theme(axis.line=element_blank(),
                     axis.text.x=element_blank(),
                     axis.text.y=element_blank(),
                     axis.ticks=element_blank(),
                     axis.title.x=element_blank(),
                     axis.title.y=element_blank(),
                     panel.background=element_blank(),
                     panel.border=element_blank(),
                     panel.grid.major=element_blank(),
                     panel.grid.minor=element_blank(),
                     plot.background=element_blank())

  ## education data
  edu <- htmltab(doc="http://www.stats.indiana.edu/web/county/edattain00.asp")

  ## population data
  population <- htmltab(doc="http://www.stats.indiana.edu/population/PopTotals/historic_counts_counties.asp")

  demo.data <- cbind(edu[, c(6, 8)], population[, c(1, 12:13)])

  ## change variable names
  names(demo.data) <- c('edu2010', 'edu1990', 'subregion', 'pop2000', 'pop2010')

  ## remove the first line of data
  demo.data <- demo.data[-1, ]

  ## convert to numeric
  demo.data$edu1990 <- as.numeric(demo.data$edu1990)
  demo.data$edu2010 <- as.numeric(demo.data$edu2010)

  ## first replace comma and then change to numeric
  demo.data$pop2000 <- as.numeric(gsub(",", "", demo.data$pop2000))
  demo.data$pop2010 <- as.numeric(gsub(",", "", demo.data$pop2010))

  ## convert uppper case to lower case letters
  demo.data$subregion <- tolower(demo.data$subregion)

  ## calculate a difference score
  demo.data$edudiff <- demo.data$edu2010 - demo.data$edu1990
  demo.data$popdiff <- demo.data$pop2010 - demo.data$pop2000

  name1 <- unique(indiana$subregion)
  name2 <- unique(demo.data$subregion)
  cbind(name1,name2)
  demo.data$subregion <- name1

  indiana <- inner_join(indiana, demo.data, by = "subregion")

  plot <- ggplot(data = indiana) +
  geom_polygon(aes(x = long, y = lat, fill = pop2010, group = group), color = "white") +
  coord_fixed(1.3) +
  scale_fill_gradient(trans = "log10", labels=scales::comma, high = "#132B43", low = "#56B1F7") +
  labs(fill = "2010 Population") +
  map_theme

  #show plot
  plot
}


plot_positiveTests()
