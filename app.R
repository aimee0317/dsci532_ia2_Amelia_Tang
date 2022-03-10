library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(plotly)
library(tidyverse)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)


## Load and wrangle the data 

xbox <- read.csv("data/xbox.csv") %>% drop_na()
ps4 <- read.csv("data/ps4.csv") %>% drop_na()

xbox_NA <- xbox  %>%
  select(Year, North.America)  %>%
  group_by(Year)  %>%
  summarise(NA_sales = sum(North.America))  %>%
  slice(1:6)

ps4_NA <- ps4  %>%
  select(Year, North.America)  %>%
  group_by(Year)  %>%
  summarise(NA_sales = sum(North.America))  %>%
  slice(1:6)


year_range <- seq(min(ps4_NA$Year), max(ps4_NA$Year))
year_range_label <- setNames(as.list(as.character(year_range)), as.integer(year_range))


app$layout(
  dbcContainer(
    list(
      htmlBr(),
      dccGraph(id='sales_plot'),
      dccSlider(
        id="slider-year",
        min=min(xbox_NA$Year),
        max=max(xbox_NA$Year),
        step=1,
        value=max(xbox_NA$Year),
        marks=year_range_label,
        tooltip=list(
          always_visible=TRUE,
          placement="top"
        )
      ),
      htmlBr(),
      htmlP("Company Name for Sales Trend plot"),
      dccDropdown(
        id='company-select',
        options = list('ps4', 'xbox', "ps4 + xbox"), 
        value='ps4')
    )
  )
)

app$callback(
    output('sales_plot', 'figure'),
    list(input('company-select', 'value'),
         input("slider-year", "value")),
    function(company, year) {
      if (company == 'ps4') {
        ps4_NA_yr <- ps4_NA  %>%
          filter(Year <= year)  %>%
            rename(Sales = NA_sales)
        p <- plot_ly(ps4_NA_yr, x = ~Year, y = ~Sales, type = 'scatter', mode = 'lines')  %>%
         layout(title =('Sales Trend'))
      } else if (company == 'xbox'){
        xbox_NA_yr <- xbox_NA  %>%
          filter(Year <= year)  %>%
            rename(Sales = NA_sales)
        p <- plot_ly(xbox_NA_yr, x = ~Year, y = ~Sales, type = 'scatter', mode = 'lines')  %>%
            layout(title =('Sales Trend'))
      } else { 
         NA_sales_plot <- merge(ps4_NA, xbox_NA, by = 'Year') %>%
             rename(Sales = NA_sales.x)  %>%
             filter(Year <= year)
         p <- plot_ly(NA_sales_plot, x = ~Year, y = ~Sales, name = "PlayStation4", type = 'scatter', mode = 'lines') 
         p <- p  %>% add_trace(y = ~NA_sales.y, name = 'XBox', mode = 'lines')  %>%
             layout(title =('Sales Trend'))
      }
      ggplotly(p)
    }
)

app$run_server(host = '0.0.0.0')
