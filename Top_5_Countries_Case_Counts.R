#Weekly Cases by Country 
#Author: Lance R. Owen
#This script creates a function that takes a start and end date as arguments and returns a bar graph of total weekly COVID 
#cases with the top five countries by total cases indicated in distinctive and sophisticated colors. Note that weeks are 
#designated as starting on Sunday.

options(scipen = 999)
library(zoo)
library(extrafont)
library(lubridate)
library(patchwork)
library(dplyr)
library(magrittr)
library(SaviR)
library(patchwork)
library(tmap)
library(geojsonio)

font = "sans serif"

weekly_covid_totals_top5 <- function(start_date, end_date) {
  caption.date <- format(Sys.Date()-1, format="%d %B %Y")
  start.date.title <- format(as.Date(start_date), "%B %d")
  end.date.title <- format(as.Date(end_date), "%B %d, %Y")
  
  #Pull COVID data using SaviR package function
  ncov_data <- get_covid_df()
  
  #filter to cases between start and end dates using sources from WHO
  totals <- ncov_data %>% 
    filter(source == "WHO") %>% 
    filter(date >= as.Date(start_date) & date <= as.Date(end_date))
  totals$week <- floor_date(totals$date, "week")
  
  #summarize by week and create week character column for x-axis labels
  weekly <- totals %>% 
    group_by(iso2code, country, week)%>% 
    summarise_if(is.numeric, sum) %>% 
    filter(week >= start_date & week <= end_date)
  weekly$week_char <- format(weekly$week,"%b %d")
  
  #get top five countries by total case counts for time span
  top5 <- weekly %>% 
    group_by(country) %>% 
    summarise_if(is.numeric, sum) %>% 
    select(country, new_cases) %>% 
    top_n(5, new_cases) %>% 
    arrange(desc(new_cases))
  
  #filter top five countries
  countries <- weekly %>% 
    filter(country %in% top5$country)
  countries <- countries %>% 
    select(iso2code, country, week, new_cases, week_char)
  
  #filter all but top five countries for "other" desgination
  other <- weekly %>% 
    filter(!country %in% top5$country) %>% 
    group_by(week) %>%
    summarise_if(is.numeric, sum)
  
  #assign "All Other Countries" as name for total exclusive of top five countries  
  other$country = "All Other Countries"
  other <- cbind(other$country, other)
  other$week_char <- format(other$week, "%b %d")
  other <- other %>% 
    select(country, week, new_cases, week_char)
  
  #bind two dataframes together
  final.df <- rbind(countries, other)
  
  #get global totals for bar labels
  final.df.totals <- final.df %>% 
    group_by(week_char) %>% 
    summarise_if(is.numeric, sum)
  
  #get list of top five countries in order or case totals and append "All Other Countries" for legend labels
  levels <- as.list(top5$country)
  levels <- append(levels, "All Other Countries")
  
  #set levels of country column for legend/bar color order
  final.df$country <- factor(final.df$country, levels = levels)
  
  #plot
  week_country.plot <- ggplot(data=final.df) +
    geom_bar(aes(x=reorder(week_char, week), y = new_cases, fill=country), position="stack", stat='identity', alpha=.75, size=.1, width = .75) +
    geom_text(data = final.df.totals, aes(x = week_char, y = new_cases, label = format(new_cases, big.mark = ","), family = font, fontface = "bold"), vjust=-.75, size = 5, color = "grey40") +
    scale_fill_manual(values = c("#F74638", "#F7C080", "#09B59A", "#179CC7", "#145D76", "#C8C7C7"))+
    scale_y_continuous(expand = c(0, 0), labels = function(x) format(x, big.mark = ","), limits = c(0,1.1*max(final.df.totals$new_cases))) + 
    scale_x_discrete(labels= paste("Week of\n",final.df$week_char)) +
    labs(x = "", y = "Total New Confirmed Cases", 
         caption = paste0("Source: WHO | Data as of ", caption.date)) +
    ggtitle(paste0("COVID-19 GLOBAL CASES | Total New Confirmed Cases (", start.date.title, " - ", end.date.title, ")"), 
            subtitle = "Aggregated by week with top five countries by total cases indicated") +
    theme(plot.title = element_text(family = font, face = "bold", size = 20, hjust = 0, color = "grey20", 
                                    margin = margin(t = 0, r = 0, b = 7, l = 0)),
          plot.subtitle = element_text(family = font, face = "italic", size = 18, hjust = 0, color = "grey35",
                                       margin = margin(t = 0, r = 0, b = 15, l = 0)),
          rect = element_rect(color = "white"),
          panel.grid = element_line(color="white"),
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(color="grey80", linetype = "dashed"), 
          panel.grid.minor.y = element_line(color="grey80", linetype = "dashed"),
          plot.background = element_rect(fill="white"),
          plot.caption = element_text(family = font, size = 14, color = "grey35", 
                                      margin = margin(t = 15, r = 0, b = 0, l = 0)),
          legend.box = "vertical",
          legend.position = "right",
          legend.key = element_rect(fill = "white"),
          legend.title = element_blank(),
          legend.text = element_text(family = font, size = 12, color = "#2C2828", face="bold"),
          axis.line.x = element_line(color="#2C2828"),
          axis.line.y = element_line(color="#2C2828"),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(family = font, size = 12, color = "#2C2828", face="bold",margin = margin(t = 0, r = 15, b = 0, l = 0)),
          axis.text.x = element_text(family = font, size = 12, color = "#2C2828", face="bold"),
          axis.text.y = element_text(family = font, size = 12, color = "#2C2828", face="bold"),
          axis.title.x = element_blank())
  
  return(week_country.plot)
}

#When using the function, it is best to use a start date that occurs on a Sunday and and end date that occurs on a Saturday.
#Dates must be entered as arguments in %Y-%m-%d format. Example: For March 1, 2021, users should enter "2021-03-01"
graph <- weekly_covid_totals_top5("2022-03-03", "2022-05-28")

graph

