# Load Packages
library(tidyverse)
library(ggplot2)
library(tidytuesdayR)
library(janitor)
library(stringr)
library(lubridate)
library(extrafont)
library(usmap)
library(maps)
library(leaflet)
library(mapview)
library(leaflet.extras)
webshot::install_phantomjs()

# Load Data 
tidy_data <- tidytuesdayR::tt_load(2023, week = "20")
tornados <- tidy_data$tornados

# take a look at data types
glimpse(tornados)

# change the st column name to state for better understanding 
tornados <- tornados %>% 
  rename("state" = st)

# view fonts 
loadfonts()
fonttable()


# Before we dive deep into this data, lets see how many tornados there were
# during this time period 
tornados %>% 
  count()
# There were a total of 68,693 tornados from 1950 to 2022

# Let's take a look at the number of tornados by year 
yearly_tornados <- tornados %>% 
  group_by(yr) %>% 
  count()

# Create a line chart from this segmented data frame 
yearly_tornados %>% 
  ggplot(aes(x=yr, y=n))+
  geom_line(color = "#DA2528")+
  labs(
    title = "Tornados through the Years",
    subtitle = "The number of tornadoes has risen the past seventy years",
    caption = "Period 1950 - 2022 | Data: NOAA | Viz: JellyJNBA",
    x= "",
    y= "Number of fatalities")+
  theme_minimal()+
  theme(text = element_text(family = "Consola Bold", color = 'black'),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 12),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank())

# in what states are tornados occurring 
tornadoes_st <- tornados %>% 
  group_by(state) %>% 
  summarize(value = n()) %>% 
  arrange(desc(value))
# Texas leads all other states by a wide margin.
# Kansas, Okalhoma, Florida, and Nebraska round out the Top five

glimpse(tornadoes_st)

# add a case_when statement to idnentify the levels within our data
tornadoes_st <- tornadoes_st %>% 
  mutate(color = case_when(value < 500 ~ "Less than 500",
                           value <= 1000 ~ "501 - 1,000",
                           value <= 2000 ~ "1,001 - 2,000",
                           value <= 4000 ~ "2,001 - 4,000",
                           value > 4000 ~ "More than 4,000",
                           TRUE ~ "No Tornadoes"))
# Arrange color levels 
tornadoes_st$color <- fct_relevel(tornadoes_st$color, c("More than 4,000",
                                                        "2,001 - 4,000",
                                                        "1,001 - 2,000",
                                                        "501 - 1,000",
                                                        "Less than 500"))


# plot data 
tornado_map <- plot_usmap(data = tornadoes_st, values = "color", labels = FALSE)+
  scale_fill_manual(values = c( "Less than 500" = "#ffdfa4",
                                "501 - 1,000" = "#FFC457",
                                "1,001 - 2,000"= "#E4683F",
                                "2,001 - 4,000"= "#C03434",
                                "More than 4,000" = "#98103E",
                                "No Tornados" = "grey70"))+
  labs(title = "Number of Tornadoes by State",
       subtitle = "Period 1950 - 2022 | Data: NOAA | Viz: JellyJNBA",
       fill = "color",
       x = "",
       y = "")+
  theme(plot.title = element_text(margin = margin(b = 20, t=15), 
                                  family = "Calibri Bold",
                                  color = "#22222b",
                                  face = "bold",
                                  size = 25,
                                  hjust = 0.5),
        plot.subtitle = element_text(family = "Calibri Bold",
                                     color = "#22222b",
                                     size = 15,
                                     hjust = 0.5, 
                                     vjust = 4),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text=element_text(family = "Calibri Bold",
                                 size = 8), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x=element_blank()) +
  guides(fill = guide_legend(
    label.position = "top",
    family = "Calibri Bold", 
    color = "#808080", nrow = 1,
    keywidth = 2, keyheight = 0.5))

ggsave("tornado_map.png", tornado_map, dpi = 300, width = 6, height = 4)

# we know where these tornadoes are happening within the USA, but what time of the year are they occurring?
month <- tornados %>% 
  group_by(mo) %>% 
  summarize(value = n())

mymonths <- c("Jan", "Feb", "Mar",
              "Apr", "May", "Jun",
              "Jul", "Aug", "Sep",
              "Oct", "Nov", "Dec")

month$abv <- mymonths[month$mo]

# view updated data 
month

# plot this data 
month %>% 
  ggplot(aes(x=abv, y=value))+
  geom_col(fill = "98103E")+
  scale_x_discrete(limits = month$abv)+
  labs(title = "Number of Tornadoes by Month",
       subtitle = "The month of May saw almost 15,000 tornadoes, by far the most in any month",
       caption = "Period 1950 - 2022 | Data: NOAA | Viz: JellyJNBA",
       x = "Month",
       y= "Number of Tornadoes")+
  theme_minimal()+
  theme(text = element_text(family = "Calibri Bold", color = 'black'),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 12),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank())


# Fatalities 
# Count the number of fatalities 
tornados %>% 
  summarize(fatalities = sum(fat))
# 6112 people died at the hands of a tornado between 1950-2022

# Summarise average fatalities by type 
yearly_tornado_fat <- tornados %>% 
  group_by(yr) %>% 
  summarize(n = sum(fat))

# Plot data
ggplot(yearly_tornado_fat, aes(yr, n)) +
  geom_line(color = "#98103E") +
  labs(title = "Tornado Fatalities through the Years",
       subtitle = "The number of fatalities has fluctuated over the past seventy years",
       caption = "Period 1950 - 2022 | Data: NOAA | Viz: JellyJNBA",
       x = "",
       y = "Number of fatalities") +
  theme_minimal() +
  theme(text = element_text(family = "Calibri Bold", color = 'black'),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 12),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank())

# the number of fatalities by a tornado rose in the past seventy years. 2011 saw the most fatalities 
# with 553 deaths

# average number of fatalities per mag rating 
mean_fat <- tornados %>% 
  group_by(mag) %>% 
  filter(!mag == -9) %>% 
  summarize(mean_fat = round(mean(fat), 2))

# plot data 
ggplot(mean_fat, aes(mag, mean_fat)) +
  geom_col(fill = "#98103E") +
  scale_x_continuous(breaks = c(0,1,2,3,4,5)) +
  labs(title = "Average Tornado Fatalities by EF Rating",
       subtitle = "EF5 Tornadoes cause more fatalities than the rest of them combined",
       caption = "Period 1950 - 2022 | Data: NOAA | Viz: JellyJNBA",
       x = "EF Rating",
       y = "Avg. Fatalities") +
  theme_minimal() +
  theme(text = element_text(family = "Calibri Bold", color = 'black'),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 12),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank())

# map where mag 5 tornadoes start and end 
tornado_locations <- tornados %>% 
  filter(mag == 5) %>% 
  select(yr, slat, slon, elat, elon) %>% 
  drop_na()

# Prepare the map of the United States using the maps package
map_usa <- map_data("state")

# create scatter plot 
map <- leaflet(tornado_locations) %>% 
  addTiles() %>% 
  addCircleMarkers(~slon, ~slat, color = "red", radius = 3, stroke = FALSE, label = ~yr, labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "top", offset = c(0, -10), style = list("font-weight" = "bold", "font-size" = "10px"))) %>%
  addCircleMarkers(~elon, ~elat, color = "blue", radius = 3, stroke = FALSE) %>%
  addLegend(colors = c("red", "blue"), labels = c("Starting Location", "Ending Location"), title = "Tornado Locations") %>%
  setView(lng = -98.5833, lat = 39.8333, zoom = 4)  # Set the initial view to focus on the United States

mapshot(map, file = "tornado_location.png", remove_controls = TRUE, width = 1000, height = 600)
