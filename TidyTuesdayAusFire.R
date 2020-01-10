require(tidyverse)
require(lubridate)

temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')
bushfires <- read_csv("aus_bushfires.csv")

temperature_cleaned <- temperature %>% 
  filter(temp_type == "max") %>%
  mutate(year = year(date)) %>%
  mutate(month_n = month(date)) %>%
  mutate(month = factor(months(date), levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))) %>%
  group_by(year, month, month_n) %>%
  summarise(temp = mean(temperature, na.rm = T)) %>%
  mutate(month_year = paste0(month, " ", year))

bushfires_cleaned <- bushfires %>%
  mutate(year = str_extract(date, "\\d{4}")) %>%
  mutate(date_new = gsub("-", " ", date)) %>%
  mutate(date_new = gsub("–", " ", date)) %>%
  mutate(date_new = gsub("-", " ", date)) %>%
  mutate(month = str_trim(str_extract(date_new, "January|February|March|April|May|June|July|August|September|October|November|December|New"))) %>%
  mutate(month = gsub("New", "January", month)) %>% 
  mutate(month_year = paste0(month, " ", year)) %>%
  mutate(bushfire = "Yes") %>%
  filter(!is.na(month)) %>%
  select(ha, month_year, bushfire) 

cleaned_data <- left_join(temperature_cleaned, bushfires_cleaned, by = "month_year") %>%
  mutate(bushfire = case_when(
    is.na(bushfire) ~ "No",
    TRUE ~ "Yes"
  )) %>%
  mutate(ha = case_when(
    is.na(ha) ~ 0,
    TRUE ~ ha
  ))

clean <- cleaned_data %>%
  filter(year > 1960)


ggplot(clean) + 
  geom_tile(aes(year, reorder(month, -month_n), fill=temp), linetype = "dotted", color = "white", size = .2) +
  scale_fill_gradientn(colors=c("#2c7bb6", "#abd9e9", "#fdae61", "#d7191c")) +
  scale_x_discrete(breaks=seq(1960, 2020, 10), limits = seq(1960, 2020, 10)) +
  theme_minimal() +
  theme(aspect.ratio = 1/4,
        panel.grid = element_blank(),
        text = element_text(family="Raleway"),
        plot.margin=unit(c(1,1,1,1),"cm"),
        plot.title = element_text(face="bold"),
        legend.key.width = unit(.5, "cm")) +
  geom_point(aes(year, reorder(month, -month_n), color=bushfire), fill = "white") +
  scale_color_manual(values=c("#ffffff00", "#ffffff"), guide = F) +
  annotate(geom="text", x=2010, y=14, label = "Months bushfires started",
           hjust = 0.55,
           family = "Raleway",
           size = 3.5) +
  expand_limits(y=c(1,15)) +
  geom_curve(aes(x = 2003, y = 13, xend = 2004, yend = 14),
             colour = "#555555", 
             curvature = -0.3,
             size=0.3,
             linetype = "dotted") +
  geom_curve(aes(x = 2016, y = 13, xend = 2015, yend = 14),
             colour = "#555555", 
             curvature = 0.3,
             size=0.3,
             linetype = "dotted") +
  labs(title = "Australia Bushfires Through Time",
       subtitle = "Bushfires becoming more frequent in last two decades \nAverage maximum temperature is also on the increase",
       fill = "Average Maximum \nTemperature",
       x = "Year",
       y = "Month",
       caption = "Data: Australian Bureau of Meterology, Wikipedia | Graphic: Htin Kyaw Aye")

