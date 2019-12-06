require(foreign)
require(tidyverse)
require(lubridate)

tickets <- read.csv("tickets.csv")
tickets$issue_datetime <- parse_date_time(tickets$issue_datetime, "ymd hms")

top_5_violation <- tickets %>% 
  group_by(violation_desc) %>%
  tally() %>%
  top_n(5)

tickets_violate <- tickets %>%
  mutate(Violation = dplyr::case_when(
    !violation_desc %in% top_5_violation$violation_desc ~ "Others",
    violation_desc %in% top_5_violation$violation_desc ~ as.character(violation_desc)
  )) %>%
  mutate(hour = hour(issue_datetime)) %>%
  group_by(Violation, hour) %>%
  tally()


tickets_plot <-   ggplot(tickets_violate, aes(hour, n, fill = Violation)) +
  geom_bar(width = 1, position = "stack", stat = "identity", color = "white") +
  coord_polar(start = -0.15) + 
  scale_fill_brewer(palette = "Spectral") +         
  theme(legend.position = "right",
        text = element_text(family = "Raleway"),
        legend.background = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_line(color = "black", size = 0.1, linetype = "dotted"),
        panel.background = element_blank(),
        plot.title = element_text(face = "bold", size = 14)) +
  labs(title="Tickets by Hour", 
       subtitle = "Most tickets are given at noon and \nMeter Experiation is the most frequent type of violation",
       caption = "Source: Open Data Philly | Graphic: Htin Kyaw Aye") +
  guides(fill=guide_legend(override.aes = list(shape=19)))
tickets_plot

ggsave("tickets_plot.png", width = 6, height = 4)