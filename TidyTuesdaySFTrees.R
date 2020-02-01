require(tidyverse)

sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')

d <- sf_trees %>%
  filter(dbh > 0) %>%
  select(legal_status, caretaker, dbh) %>%
  group_by(legal_status, caretaker) %>%
  summarise(avg_dbh = mean(dbh)) %>%
  add_tally() %>%
  filter(!is.na(legal_status))


ggplot(d) +
  geom_point(aes(legal_status, avg_dbh, size = n, color = legal_status)) +
  scale_color_brewer(palette = "BuGn") +
  geom_segment(aes(x = legal_status, xend = legal_status, y = 0, yend = avg_dbh)) +
  facet_wrap(.~caretaker) +
  theme_minimal() +
  theme(text = element_text(family = "Raleway"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = .5, linetype = "dotted", color = "#cfc7de"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 15, face = "bold"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        legend.position = "bottom",
        plot.background = element_rect(fill = "#a6bec8")) +
  guides(color=guide_legend(ncol=3), size=guide_legend(ncol=2)) +
  labs(color = "Legal Status",
       y = "Average Depth, Height",
       size = "# of Trees", 
       title = "The Forest of San Francisco",
       subtitle = "Hats off to the people of San Francisco for taking care of the city's trees, short and tall!", 
       caption = "Data: DataSF | Graphic: Htin Kyaw Aye")

ggsave("TidyTuesdaySFTrees.png", width = 12, height = 8, units = "in", dpi = 300)
