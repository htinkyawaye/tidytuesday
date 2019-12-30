require(tidyverse)

christmas_songs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-24/christmas_songs.csv")

songs <- christmas_songs %>%
  select(song, songid, peak_position, weeks_on_chart, year) %>%
  distinct() 

first_entry <- christmas_songs %>%
  group_by(songid) %>%
  slice(which.min(year)) %>%
  select(songid, "first" = year)

songs <- merge(songs, first_entry, by= "songid")

xmas_plot <- ggplot(songs, aes(x=as.factor(year), y=reorder(song, first), color="red", size=weeks_on_chart, alpha=peak_position)) +
  geom_point() +
  scale_x_discrete(breaks=seq(1950, 2020, 10)) +
  scale_alpha_continuous(trans="reverse") +
  theme_minimal() + 
  theme(text = element_text(family="Raleway"), 
        plot.title = element_text(size=20, face="bold"),
        legend.position = "top",
        plot.background = element_rect(fill="#e5f5e0"),
        panel.grid = element_line(color = "black", size = 0.1, linetype = "dotted"),
        plot.margin=unit(c(1,1,1,0),"cm")) +
  labs(title="100-Hit Christmas Songs since 1958", 
       subtitle = "Two oldies songs still rocking around in 21st Century \nMariah Carey is Christmas Queen",
       x="Year",
       y="",
       alpha="Peak Position",
       size="Weeks on Chart",
       caption = "Source: Billboard Top 100 | Graphic: Htin Kyaw Aye") +
  guides(colour=F)
  
xmas_plot

ggsave("ttxmas.png", width = 16, height = 14, units="in", dpi=300)

