require(tidyverse)
passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

data <- passwords %>% 
  filter(!is.na(value)) %>%
  mutate(n=1)

top_picks <- passwords %>%
  filter(!is.na(value)) %>%
  group_by(category, password) %>%
  mutate(popularity = 501 - rank) %>%
  tally(popularity) %>%
  top_n(1) %>%
  mutate(top_1 = TRUE) %>%
  ungroup() %>%
  select(password, top_1)

pwd <- left_join(data, top_10_picks, by = "password") %>%
  mutate(top_1 = case_when(
    is.na(top_1) ~ 1,
    TRUE ~ 0
  )) %>%
  arrange(desc(rank))

pwd_order <- passwords %>%
  group_by(category) %>%
  tally()
  

ggplot(pwd, aes(fct_relevel(category, "name", 
                            "cool-macho", 
                            "simple-alphanumeric", 
                            "fluffy", 
                            "sport", 
                            "nerdy-pop",
                            "animal",
                            "password-related",
                            "food",
                            "rebellious-rude"), n, label = password, color = top_1)) +
  geom_text(position = "stack", stat = "identity", size = 5, family = "Raleway ExtraBold") +
  scale_colour_gradient(low = "#b2182b", high = "#f4a582") +
  annotate(geom="text", x=5.5, y=122, label = "Most popular in each category",
           hjust = 0.55,
           family = "Raleway ExtraBold",
           color = "#b2182b",
           size = 4) +
  geom_curve(aes(x = 3, y = 65, xend = 4.2, yend = 120),
             colour = "#555555", 
             curvature = 0,
             size=0.3,
             linetype = "dotted") +
  geom_curve(aes(x = 8, y = 20, xend = 6.6, yend = 120),
             colour = "#555555", 
             curvature = 0,
             size=0.3,
             linetype = "dotted") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family="Raleway"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(face="bold", size = 16),
        plot.margin = unit(c(1,1,1,1), "cm")) +
  labs(x = "Category",
       y = "# of Password",
       title = "Share love, not password", 
       subtitle = "Michael, you should stop setting your name as password!!!",
       caption = "Data: Knowledge is Beautiful | Graphic: Htin Kyaw Aye")

ggsave("TidyTuesdayPwd.png", height = 8, width = 12, unit = "in", dpi = 300)
  