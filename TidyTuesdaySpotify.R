require(tidyverse)
require(reshape2)
require(ggchicklet)
require(lubridate)

spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

genre_optimized <- spotify_songs %>%
  select(c(track_popularity, track_name, track_album_release_date, playlist_genre, duration_ms)) %>%
  arrange(desc(-duration_ms)) %>%
  group_by(playlist_genre) %>%
  mutate(cum_sum = cumsum(duration_ms)) %>%
  filter(cum_sum <= 1800000) %>%
  mutate(year = year(as.Date(track_album_release_date))) %>%
  mutate(recent_year = case_when(
    year < 2019 ~ "Earlier",
    TRUE ~ "2019"
  ))

ggplot(genre_optimized) +
  geom_chicklet(aes(playlist_genre, duration_ms, group = duration_ms, fill = recent_year, alpha = track_popularity)) +
  geom_hline(yintercept = 1800000, linetype = "dotted") +
  annotate(geom = "text", 
           y = 1890000, 
           x = 6.5, 
           label = "30 Mins",
           family = "Raleway",
           size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = c("#31a354", "#636363")) + 
  theme_minimal() + 
  theme(text = element_text(family="Raleway"),
        axis.title = element_blank(),
        axis.text.x  = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(face="bold", size = 15),
        legend.position = "top", 
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background = element_rect(fill="#f0f0f0")) +
  labs(title = "30 Minutes of Uninterrupted Listening",
       subtitle = "Maximum possible number of full tracks you could listen in each genre \non Spotify Free after an ad",
       caption = "Data: Spotifyr | Graphic: Htin Kyaw Aye",
       alpha = "Track Popularity",
       fill = "Release Year")

ggsave("TidyTuesdaySpotify.png", height = 6, width = 8, unit = "in", dpi = 300)


playlist_optimized <- spotify_songs %>%
  select(c(track_popularity, track_name, track_album_release_date, playlist_name, duration_ms)) %>%
  arrange(desc(-duration_ms)) %>%
  group_by(playlist_name) %>%
  mutate(cum_sum = cumsum(duration_ms)) %>%
  filter(cum_sum <= 1800000) %>%
  mutate(year = year(as.Date(track_album_release_date))) %>%
  mutate(recent_year = case_when(
    year < 2019 ~ "Earlier",
    TRUE ~ "2019"
  ))

ggplot(playlist_optimized) +
  geom_chicklet(aes(playlist_genre, duration_ms, group = duration_ms, fill = recent_year, alpha = track_popularity)) +
  geom_hline(yintercept = 1800000, linetype = "dotted") +
  annotate(geom = "text", 
           y = 1890000, 
           x = 6.5, 
           label = "30 Mins",
           family = "Raleway",
           size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = c("#31a354", "#636363")) + 
  theme_minimal() + 
  theme(text = element_text(family="Raleway"),
        axis.title = element_blank(),
        axis.text.x  = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(face="bold", size = 15),
        legend.position = "top", 
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background = element_rect(fill="#f0f0f0")) +
  labs(title = "30 Minutes of Uninterrupted Listening",
       subtitle = "Maximum possible number of full tracks you could listen in each genre \non Spotify Free after an ad",
       caption = "Data: Spotifyr | Graphic: Htin Kyaw Aye",
       alpha = "Track Popularity",
       fill = "Release Year")

