require(foreign)
require(tidyverse)
require(ggnewscale)

descriptions <- read.csv("dog_descriptions.csv")

data <- descriptions %>% 
  select(c("breed_primary", "breed_secondary")) %>%
  mutate(breed_secondary=as.character(breed_secondary)) %>%
  mutate(breed_primary=as.character(breed_primary)) %>%
  group_by(breed_primary, breed_secondary) %>%
  count() %>%
  rename(count=n) %>%
  ungroup()

features_data <- descriptions %>% 
  select(c("age", "sex", "size")) %>%
  group_by(sex, age, size) %>%
  count() %>%
  rename(count=n)

features_data$fraction <- features_data$count / sum(features_data$count)
features_data$ymax <- cumsum(features_data$fraction)
features_data$ymin <- c(0, head(features_data$ymax, n=-1))


ggplot(features_data) + 
  geom_rect(aes(fill=size, ymax=ymax, ymin=ymin, xmax=5, xmin=4), color="white", size=.5, alpha=0.7) +
  scale_fill_manual(values=c("#a6611a", "#dfc27d", "#80cdc1", "#018571"),
                    guide=guide_legend(title="size", order=3)) +
  new_scale_fill() +
  geom_rect(aes(fill=age, ymax=ymax, ymin=ymin, xmax=4, xmin=3), color="white", size=.5, alpha=0.75) +
  scale_fill_manual(values=c("#d01c8b", "#f1b6da", "#b8e186", "#4dac26"),
                    guide=guide_legend(title="Age", order=2)) +
  new_scale_fill() +
  geom_rect(aes(fill=sex, ymax=ymax, ymin=ymin, xmax=3, xmin=2), color="white", size=.5, alpha=0.8) +
  scale_fill_manual(values=c("#af8dc3", "#7fbf7b", "#f7f7f7"),
                    guide=guide_legend(title= "Sex", order=1)) +
  xlim(c(0, 5)) + 
  theme(aspect.ratio=1,
        legend.position = "top",
        legend.direction = "vertical") +
  coord_polar(theta = "y") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family="Raleway", size = 15),
        plot.title = element_text(face="bold")) +
  labs(title="Adoptable Dogs by Sex, Age and Size",
       subtitle = "Medium- and large-sized adult doggos, both male and femal, \nare most up for adoption",
       caption ="Source: Petfinder via The Pudding | Graphic: Htin Kyaw Aye") 

ggsave("ttdoggo.png", width = 8, height = 10, units="in", dpi=300)
