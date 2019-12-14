require(foriegn)
require(tidyverse)
require(waffle)
require(hrbrthemes)

uk_results <- read.csv("uk_2019_elections.csv")

results_plot <- ggplot(uk_results, aes(fill=reorder(Party, desc(Seat)), values=Seat)) +
  geom_waffle(color = "white", size=0.5, n_rows = 5, radius = unit(4, "pt")) +
  geom_vline(xintercept = 65.5, size = 0.5, colour = "black", linetype = "dotted") +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  scale_fill_manual(values = c("#0084c6", "#c70000", "#ffe500", "#767676","#ee6f00", "#3db540")) + 
  coord_equal() +
  labs(
    title = "UK Election Results 2019",
    subtitle = "Boris Johnson to remain as PM \nafter leading the Conservatives to comfortable victory",
    caption = "Source: The Guardian | Graphic: Htin Kyaw Aye"
  ) +  
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  theme(strip.text  =element_text(family="Raleway"), 
        plot.title =element_text(family="Raleway"), 
        plot.subtitle =element_text(family="Raleway"), 
        text=element_text(family="Raleway"), 
        legend.key.size = unit(1,"line"), 
        legend.position = "top",
        legend.title = element_blank(), 
        legend.justification = "left") +
  guides(fill=guide_legend(nrow=1)) +
  geom_label(aes(x = 65.5, 
                 y = -2, 
                 label = "326 to win"),
             label.size = NA,
             lineheight=0.8,
             colour = "#555555", 
             fill = "white") +
  ylim(-2,NA) 

results_plot

