require(treemapify)
require(tidyverse)

data <- read_csv("mandalay_20.csv") %>%
  filter(!is.na(value))

ministry_budget <- data %>%
  filter(budget_category == "ဝန်ကြီး၊ ဦးစီးဌာနများ") %>%
  select(parent_ministry, flow, value) %>%
  group_by(parent_ministry, flow) %>%
  summarise(value=sum(value))

mandalay_budget_plot <- ggplot(ministry_budget, aes(area=value, fill=parent_ministry, label = value)) +
  facet_wrap(.~flow) +
  geom_treemap(size=2, color="#ffffff", linetype="dashed", alpha=.9) +
  geom_treemap_text(padding.x = grid::unit(3, "mm"),
                    padding.y = grid::unit(3, "mm"),
                    family = "Raleway",
                    color = "#525252") +
  scale_fill_brewer(palette = "YlOrRd") +
  guides(fill=guide_legend(ncol=3)) +
  theme(text = element_text(family = "Myanmar Sabae"), 
        plot.title = element_text(size="20"),
        strip.background = element_blank(),
        strip.text = element_text(family="Raleway", face = "bold", size = "10"),
        legend.position = "bottom",
        plot.margin=unit(c(1,1,1,1),"cm")) +
  labs(title="မန်းတိုင်းအစိုးရ ဝန်ကြီးဌာနအလိုက် ဘတ်ဂျက် ၂၀၂၀",
       subtitle = "\nမန္တလေးစည်ပင်နဲ့ ဆောက်လုပ်ရေးဝန်ကြီးဌာနလက်အောက် ဦးစိးဌာနတွေက ဘတ်ဂျက်ပမာဏအများဆုံးကို\nအကောင်အထည်ဖော်မှာ ဖြစ်ပါတယ်။ (ကျပ်သန်းပေါင်း)",
       fill = "ဝန်ကြီးဌာနများ",
       caption = "ရင်းမြစ်။ မန်းတိုင်းအစိုးရဘတ်ဂျက် | ဂရပ်။ ထင်ကျော်အေး")
mandalay_budget_plot

ggsave("ttown.png", width = 12, height = 8, units="in", dpi=300)
