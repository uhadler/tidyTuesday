measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')

summary(measles)
library(tidyverse)
library(ggplot2)
library(extrafont)
library(patchwork)
loadfonts()
theme_set(theme_minimal())


data <- measles %>%
  filter(enroll > 0, !is.na(type), mmr >= 0) %>%
  distinct()

rates <- data %>%
  select(type, xrel, xmed, xper) %>%
  mutate(xrel = as.numeric(xrel)) %>%
  pivot_longer(cols = xrel:xper, names_to = "reason", values_to = "value") %>%
  filter(!is.na(value)) %>%
  group_by(type, reason) %>%
  summarize(avg = mean(value), se = sd(value)/sqrt(n()), na.rm=TRUE) %>%
  mutate(reason = ifelse(reason == "xmed", "Medical reasons", "Personal reasons"))
  
label <- data.frame(x=0.5, y=10, reason = "Medical reasons",
                    label="Exemption rates above 5% may compromise herd\n immunity and put children at risk.")
                    
                  
arrow <- data.frame(x=1.75, y= 8.75, xend = 2.5, yend = 5.25, reason = "Medical reasons")



boxplot <- ggplot(data) + 
  geom_hline(yintercept=95, linetype = "dashed") +
  geom_boxplot(aes(x = as.factor(type), y=mmr, fill = type),
               varwidth = TRUE, notch = TRUE, alpha = 0.75,
               outlier.alpha = 0, key_glyph = "dotplot") + 
  geom_jitter(aes(x=as.factor(type), y=mmr, colour = mmr),
              width = 0.08, height = 0.5, alpha = 0.15) +
  annotate(geom="text", y=45, x=1.8, hjust=0, vjust=1,
           label = "The CDC estimates that a 93% to 95% vaccination rate\n is enough to provide herd immunity for measles, but\n that theshold is often not met.")+
  annotate(geom="curve", x= 1.45, y=80, xend = 1.33, yend=94,
           curvature = .2, arrow = arrow(length = unit(1, "mm"))) + 
  coord_flip() + 
  scale_colour_gradient2(low="#FC4E07", mid = "#E7B800", high="#00AFBB",
                        limits = c(90,95), oob=scales::squish, midpoint=92.5) +
  viridis::scale_fill_viridis(option = "D", discrete = TRUE) +
  labs(y="MMR Vaccination Rate (%)", x="Type of school", colour = "Vaccination Rate", fill = "Type of school") + 
  theme(text = element_text(family="Calibri"),
        legend.position = "bottom") + 
  guides(colour = guide_colourbar(barwidth = 10, barheight = 0.5,
                                  ticks = FALSE, title.vjust = 1))

barplot <- ggplot(rates) +
  geom_bar(aes(x=type, y=avg, fill = type), stat="identity") + 
  geom_errorbar(aes(x=type, ymin=avg-se, ymax = avg+se), width = 0.25) + 
  geom_hline(yintercept = 5, linetype = "dashed") +
  geom_text(data=label, aes(x=x, y=y, label=label),
            hjust = 0 ) +
  geom_curve(data=arrow, aes(x=x, y=y, xend=xend, yend=yend),
             curvature = 0.2, arrow = arrow(length=unit(1,"mm"))) +
  viridis::scale_fill_viridis(option="D", discrete=TRUE) +
  facet_grid(~reason) + 
  labs(x="Type of school", y="Average Vaccination exemptions (%)") + 
  theme(text=element_text(family="Calibri"),
        legend.position = "none",
        strip.background = element_rect(fill="#a9a9a922", colour="#00000000"),
        strip.text = element_text(size = 10, face="bold"))


(boxplot / barplot) + plot_layout (heights = c(2,1)) + 
  plot_annotation(title = "Low vaccination rates at charter schools may compromise herd immunity.",
                  subtitle = str_wrap("Charter schools show high vaccination exemption rates and report the lowest MMR vaccination rates. Just over every fourth charter school reports vaccination rates high enough to provide herd immunity.", 130),
                  caption = "Visualization: Uwe Hadler | #TidyTuesday 2020 Week 9 | Source: The Wallstreet Journal",
                  theme = theme(text = element_text(family="Calibri"),
                                plot.title = element_text(face="bold", size=14),
                                plot.subtitle = element_text(size=12),))

ggsave(path = "output", filename="2020-02-27__measles.png", dpi = 400, width = 12, height = 10, type="cairo")

