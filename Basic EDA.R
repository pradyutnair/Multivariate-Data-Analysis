library(pacman)
pacman::p_load(dplyr,MASS,ggplot2,openxlsx, tidyverse, plotly,ggpubr)

df <- data.frame(read.csv('./Chicken Fillet NIR data.csv',
                          strip.white = TRUE,
                          sep=';',
                          stringsAsFactors = TRUE))

head(df)
summary(df)

# EDA
colnames(df)
sapply(df, class)
length(df)

unique(df$Freshness)
unique(df$Scan_type)
unique(df$Production_system)

sum(is.na(df))

# Plot distributions
fr <- ggplot(data = df) +
  geom_bar(mapping = aes(x = Freshness),fill = "orange",width=0.2) +
  ggtitle('Distribution of Freshness') +
  theme(plot.title = element_text(face = "bold"))  +
  scale_x_discrete(labels=c("FR" = "Fresh", "TH" = "Thawed"))

sc <- ggplot(data = df) +
  geom_bar(mapping = aes(x = reorder(Scan_type,Scan_type,
                                     function(x)-length(x))),fill = "blue",width=0.2) +
  ggtitle('Distribution of Scan Type') +
  xlab("Scan_type") +
  theme(plot.title = element_text(face = "bold"))

ps <- ggplot(data = df) +
  geom_bar(mapping = aes(x = reorder(Production_system,Production_system,
                                     function(x)-length(x))),fill = "darkgreen",width=0.3) +
  ggtitle('Distribution of Production System') +
  xlab("Production_system") +
  theme(plot.title = element_text(face = "bold"))

ggarrange(fr, sc, ps,
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)