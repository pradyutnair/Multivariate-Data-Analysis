#Goal of this script is to create balanced samples of the variable so further
#data analysis can be done
dirName = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirName)

set.seed(123)

pacman::p_load(dplyr,MASS,ggplot2,openxlsx, plotly,ggpubr)

df <- data.frame(read.csv('./Chicken Fillet NIR data.csv',
                          strip.white = TRUE,
                          sep=';',
                          stringsAsFactors = TRUE))

#separating items for freshness
df_fresh = df[df$Freshness == 'FR',]
df_thawed = df[df$Freshness == 'TH',]


#getting the number of values in each cathegory
thawed_values= as.numeric(table(df$Freshness))
thawed_values = thawed_values[2]


#undersampling the fresh dataframe
df_undersample_fresh = sample_n(df_fresh, thawed_values)

#binding the dataframes
df_undersample_fresh = rbind(df_undersample_fresh, df_thawed)




#separating items for Scan types
df_OM = df[df$Scan_type == 'OM',]
df_TP = df[df$Scan_type == 'TP',]
df_TB = df[df$Scan_type == 'TB',]


#getting the number of values in each cathegory
TB_values= as.numeric(table(df$Scan_type))
print(TB_values)
TB_values = TB_values[2]



#undersampling the fresh dataframe
df_undersample_OM = sample_n(df_OM,TB_values )
df_undersample_TP = sample_n(df_TP,TB_values )

#binding the dataframes
df_undersampled_Scan_type = rbind(df_undersample_OM, df_undersample_TP)
df_undersampled_Scan_type = rbind(df_undersampled_Scan_type, df_TB)

# Plot distributions
fr <- ggplot(data = df_undersample_fresh) +
  geom_bar(mapping = aes(x = Freshness),fill = "orange",width=0.2) +
  ggtitle('Distribution of Freshness Undersapled') +
  theme(plot.title = element_text(face = "bold"))  +
  scale_x_discrete(labels=c("FR" = "Fresh", "TH" = "Thawed"))

sc <- ggplot(data = df_undersampled_Scan_type) +
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