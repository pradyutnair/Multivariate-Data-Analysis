library(pacman)
pacman::p_load(dplyr,MASS,ggplot2,openxlsx, tidyverse, plotly,ggpubr,reshape2,Hmisc,
               cowplot, PerformanceAnalytics)

# Import the data and create dataframe
setwd('./')
df <- data.frame(read.csv('./Chicken Fillet NIR data.csv',
                          strip.white = TRUE,
                          sep=';',
                          stringsAsFactors = TRUE))

head(df)
# Summary statistics
summary(df)

# Obtain column names, data type, and number of columns
colnames(df)
sapply(df, class)
length(df)

# Find unique values of each categorical variable
unique(df$Freshness)
unique(df$Scan_type)
unique(df$Production_system)

# Missing values per column
colSums(is.na(df))
sum(is.na(df))

# Plot categorical distributions
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

# Continuous variables distribution
melt.df <- melt(df)
ggplot(data = melt.df, aes(x = value)) +
  stat_density() +
  facet_wrap(~variable, scales = "free")

# Visualise the distribution and correlation

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
} # function that flattens the correlation matrix of the dataframe

res2<-rcorr(as.matrix(df[,5:129]))                # compute the corr matrix and change column names
corr.df <- data.frame(flattenCorrMatrix(round(res2$r,5), round(res2$P,5)))
colnames(corr.df) <- c("Feature 1","Feature 2", "Corr", "p")

corr.df <- corr.df[order(corr.df$Corr),]          # sort correlation in ascending order
head(corr.df)

corplot <- function(x,y,title){
pmain <- ggplot(df, aes(x = x, y = y, color = Freshness))+
  geom_point()+
  ggpubr::color_palette("jco") +
  ggtitle(title) + xlab(deparse(substitute(x))) + ylab(deparse(substitute(y))) +
  theme(plot.title = element_text(size=20,face = "bold"))
# Marginal densities along x axis
xdens <- axis_canvas(pmain, axis = "x")+
  geom_density(data = df, aes(x = x, fill = Freshness),
               alpha = 0.7, size = 0.2)+
  ggpubr::fill_palette("jco")
# Marginal densities along y axis
# Need to set coord_flip = TRUE, if you plan to use coord_flip()
ydens <- axis_canvas(pmain, axis = "y", coord_flip = TRUE)+
  geom_density(data = df, aes(x = y, fill = Freshness),
               alpha = 0.7, size = 0.2)+
  coord_flip()+
  ggpubr::fill_palette("jco")
p1 <- insert_xaxis_grob(pmain, xdens, grid::unit(.2, "null"), position = "top")
p2<- insert_yaxis_grob(p1, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(p2)
}               # function that plots two given features and their individual distributions

corplot(x=df$X939.072,y=df$X1465.592,'Correlation plot of least correlated features')
corplot(x=df$X1459.398,y=df$X1465.592,'Correlation plot of most correlated features')

# Defining target variable
library(gplots)
open_simsim