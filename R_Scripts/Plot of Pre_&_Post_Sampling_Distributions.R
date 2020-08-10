rm(list=ls())

library(purrr);  library(ggplot2); library(dplyr)
library(plotly); library(tibble); library(scales)

pre_distribution_data = read.csv('Training_Data_Distribution_1.csv',
                          header = T, fileEncoding = "UTF-8-BOM")
attach(pre_distribution_data);
#View(pre_distribution_data);

data_df = data.frame(pre_distribution_data)

data_df$Time = as.POSIXct(hms::parse_hm(data_df$Time))

data_df = data_df[, -1]
Colours = c(rep(0,388558), rep(1,1442))
data_df = cbind(data_df, Colours)
View(data_df)

y_vals = c(-7, -5, -3, -1.8, 0,
           1.8, 3, 5, 7)

Label_Color = c("#4169E1", "#FFA500")
Label_Name =  c("Volatility", "Jump")

dist_plot = ggplot(data_df, aes(x = Time, 
                       y = Return, z = Colours,
                       color = as.factor(Colours))) +
  geom_point() +
  scale_color_manual(name = "Return Category",
                     values = Label_Color,
                     labels = Label_Name) +
  labs(title = "Scatterplot of the Training Data Distribution",
       y = "Two minute Price Return %",
       x = "Time of Price Return") +
  theme_bw() +
  scale_y_continuous(breaks = y_vals) +
  scale_x_datetime(date_labels = "%H:%M",
                   breaks = date_breaks("1 hours")) +
  theme(plot.title = element_text(size = 16,
                                  color = "black",
                                  hjust = 0.5),
        axis.text.x = element_text(angle=30, hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.box.background = element_rect(colour = "black")) +
  geom_hline(yintercept=c(0, 1.65, -1.65),
             linetype=c("solid", "dashed", "dashed"),
             color=c("black", "#3d3d3d", "#3d3d3d"),
             size=c(1.25, 1, 1))
dist_plot

ggsave('Training_Data_Category_Distribution_Graph.pdf')
ggsave('Training_Data_Category_Distribution_Graph.png')

# Post Minority Class Upsampling

rm(list=ls())

library(purrr);  library(ggplot2); library(dplyr)
library(plotly); library(tibble); library(scales)

post_distribution_data = read.csv('Upsampled_Combined_Data.csv',
                                 header = T, fileEncoding = "UTF-8-BOM")
attach(post_distribution_data);
#View(post_distribution_data);

upsampled_data_df = data.frame(post_distribution_data)

upsampled_data_df$Time = as.POSIXct(hms::parse_hm(upsampled_data_df$Time))

upsampled_data_df = upsampled_data_df[, -1]
Colours = c(rep(0,388558), rep(1,291778))
upsampled_data_df = cbind(upsampled_data_df, Colours)
#View(upsampled_data_df)

upsampled_y_vals = c(-7.5, -5, -3, -1.8, 0,
           1.8, 3, 5, 7.5)

upsampled_Label_Color = c("#4169E1", "#FFA500")
upsampled_Label_Name =  c("Volatility", "Jump")

upsampled_dist_plot = ggplot(upsampled_data_df, aes(x = Time, 
                                y = Return, z = Colours,
                                color = as.factor(Colours))) +
  geom_point() +
  scale_color_manual(name = "Return Category",
                     values = upsampled_Label_Color,
                     labels = upsampled_Label_Name) +
  labs(title = "Scatterplot of the Upsampled Training Data Distribution",
       y = "Two minute Price Return %",
       x = "Time of Price Return") +
  theme_bw() +
  scale_y_continuous(breaks = upsampled_y_vals) +
  scale_x_datetime(date_labels = "%H:%M",
                   breaks = date_breaks("1 hours")) +
  theme(plot.title = element_text(size = 16,
                                  color = "black",
                                  hjust = 0.5),
        axis.text.x = element_text(angle=30, hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.box.background = element_rect(colour = "black")) +
  geom_hline(yintercept=c(0, 1.65, -1.65),
             linetype=c("solid", "dashed", "dashed"),
             color=c("black", "#3d3d3d", "#3d3d3d"),
             size=c(1.25, 1, 1))
upsampled_dist_plot

ggsave('upsampled_Training_Data_Category_Distribution_Graph.pdf')
ggsave('upsampled_Training_Data_Category_Distribution_Graph.png')

# Post SMOTE Class Upsampling

rm(list=ls())

library(purrr);  library(ggplot2); library(dplyr)
library(plotly); library(tibble); library(scales)

SMOTE_distribution_data = read.csv('SMOTE_Training_Data.csv',
                                  header = T, fileEncoding = "UTF-8-BOM")
attach(SMOTE_distribution_data);
View(SMOTE_distribution_data);

SMOTE_data_df = data.frame(SMOTE_distribution_data)

SMOTE_data_df$Time = as.POSIXct(hms::parse_hm(SMOTE_data_df$Time))

SMOTE_data_df = SMOTE_data_df[, -1]
Colours = c(rep(0,77711), rep(1,38854))
SMOTE_data_df = cbind(SMOTE_data_df, Colours)
View(SMOTE_data_df)

SMOTE_y_vals = c(-7.5, -5, -3, -1.8, 0,
                     1.8, 3, 5, 7.5)

SMOTE_Label_Color = c("#4169E1", "#FFA500")
SMOTE_Label_Name =  c("Volatility", "Jump")

SMOTE_dist_plot = ggplot(SMOTE_data_df, aes(x = Time, 
                                            y = Return, z = Colours,
                                            color = as.factor(Colours))) +
  geom_point() +
  scale_color_manual(name = "Return Category",
                     values = SMOTE_Label_Color,
                     labels = SMOTE_Label_Name) +
  labs(title = "Scatterplot of the SMOTE Training Data Distribution",
       y = "Two minute Price Return %",
       x = "Time of Price Return") +
  theme_bw() +
  scale_y_continuous(breaks = SMOTE_y_vals) +
  scale_x_datetime(date_labels = "%H:%M",
                   breaks = date_breaks("1 hours")) +
  theme(plot.title = element_text(size = 16,
                                  color = "black",
                                  hjust = 0.5),
        axis.text.x = element_text(angle=30, hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.box.background = element_rect(colour = "black")) +
  geom_hline(yintercept=c(0, 1.65, -1.65),
             linetype=c("solid", "dashed", "dashed"),
             color=c("black", "#3d3d3d", "#3d3d3d"),
             size=c(1.25, 1, 1))
SMOTE_dist_plot

ggsave('SMOTE_Training_Data_Category_Distribution_Graph.pdf')
ggsave('SMOTE_Training_Data_Category_Distribution_Graph.png')


