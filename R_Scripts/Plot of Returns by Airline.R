rm(list=ls())

library(purrr);  library(ggplot2); library(dplyr)
library(plotly); library(tibble); library(scales)

airline_data = read.csv('C:\\Users\\Ianm9\\OneDrive\\Desktop\\Workings\\Simulation_CSV_Files\\Visualization_Files\\Airline_Data_Distribution.csv',
                                 header = T, fileEncoding = "UTF-8-BOM")
attach(airline_data);
#View(airline_data);

airline_df = data.frame(airline_data)

airline_df$Time = as.POSIXct(hms::parse_hm(airline_df$Time))

airline_df = airline_df[, -1]
#View(airline_df)
y_vals = c(-7, -5, -3, -1.8, 0,
           1.8, 3, 5, 7)

Airline_Color = c("#228B22", "#93a6ad", "#c8102e",
                   "#F9B612", "#005daa")
Airline_Name =  c("Price_Volatility","American_Airlines","Delta_Airlines",
                  "Southwest_Airlines", "United_Airlines")


airline_plot = ggplot(airline_df, aes(x = Time, 
                                y = Return, z = Label,
                                color = as.factor(Label))) +
  geom_point() +
  scale_color_manual(name = "Return Category",
                     values = Airline_Color,
                     labels = Airline_Name) +
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
             color=c("black", "black", "black"),
             size=c(1.25, 1, 1))
airline_plot

ggsave('C:\\Users\\Ianm9\\OneDrive\\Desktop\\Workings\\Simulation_Images\\Airline_Return_Distribution_Graph.png')
ggsave('C:\\Users\\Ianm9\\OneDrive\\Desktop\\Workings\\Simulation_Images\\Airline_Return_Distribution_Graph.pdf')