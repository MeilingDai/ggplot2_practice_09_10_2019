install.packages("tidyverse")
.libPaths(c("C:/Users/DAI035/data_school/Packages"))

library(tidyverse)
sessionInfo()
#read csv data
read_csv("data/BOM_data.csv")
BOM_data <- read_csv("data/BOM_data.csv")
BOM_data


# Question 1
Perth_BOM_data <- BOM_data %>% 
  filter(Station_number == 9225)
Perth_BOM_data



Temp_Perth_BOM_data <- Perth_BOM_data %>%  
  separate(col = Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/" ) %>% #separate column into temp-max and temp_min
 mutate (Temp_min = as.numeric(Temp_min), Temp_max = as.numeric(Temp_max), Rainfall = as.numeric(Rainfall), Solar_exposure = as.numeric(Solar_exposure) )
Temp_Perth_BOM_data

# figure 1: 
# figure 1-1: the relationship between the maximum temperature and minimum temperature

Figure1_plot1 <- ggplot(Temp_Perth_BOM_data,
       aes(x = Temp_max, y = Temp_min, color = Year))+
  geom_point()+
  scale_color_continuous()+
  labs(title = " Figure 1-1 : the relationship between the maxi and mini temperature
")+
  theme(plot.title = element_text(color = "red", size = 8 ))
ggsave(filename = "results/Figure 1-1.png",  plot = Figure1_plot1, width = 10, height = 8, dpi = 300, units = "cm")

# figure 1-2: the relationship between the maximum temperature and rainfall
Figure1_plot2 <- ggplot(Temp_Perth_BOM_data,
                        aes(x = Temp_max, y = Rainfall, color = Year, alpha = 0.5))+
  geom_point()+
  scale_color_continuous()+
  labs(title = " Figure 1-2 : the relationship between the maxi temperature and Rainfall
")+
  theme(plot.title = element_text(color = "green", size = 8))

ggsave(filename = "results/Figure 1-2.png", plot = Figure1_plot2, width = 10, height = 8, dpi = 300, units = "cm")


# figure 1-3: the relationship between the maximum temperature and solar
Figure1_plot3 <- ggplot(Temp_Perth_BOM_data,
                        aes(x = Temp_max, y = Solar_exposure, color = Year))+
  geom_point()+
  scale_color_continuous()+
  labs(title = " Figure 1-3 : the relationship between the maxi temperature and solar
",
       x = "Temp_max",
       y = "Solar_exposure")+
  theme_classic()+
  theme(plot.title = element_text(color = "Purple", size = 8))

ggsave(filename = "results/Figure 1-3.png", plot = Figure1_plot3, width = 10, height = 8, dpi = 300, units = "cm")


# Question 2
# Display these four measurements for the Perth station in a single scatter plot by using additional aesthetic mappings.
Figure2_plot <- ggplot(Temp_Perth_BOM_data,
                        aes( x = Temp_max, y = Temp_min, color = Solar_exposure, size = Rainfall))+
  geom_point()+
  scale_color_gradient2()+
  labs(title = "All measurements together")

ggsave(filename = "results/Figure 2.png", plot = Figure2_plot, width = 10, height = 8, dpi = 300, units = "cm")


# Question 3
# Take the four plots you have produced in Q1 and Q2 and save them as a multi-panel figure.
install.packages("cowplot")
library(cowplot)
# # Combining 4 graphs into one
Big_plot <- plot_grid(Figure1_plot1, Figure1_plot2, Figure1_plot3, Figure2_plot)
ggsave(filename = "results/Big plot.png", plot = Big_plot, width = 45, height = 36, dpi = 300, units = "cm")




# Question 4: Using the entire BOM dataset, calculate the average monthly rainfall for each station. Produce a lineplot to visualise this data and the state each station is in.
# change variables to numeric

Rainfall_BOM_data <- BOM_data %>%  
    mutate (Rainfall = as.numeric(Rainfall))
Grouped_Rainfall_BOM_data <- Rainfall_BOM_data %>% 
  group_by(Station_number, Month) %>% 
  filter(Rainfall != "NA") %>%  
  summarise(mean_monthly_rainfall = mean(Rainfall))
Grouped_Rainfall_BOM_data

read_csv("data/BOM_stations.csv")
BOM_stations<- read_csv("data/BOM_stations.csv")
BOM_stations
tidy_BOM_stations <- BOM_stations %>% gather(Station_number, measurement, -info) %>% 
  spread(key = info, value = measurement) %>% 
  mutate(Station_number = as.numeric(Station_number))
tidy_BOM_stations


left_BOM <- left_join (Grouped_Rainfall_BOM_data, tidy_BOM_stations)
left_BOM

left_BOM_plot <- 
ggplot(data = left_BOM,
       aes(x = Month,
           y = mean_monthly_rainfall,
           group = Station_number,
           color = state)) + 
   geom_line()+
  labs(title = "Average monthly rainfall for each station")+
  theme_classic()+
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(color = "Orange", size =6))+
  facet_wrap(~ state)

ggsave(filename = "results/Figure 5.png", plot = left_BOM_plot, width = 10, height = 8, dpi = 300, units = "cm")
