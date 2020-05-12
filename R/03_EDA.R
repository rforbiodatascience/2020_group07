# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")
library("gridExtra")
library("grid")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------

# PAM50genes filtered version
joined_data_aug <- read_csv(file = "data/02_joined_data_PAM50_aug.csv")

# Full version
joined_data_full_aug <-  read_csv(file = "data/02_joined_data_full_aug.csv")



# Wrangle data ------------------------------------------------------------

# Get the Control sample range (for the boxplot)
control_range <- joined_data_full_aug %>%
  # Select control observations (rows)
  filter(Class == "Control") %>%
  # Select ID and gene variables (columns)
  select(patient_ID, starts_with("NP_")) %>%
  # Make tibble longer
  pivot_longer(cols = -patient_ID,
               names_to = "genes",
               values_to = "value") %>% 
  # We want the statistics grouped by patient
  group_by(patient_ID) %>% 
  # Find IQR for each patient
  summarise(lower = quantile(value, 0.25), 
            upper = quantile(value, 0.75)) %>% 
  # Find minimum and maximum value of IQR
  summarise(min = min(lower), 
            max = max(upper)) %>%
  unlist()

# Wrangle joined_data_aug specific for this script
joined_data_aug <- joined_data_aug %>%
  # Remove controls, since there are too few samples in this group
  filter(Class != "Control")  %>% 
  # Change Gender values to lowercase
  mutate(Gender = str_to_lower(Gender))




# Boxplot: protein expression in different cancer types -------------------
## 4 boxplots combined into one canvas

## Create individual boxplots
p1_boxplot <- plotting_boxplot(data = joined_data_full_aug, 
                               subset_term = "Basal", 
                               color = "red",
                               control_range = control_range)
p2_boxplot <- plotting_boxplot(data = joined_data_full_aug, 
                               subset_term = "HER2", 
                               color = "green",
                               control_range = control_range)
p3_boxplot <- plotting_boxplot(data = joined_data_full_aug, 
                               subset_term = "LumA", 
                               color = "turquoise3",
                               control_range = control_range)
p4_boxplot <- plotting_boxplot(data = joined_data_full_aug, 
                               subset_term = "LumB", 
                               color = "purple",
                               control_range = control_range)


# Call the helper function for legend extraction
shared_legend <- get_legend(p4_boxplot)

# Remove the legend from the remaining plot
p1_boxplot <- p1_boxplot + theme(legend.position = "none")
p2_boxplot <- p2_boxplot + theme(legend.position = "none")
p3_boxplot <- p3_boxplot + theme(legend.position = "none")
p4_boxplot <- p4_boxplot + theme(legend.position = "none")


# Combine the 4 plots and the shared legend
superior_title <- "Protein expression distribution across classes"

plot_EDA2_boxplot_combo <- grid.arrange(p1_boxplot,
                                        p2_boxplot, 
                                        p3_boxplot, 
                                        p4_boxplot,
                                        shared_legend,
                                        top = textGrob(superior_title,
                                                       gp = gpar(fontsize = 25)),
                                        ncol= 2,
                                        widths = c(2.7, 2.7),
                                        nrow = 3,
                                        heights = c(2.7, 2.7, 0.5),
                                        # To bind the legend as a shared 3rd row c(el.5, el.5)
                                        layout_matrix = rbind(c(1,2), 
                                                              c(3,4), 
                                                              c(5,5))) 

ggsave(plot = plot_EDA2_boxplot_combo, filename = "results/03_EDA_boxplot_combined.png",
       device = "png",
       scale = 1.8,
       dpi = 300)



# Barplot: Class distribution-----------------------------------------------
joined_data_aug %>% 
  ggplot(mapping = aes(x = Class, 
                       fill = Class)) +
  geom_bar(alpha = 0.8) +
  theme_bw(base_family = "Times", 
           base_size = 15) +
  labs(y = "Number of patients",
       title = "Breast cancer class distribution") +
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 18),
        legend.position = "none")

ggsave(filename = "results/03_EDA_class_distribution.png",
       device = "png",
       scale = 1,
       dpi = 300)


# Histogram: Age distribution ---------------------------------------------
joined_data_aug %>% 
  ggplot(mapping = aes(x = Age_at_Initial_Pathologic_Diagnosis, 
                       fill = Class)) +
  geom_histogram(binwidth = 10,
                 alpha = 0.8) +
  scale_x_continuous(breaks = seq(20, 100, 10)) + 
  labs(title = "Age distribution",
       subtitle = "across breast cancer classes",
       x = 'Age',
       y = 'Number of patients') +
  theme_bw(base_family = "Times", 
           base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5, 
                                size = 18),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 12)) 

ggsave(filename = "results/03_EDA_age_distribution.png", 
       device = "png",
       scale = 1,
       width = 6,
       dpi = 300)



# Barplot: Gender distribution -----------------------------------------------
joined_data_aug %>% 
  ggplot(mapping = aes(Gender)) +
  geom_bar(fill = "darkblue",
           alpha = 0.75) +
  theme_bw(base_family = "Times", 
           base_size = 15) +
  labs(y = "Number of patients",
       title = "Gender distribution") +
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 18))

ggsave(filename = "results/03_EDA_gender_vs_tumortype.png", 
       device = "png",
       scale = 1,
       dpi = 300)   



