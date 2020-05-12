# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
joined_data_full_aug <- read_csv(file = "data/02_joined_data_full_aug.csv")


# Wrangle data -----------------------------------------------------------

## Create a subset with only the relevant biomarkers

gene_set <- joined_data_full_aug %>% 
  # Keep only relevant columns for heatmap
  select(patient_ID, 
         Class, 
         NP_000537, 
         NP_006209, 
         NP_001002295, 
         NP_000116, 
         NP_000917, 
         NP_004439) %>% 
  # Change protein RefIDs to correspondent gene names
  rename("TP53" = "NP_000537",
         "PIK3CA" = "NP_006209",
         "GATA3" = "NP_001002295",
         "ESR1" = "NP_000116",
         "PGR" = "NP_000917",
         "ERBB2" = "NP_004439") %>% 
  # Create long data for heatmap
  pivot_longer(cols = c("TP53", "PIK3CA", "GATA3", "ESR1", "PGR", "ERBB2"), 
               names_to = "RefSeq",
               values_to = "ITRAQ_log2_ratio") %>% 
  # Set factors and levels for the plot
  mutate(Class = factor(Class, 
                        levels = c("Basal", "HER2", "LumA", "LumB", "Control"))) %>% 
  mutate(RefSeq = factor(RefSeq, 
                         levels = c("TP53", "PIK3CA", "GATA3", "ESR1", "PGR", "ERBB2")))



# Create heatmap ----------------------------------------------------------

heatmap <- gene_set %>% 
  ggplot(mapping = aes(x = RefSeq, 
                       y = patient_ID, 
                       fill = ITRAQ_log2_ratio)) +
  geom_tile() +
  facet_grid(Class ~ ., 
             scales = "free") +
  scale_fill_gradient2(low = "darkblue",
                       mid = "white",
                       high = "red",
                       midpoint = 0,
                       name = "Expression \n(log2 ratio)") +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 18, 
                                   angle = 50,
                                   hjust = 1),
        axis.text.y = element_blank(),
        strip.text.y = element_text( face = "bold", 
                                    size = 18),
        plot.title = element_text(size = 21,
                                  hjust = 0.5,
                                  face = "bold"),
        legend.title = element_text(size = 18,
                                    hjust = 0.5),
        legend.text = element_text(size = 18),
        panel.spacing.y = unit(0.1, "cm")) +
  labs(title = "Protein expression of common biomarkers in breast cancer",
       x = "Biomarkers",
       y = NULL) +
  
  
ggsave(filename = "results/04_heatmap.png", 
       device = "png",
       scale = 2.0,
       dpi = 300)

