# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(broom)
library(patchwork)


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
joined_data_aug <- read_csv(file = "data/02_joined_data_PAM50_aug.csv")


# Wrangle data ------------------------------------------------------------

## Remove healthy control samples
joined_data_aug <- joined_data_aug %>% 
  filter(Class != "Control")

## Select only proteome-count columns
proteome_data <- joined_data_aug %>%
  select(starts_with("NP"))            


# PCA ---------------------------------------------------------------------

## Compute PCA
pca <- proteome_data %>% 
  prcomp(center = TRUE, scale = TRUE) 

## Scree plot
pca %>%
  tidy("pcs") %>% 
  ggplot(mapping = aes(x = PC, 
                       y = percent)) +
  geom_col() +
  labs(title = "Scree plot: PCA proteome data", 
       x = "Principal components",
       y = "Variance explained (%)") +
  scale_y_continuous(labels = scales::percent) + 
  myplot_aes

ggsave(filename = "results/05_PCA_scree_plot.png", 
       device = "png",
       scale = 1)


## Augment and add y class
proteome_pca_aug <- pca %>%
  augment(proteome_data) %>%
  mutate(Class = factor(joined_data_aug$Class, levels = c("Basal", "HER2", 
                                                          "LumA", "LumB")))


## Get PC percents
PC1_perc <- pca %>% 
  tidy("pcs") %>% 
  filter(PC==1) %>% 
  pull(percent) 

PC2_perc <- pca %>% 
  tidy("pcs") %>% 
  filter(PC==2) %>% 
  pull(percent) 


## Scatter proteome data - PC1/PC2
proteome_pca_aug %>% 
  ggplot(mapping = aes(x = .fittedPC1,
                       y = .fittedPC2,
                       colour = Class)) +
  geom_point(size = 3) +
  xlim(-8,10) +
  ylim(-7,7) +
  labs(title = "PCA plot of proteome data", 
       x = str_c("PC1 (", round(PC1_perc * 100, 2), "%)"),
       y = str_c("PC2 (", round(PC2_perc * 100, 2), "%)")) +
  myplot_aes 

ggsave(filename = "results/05_PCA.png", 
       device = "png",
       height = 5,
       dpi = 300)

# K-means clustering ------------------------------------------------------

## Select number of clusters (as many as categories in Class variable)
k <- proteome_pca_aug %>% 
  select (Class) %>% 
  unique() %>%
  pull() %>% 
  length()

## Clustering on original data
set.seed(12)
cluster_original <- proteome_data %>%
  kmeans(centers = k)

## Augment to PCA data
proteome_pca_cluster_aug <- cluster_original %>%
  broom::augment (proteome_pca_aug) %>% 
  rename(cluster_original = .cluster)


## Clustering on dimensionality-reduced data (2 first PCs)
cluster_pca <- proteome_pca_aug %>%
  select(.fittedPC1, .fittedPC2) %>%
  kmeans(centers = k)

## Augment to PCA/kmeans data
proteome_pca_cluster_aug <- 
  cluster_pca %>% 
  broom::augment (proteome_pca_cluster_aug) %>% 
  rename(cluster_pca = .cluster)


## Which clustering technique performs better
accuracy <- proteome_pca_cluster_aug %>%
  select(Class, cluster_original, cluster_pca) %>%
  # Class cross-referencing & annotation
  mutate(cluster_original = case_when(cluster_original == 1 ~ "HER2",
                                      cluster_original == 2 ~ "LumA",
                                      cluster_original == 3 ~ "LumB",
                                      cluster_original == 4 ~ "Basal"),
         cluster_pca = case_when(cluster_pca == 1 ~ "LumA",
                                 cluster_pca == 2 ~ "Basal",
                                 cluster_pca == 3 ~ "HER2",
                                 cluster_pca == 4 ~ "LumB"),
         cluster_original_correct = case_when(Class == cluster_original ~ 1,
                                              Class != cluster_original ~ 0),
         cluster_pca_correct = case_when(Class == cluster_pca ~ 1,
                                         Class != cluster_pca ~ 0)) %>% 
  summarise(score_original = mean(cluster_original_correct) * 100,
            score_pca = mean(cluster_pca_correct) * 100)



# Visualization of clusters on PCs ----------------------------------------

## Original classes
plot1 <- proteome_pca_cluster_aug %>%
  ggplot(mapping = aes(x = .fittedPC1,
                       y = .fittedPC2,
                       colour = Class)) +
  geom_point(size = 2) +
  labs(title = "Original data",
       x = 'PC1',
       y = 'PC2',
       colour = "True class") +
  xlim(-8,10) +
  ylim(-7,7) +
  myPCA_aes +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title.align = 0.5,
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.5, "cm")) +
  guides(colour = guide_legend(title.position = "top",
                                nrow = 2,
                                byrow = TRUE))


## Clusters on original data
plot2 <- proteome_pca_cluster_aug %>%
  ggplot(mapping = aes(x = .fittedPC1,
                       y = .fittedPC2,
                       colour = cluster_original)) +
  geom_point(size = 2) +
  labs(title = "Clusters on original data",
       subtitle = paste0("accuracy = ", round(accuracy[[1]], 1), "%"),
       x = 'PC1',
       y = 'PC2',
       colour = "Clusters") +
  xlim(-8,10) +
  ylim(-7,7) +
  myPCA_aes +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), 
        legend.title.align = 0.5,
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.5,"cm")) +
  guides(colour = guide_legend (title.position = "top")) 



## Clusters on dimensionality-reduced data (first 2 PCs)
plot3 <- proteome_pca_cluster_aug %>%
  ggplot(mapping = aes(x = .fittedPC1,
                       y = .fittedPC2,
                       colour = cluster_pca)) +
  geom_point(size = 2) +
  labs(title = "Clusters on PCA data",
       subtitle = paste0("accuracy = ", round(accuracy[[2]], 1), "%"),
       x = 'PC1',
       y = 'PC2',
       colour = "Clusters") + 
  xlim(-8,10) +
  ylim(-7,7) +
  myPCA_aes +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), 
        legend.title.align = 0.5,
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.5,"cm")) +
  guides(colour = guide_legend(title.position = "top")) 




(plot1 + plot2 + plot3) 

ggsave(filename = "results/05_PCA_kmeans.png", device = "png",
       height = 5,
       dpi = 300)


