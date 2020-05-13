# 
# joined_data_aug %>%   
#   select(patient_ID,
#          starts_with("NP_"),
#          Class) %>%
#   subset(Class == "Basal") %>%
#   pivot_longer(cols = starts_with('NP_')) %>%
#   ggplot() + 
#   
#   geom_rect(aes(xmin = control_range[1],
#                         xmax = control_range[2], # supply from data
#                         ymin = -Inf,
#                         ymax = Inf, color = "Control"), fill = "yellow2", alpha = 0.3) +
#   geom_boxplot(mapping = aes(y = reorder(patient_ID, value, FUN = median),
#                              x = value),fill="red",alpha = 0.7, varwidth = TRUE, outlier.shape = NA) +
#   scale_color_manual('IQR',
#                     values = 'yellow',  
#                     guide = guide_legend(override.aes = list(alpha = 1))) +
# 
#   labs(x = "Log2 Expression levels",
#        y = "Patients",
#        title = subset_term) +
#   xlim(-10, 10) +
#   geom_vline(aes(xintercept = control_range[2],
#                  color = "Control samples"),
#              linetype = "solid",
#              size = 1) +
#   geom_vline(aes(xintercept = control_range[1]),
#              linetype = "solid",
#              size=1) +
#   scale_color_manual(name = "Inter Quartile Range:",
#                      values = c('Control samples' = "black",
#                                 'Lower Bound' = "grey12")) +
#   stat_summary(fun = "median",
#                geom = "point",
#                colour = "white",
#                shape = 23,
#                size = 1) +
#   theme_bw(base_family = "Times",
#            base_size = 16) +
# myplot_aes +
#   theme(legend.position = "bottom",
#         legend.title = element_text(size = 15),
#         axis.text.x = element_text(size = 18),
#         axis.text.y = element_blank(),
#         plot.title = element_text(hjust = 0.5,
#                                   size = 22)) 

plotting_boxplot2 <- function(data, subset_term, color, 
                             control_range = control_range, 
                             control_range_colour = "yellow1") {
  plot <- data %>%   
    select(patient_ID,
           starts_with("NP_"),
           Class) %>%
    subset(Class == subset_term) %>%
    pivot_longer(cols = starts_with('NP_')) %>%
    ggplot() + 
    geom_rect(aes(xmin = control_range[1],
                  xmax = control_range[2], # supply from data
                  ymin = -Inf, ymax = Inf, 
                  color = "Controls"),
              fill = "yellow2",
              alpha = 0.3) +
    geom_boxplot(mapping = aes(y = reorder(patient_ID, value, FUN = median),
                               x = value),fill=color,alpha = 0.7, varwidth = TRUE, outlier.shape = NA) +
    scale_color_manual('Inter Quartile Range',
                       values = 'yellow',  
                       guide = guide_legend(override.aes = list(alpha = 0.1))) +
    geom_vline(aes(xintercept = control_range[2]),
               linetype = "solid",
               size = 1) +
    geom_vline(aes(xintercept = control_range[1]), 
               linetype = "solid",
               size=1) +
    # stat_summary(fun = "median", 
    #              geom = "point", 
    #              colour = "white", 
    #              shape = 23, 
    #              size = 1) +
    labs(x = "Log2 Expression levels",
         y = "Patients",
         title = subset_term) +
    xlim(-10, 10) +
    theme_bw(base_family = "Times", 
             base_size = 16) +
    theme(legend.position = "bottom",
          legend.title = element_text(size = 15),
          axis.text.x = element_text(size = 18),
          axis.text.y = element_blank(),
          plot.title = element_text(hjust = 0.5,
                                    size = 22))
  
  return(plot)
}

## Create individual boxplots
p1_boxplot <- plotting_boxplot(data = joined_data_aug, 
                               subset_term = "Basal", 
                               color = "red",
                               control_range = control_range)
p2_boxplot <- plotting_boxplot(data = joined_data_aug, 
                               subset_term = "HER2", 
                               color = "green",
                               control_range = control_range)
p3_boxplot <- plotting_boxplot(data = joined_data_aug, 
                               subset_term = "LumA", 
                               color = "turquoise3",
                               control_range = control_range)
p4_boxplot <- plotting_boxplot(data = joined_data_aug, 
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

