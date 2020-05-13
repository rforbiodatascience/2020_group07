# DEFINE PROJECT FUNCTIONS


# Tibble to matrix --------------------------------------------------------

as_matrix <- function(x){
  if(!tibble::is_tibble(x) ) stop("x must be a tibble")
  y <- as.matrix.data.frame(x[,-1])
  rownames(y) <- x[[1]]
  y
}
## Credit: 
## https://rdrr.io/github/HuntsmanCancerInstitute/hciR/man/as_matrix.html


# Extract plot's legend -----------------------------------------------

get_legend <- function(myggplot){
  #' @param myggplot a ggplot object
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


# Customized boxplot ------------------------------------------------------

plotting_boxplot <- function(data, subset_term, color, 
                             control_range = control_range, 
                             control_range_colour = "yellow1") {
  plot <- data %>%   
    select(patient_ID,
           starts_with("NP_"),
           Class) %>%
    subset(Class == subset_term) %>%
    pivot_longer(cols = starts_with('NP_')) %>%
    ggplot(mapping = aes(y = reorder(patient_ID, value, FUN = median),
                         x = value)) + 
    annotate("rect", 
             xmin = control_range[1], 
             xmax = control_range[2], # supply from data
             ymin = -Inf,
             ymax = Inf,
             alpha = 0.3, 
             fill = control_range_colour) +
    geom_boxplot(alpha = 0.7,
                 varwidth = TRUE,
                 outlier.shape = NA,
                 fill = color) + 
    labs(x = "Log2 Expression levels",
         y = "Patients",
         title = subset_term) +
    xlim(-10, 10) +
    geom_vline(aes(xintercept = control_range[2],
                   color = "Control samples"), 
               linetype = "solid",
               size = 1) +
    geom_vline(aes(xintercept = control_range[1]), 
               linetype = "solid",
               size=1) +
    scale_color_manual(name = "Inter Quartile Range:", 
                       values = c('Control samples' = "black", 
                                  'Lower Bound' = "grey12")) +
    stat_summary(fun = "median", 
                 geom = "point", 
                 colour = "white", 
                 shape = 23, 
                 size = 1) +
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

# TESTING
plotting_boxplot <- function(data, subset_term, color, 
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

# Customized plot's theme -------------------------------------------------

## to be consistent across all plots
myplot_aes <- theme_bw(base_family = "Times", 
                         base_size = 18) +
              theme(plot.title = element_text(hjust = 0.5, 
                                              size = 25),
                    legend.position = "bottom")

