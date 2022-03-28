#' # Written by:    Jessica Tryner
#' # Written on: October 21, 2020
#' # Modified by: Casey Quinn
#' # Last Modified: March 04, 2021
#'
#' #PLOTTING
#'
#' # Written by: Gabe Neymark
#' # Written on: December 14, 2021
#' # Modified by: Gabe Neymark
#' # Last Modified: December 22, 2021
#'
#' # Purpose: This function sets the x scale plot variables
#'
#' # Inputs:
#' # (1) a vector
#'
#' # Output: The minimum, maximum, and x step values for plotting
#'
#' #'Read the header data from an Access Sensor Technologies (AST) air sampler
#' #'log file
#' #'
#' #' @param vector Pass a vector for setting x scale
#' #'
#' #' @return An x scale value
#' #' @export
#' #'
#' #' @examples
#' #' x_scale <- set_x_axis(vector)
#'
#' set_x_axis <- function(vector){
#'
#'   vector = vector[!is.na(vector)]
#'
#'   x_min  <- round(min(vector), 1)
#'   x_max  <- round(max(vector), 1)
#'   x_step <- round((x_max - x_min) / 10, 1)
#'
#'   x_scale <- c(x_min, x_max, x_step)
#'
#'   return(x_scale)
#' }
#'
#' # Written by: Jessica Tryner
#' # Written on: October 21, 2020
#' # Modified by: Gabe Neymark
#' # Last Modified: December 22, 2021
#'
#' # Purpose: This function sets the y scale plot variables
#'
#' # Inputs:
#' # (1) a vector
#'
#' # Output: The minimum, maximum, and y step values for plotting
#'
#' set_y_axis <- function(vector){
#'
#'   vector = vector[!is.na(vector)]
#'
#'   y_min  <- min(vector)
#'   y_max  <- max(vector)
#'   y_step <- (y_max - y_min) / 10
#'
#'   if((y_max - y_min) < 0.01){
#'     y_min  <- round(y_min,  digits=4)
#'     y_max  <- round(y_max,  digits=4)
#'     y_step <- round(y_step, digits=4)
#'   }else if((y_max - y_min) <= 0.1){
#'     y_min  <- round(y_min,  digits=3)
#'     y_max  <- round(y_max,  digits=3)
#'     y_step <- round(y_step, digits=3)
#'   } else {
#'     y_min  <- round(y_min,  digits=2)
#'     y_max  <- round(y_max,  digits=2)
#'     y_step <- round(y_step, digits=2)
#'   }
#'
#'   y_scale <- c(y_min, y_max, y_step)
#'
#'   return(y_scale)
#' }
#'
#' # Written by: Gabe Neymark
#' # Written on: December 14, 2021
#' # Modified by: Gabe Neymark
#' # Last Modified: January 4, 2022
#'
#' # Purpose: Plots the variable of interest over the sample time
#'
#' # Inputs:
#' # (1) data frame
#' # (2) y value
#' # (3) x axis text
#' # (4) x scale array (contains x minimum, maximum, and step)
#' # (5) y scale array (contains y minimum, maximum, and step)
#'
#' # Output: A plot of the y value vs. Sample Time
#'
#' timePlot <- function(df, yval, x_text, x_scale, y_text, y_scale) {
#'
#'   x_min <- x_scale[1]
#'   x_max <- x_scale[2]
#'   x_step <- x_scale[3]
#'   y_min <- y_scale[1]
#'   y_max <- y_scale[2]
#'   y_step <- y_scale[3]
#'
#'   p <- ggplot(df, aes(x=SampleTime, y=.data[[yval]], color=as.factor(UPASserial), shape=as.factor(SampleName))) +
#'     # geom_hline(yintercept=0.96*df_head$VolumetricFlowRate, color=ast_red, linetype=5, size=1) +
#'     # geom_hline(yintercept=1.04*df_head$VolumetricFlowRate, color=ast_red, linetype=5, size=1) +
#'     geom_point(size=0.5) +
#'     coord_cartesian(xlim=c(x_min, x_max), ylim=c(y_min, y_max)) +
#'     xlab(x_text) +
#'     ylab(y_text) +
#'     # ylab(expression(Flow ~ rate ~ (L ~ min^-1))) +
#'     scale_x_continuous(n.breaks=10) +
#'     scale_y_continuous(n.breaks=10, labels=scales::comma) +
#'     #ggtitle('Log Variable: VolFlow') +
#'     labs(color="Serial", shape="") +
#'     theme_plot
#'
#'   p
#' }
#'
#'
#' # Written by: Gabe Neymark
#' # Written on: December 14, 2021
#' # Modified by: Casey Quinn
#' # Last Modified: January 14, 2022
#'
#' # Purpose: Plots the variable of interest over the sample time
#'
#' # Inputs:
#' # (1) data frame
#' # (2) y value
#' # (3) x axis text
#' # (4) x scale array (contains x minimum, maximum, and step)
#' # (5) y scale array (contains y minimum, maximum, and step)
#'
#' # Output: A plot of the y value vs. Sample Time
#' timePlot_logy <- function(df, yval, x_text, x_scale, y_text, y_scale) {
#'
#'   x_min <- x_scale[1]
#'   x_max <- x_scale[2]
#'   x_step <- x_scale[3]
#'   y_min <- y_scale[1]
#'   y_max <- y_scale[2]
#'   y_step <- y_scale[3]
#'
#'   p <- ggplot(df, aes(x=SampleTime, y=.data[[yval]], color=as.factor(UPASserial), shape=as.factor(SampleName))) +
#'     # geom_hline(yintercept=0.96*df_head$VolumetricFlowRate, color=ast_red, linetype=5, size=1) +
#'     # geom_hline(yintercept=1.04*df_head$VolumetricFlowRate, color=ast_red, linetype=5, size=1) +
#'     geom_point(size=0.5) +
#'     # xlim(x_min, x_max) +
#'     # coord_cartesian(xlim=c(x_min, x_max), ylim=c(y_min, y_max)) +
#'     xlab(x_text) +
#'     ylab(y_text) +
#'     # ylab(expression(Flow ~ rate ~ (L ~ min^-1))) +
#'     scale_x_continuous(n.breaks=10) +
#'     scale_y_log10() +
#'     #scale_y_continuous(n.breaks=10, labels=scales::comma) +
#'     #ggtitle('Log Variable: VolFlow') +
#'     labs(color="Serial", shape="") +
#'     theme_plot
#'
#'   p
#' }
#'
#' # Define custom colors and color scales for plotting
#' ast_red    <- "#EF2D35"
#' ast_blue   <- "#91B3E0"
#' ast_navy   <- "#081B33"
#'
#' # Plot formatting
#' theme_ast <- theme(plot.background  = element_blank(),
#'                     panel.background = element_blank(),
#'                     panel.border = element_rect(fill=NA),
#'                     panel.grid.major = element_line(color="grey90"),
#'                     panel.grid.minor = element_line(color="grey90"),
#'                     axis.text  = element_text(size=9, color='black'),
#'                     axis.title = element_text(size=9, color='black'),
#'                     axis.ticks = element_line(color='black'),
#'                     axis.text.x = element_text(angle=30, vjust=0.7),
#'                     strip.text = element_text(size=9, color='black', face="bold"),
#'                     strip.background = element_blank(),
#'                     plot.title = element_text(size=11, color="black", hjust=0.5),
#'                     legend.title = element_text(size=10),
#'                     # legend.position = c(0.8,0.22),
#'                     legend.box.background = element_rect(color="black"),
#'                     legend.key.height = unit(0.3, "cm"),
#'                     legend.key = element_blank(),
#'                     legend.background = element_blank())
