#' Create Github style waffle plots.
#'
#' Based on code from: https://mvuorre.github.io/r/github-waffle-plot/
#'
#' @param data The table containing the data
#' @param date_par The column containing the dates
#' @param data_scale The data column to be plotted
#' @param scale_name The description of the data.
#' @param pal The color palette. Default is "D" (viridis).
#' @param dir The color direction. Default is "-1".
#'
#' @export
#' @return
#' A GitHub style waffle plot of the data.
#'
#' @examples
#' geom_waffle(data, dates, data_scale, scale_name)
geom_waffle <- function(data, date_par, data_scale,
                        scale_name, pal = "D", dir = -1){

  waffle_plot <- ggplot(flights_filtered, aes(x = month(date_par), y = year(date_par),
                                              fill = data_scale)) +
    scale_fill_viridis(name = scale_name,
                       option = pal,  # Variable color palette
                       direction = dir,  # Variable color direction
                       na.value = "grey93",
                       limits = c(0, max(year(date_par)))) +
    geom_tile(color = "white", size = 0.4) +
    # facet_wrap("date_year", ncol = 1) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = seq(length = 12),
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
    scale_y_continuous(
      expand = c(0, 0),
      breaks = seq(min(year(date_par)),
                   max(year(date_par)), by = 1)
    ) +
    theme_tufte(base_family = "Helvetica") +
    theme(axis.title = element_text(),
          axis.ticks = element_blank(),
          legend.position = "bottom",
          # legend.position = c(1, 1),
          # legend.justification = c(1, 0),
          legend.key.width = unit(1, "cm"),
          strip.text = element_text(hjust = 0.01, face = "bold", size = 12)) +
    xlab("month") +
    ylab("year")

  return(waffle_plot)
}
