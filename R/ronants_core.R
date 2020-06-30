## THEME FUNCTION ====
#' Custom ggplot2 theme
#' @export
theme_ronats <- function(...) {
  # starting point
  theme_minimal() +
    theme(
      text = element_text(family = "serif"),
      # lines on left/bottom axes only
      axis.line.x.bottom = element_line(color = "black", size = .5),
      axis.line.y.left = element_line(color = "black", size = .5),
      # extend ticks marks
      axis.ticks = element_line(color = "black", size = .3),
      # darker gridlines
      panel.grid = element_line(color = "grey90"),
      # smaller, boldface plot titles/subtitles
      plot.title = element_text(size = 10, face = "bold"),
      plot.subtitle = element_text(size = 8, face = "bold"),
      # smaller axis titles
      axis.title = element_text(size = 8),
      # true (RGB) black axis tick labels
      axis.text = element_text(color = "black"),
      # smaller, lighter, left-aligned caption w/ little margin
      plot.caption = element_text(size = 7, face = "italic", color = "grey20",
                                  hjust = 0, margin = margin(t = 3)),
      # move legend to bottom & make horizontal
      legend.position = "bottom",
      # move legend closer to x-axis title, smaller text to match axis titles
      legend.box.spacing = unit(2, units = "pt"),
      legend.box.margin = margin(0),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8),
    )
}



## PULL FUNCTION ======

#' Pull data from Covid Tracking Project API
#'
#' @param state If NULL (the default), pull data for all US states plus DC.
#'     Otherwise, a 2-character lowercase string with the state's abbreviation,
#'     e.g. "nc".
#' @export
#' @examples
#' us <- ronats_pull()
#' head(us)
#'
#' nc <- ronats_pull(state = "nc")
#' head(nc)
#'
ronats_pull <- function(state = NULL) {

  if (is.null(state)) {
    api <- "https://covidtracking.com/api/v1/us/daily.csv"
  } else {
    api <- paste0("https://covidtracking.com/api/v1/states/", state, "/daily.csv")
  }

  d <- read_csv(
    url(api),
    cols(date = col_date(format = "%Y%m%d")),
    col_names = TRUE
  ) %>% arrange(date) %>% filter(date >= "2020-03-01")
}



## PLOT FUNCTION =====
#' Covid Tracking Project Time Series Plots
#'
#' This function returns a ggplot2 object plotting a Covid Tracking Project
#'   variable with the daily value as a column chart and 7-day, right-aligned
#'   moving averages and LOESS-smoothed trends as line graphs.
#'
#' @param d A data frame with a date column named "date." Intended for use with
#'   Covid Tracking Project data via ronats::ronats_pull()
#' @param v A non-date continuous variable. Intended for use with Covid Tracking
#'   Project variables. See https://covidtracking.com/api for current and
#'   deprecated variables available via the project API.
#' @export
#' @examples
#' nc <- ronats_pull(state = "nc")
#' nc_hosp <- ronats_plot(nc, hospitalizedCurrently)

ronats_plot <- function(d, v, ...) {

  # quoted variable argument
  vquo <- deparse(substitute(v))

  datemin <- d[["date"]][min(which(!is.na(d[[vquo]])))]
  datemax <- d[["date"]][max(which(!is.na(d[[vquo]])))]
  d <- filter(d, date >= datemin & date <= datemax)

  plot_theme <- theme_ronats()

  if ("state" %in% colnames(d)) {
    state <- d[["state"]]
  } else {
    state <- "US"
  }
  plot_title <- paste(vquo, "in", state, "as of", datemax)

  plot_caption <- "Data: Covid Tracking Project | Credit: @NeedsMoreChill"

  datelines <- "#4B9CD3"
  daybars <- "#13294B"
  raline <- "#F29137"
  smoothline <- "#C7DA7D"

  vmax <- d %>% mutate(vmax = max({{v}}, na.rm = TRUE)) %>% pull(vmax)

  p <- ggplot(data = d, aes(x = date)) +
    # scale time axis by min & max represented dates
    scale_x_date(date_labels = ("%B")) +
    # column graph for individual days
    geom_col(aes(y = {{v}}), fill = daybars, color = daybars) +
    # loess-smoothed trend line
    geom_smooth(
      aes(y = {{v}}),
      method = "loess", formula = y ~ x, se = FALSE, na.rm = TRUE,
      color = smoothline, size = 1
    ) +
    # 7 day rolling average
    geom_line(
      aes(y = rollapplyr({{v}}, 7, mean, fill = NA)),
      color = raline,
      size = 1
    ) +
    # labels
    labs(x = "", y = "", caption = plot_caption, title = plot_title) +
    # theme
    plot_theme
}
