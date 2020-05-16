
#' Fetch Latest COVID19 data.
#' 
#' Retrieve latest data provided by 
#' John Hopkins University (Baltimore, US), 
#' from GitHub: "CSSEGISandData/COVID-19/".
#' 
#' @param  US.only logical, shall US-only be downloaded. If FALSE, global
#' data are downloaded.
#'
#' @return list including three elements:
#' \describe{
#'   \item{cse.df}{data.frame including COVID19 cumulative cases data}
#'   \item{dth.df}{data.frame including COVID19 cumulative death data}
#'   \item{US.only}{logical, value of the US.only argument}
#' }   
#' 
#' @importFrom utils read.csv
#' 
#' @examples
#' library(Covid19CasesPlot)
#' x <- fetch_latest_covid19_data()
#' 
#' @export
fetch_latest_covid19_data <- function(US.only = FALSE)
{
  if (US.only) {
    x_cse <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
    x_dth <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
  } else {
    x_cse <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
    x_dth <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
  }
  
  cse_df <- utils::read.csv(x_cse, as.is = TRUE)
  dth_df <- utils::read.csv(x_dth, as.is = TRUE)
  
  return(list(cse.df = cse_df,
              dth.df = dth_df,
              US.only = US.only))
}


#' Get All Available Geo Areas.
#' 
#' Retrieve a character vector including all valid Geographical 
#' Areas (Countries or US States). 
#' 
#' @param covid19.data, list returned by fetch_latest_covid19_data(). 
#' It includes worldwide or US COVID19 infection data.
#'
#' @return a character vector including all valid geographical areas.
#'
#' @examples
#' library(Covid19CasesPlot)
#' x <- fetch_latest_covid19_data()
#' head(get_available_geo_areas(x))
#' 
#' @export
get_available_geo_areas <- function(covid19.data) 
{
  geoColumn <- "Country.Region"
  if (covid19.data$US.only) {
    geoColumn <- "Province_State"
  }
  
  xa <- covid19.data$cse.df
  xb <- covid19.data$dth.df
  
  xa <- base::sort(unique(xa[, geoColumn]))
  xb <- base::sort(unique(xb[, geoColumn]))
  
  xx <- xa[xa %in% xb]
  
  return(xx)
}


#' Get All Available Dates.
#' 
#' Retrieve some summary data about the dates/time points included in the 
#' dataset.
#' 
#' @param covid19.data, list returned by fetch_latest_covid19_data(). 
#' It includes worldwide or US COVID19 infection data.
#'
#' @return list, including 4 elements:
#'   * first.day: date of the first data point
#'   * last.day: date of the latest data point
#'   * window: integer, number of days in the window
#'   * data.window: integer, number of data points
#'
#' @examples
#' library(Covid19CasesPlot)
#' x <- fetch_latest_covid19_data()
#' get_available_dates(x)
#' 
#' @export
get_available_dates <- function(covid19.data) 
{
  x <- covid19.data$cse.df
  xx <- colnames(x)
  xx <- grep("^X", xx, value = TRUE)
  xx <- sub("^X", "", xx)
  xx <- as.Date(xx, format = "%m.%d.%y")
  xx <- sort(xx)
  first_day <- xx[1]
  last_day <- xx[length(xx)]
  yy <- list(first.day = first_day, 
             last.day = last_day,
             window = 1 + as.numeric(difftime(time2 = first_day, time1 = last_day, units = "days")),
             data.window = length(xx))
  
  return(yy)
}


#' Plot Numbers of New COVID19 Cases and Deaths. 
#' 
#' Visualize the trend of new cases and deaths due to COVID-19 infection
#' in a country of interest. A ggplot2-object is returned. Points indicate 
#' the number of new cases or deaths measured day by day. Trendlines are 
#' computed by 'loess'.
#'  
#' @param covid19.data, list returned by fetch_latest_covid19_data(). 
#' It includes worldwide COVID19 infection data.
#' @param geo.area, string indicate the country or the US-state of interest. 
#' For example, 'US', 'Italy' (country), or "Alabama" (US-state). 
#' @param y.limit, integer of length 1. This is the maximum value used
#' in the y axis (New cases). If NULL, it is automatically set to the 
#' 95th percentile. 
#' @param dth.scale.k, integer of length 1. This is the coefficient 
#' used to scale the secondary y-axis (deaths counts). If NULL, it
#' is automatically computed. Typically, reasonable numbers range between 3 (US, 
#' United Kingdom, Italy), and 30 (Germany, Japan).  
#'
#' @return a ggplot-class object.
#'   
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth 
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous 
#' @importFrom ggplot2 sec_axis scale_color_manual ggtitle 
#' @importFrom ggplot2 theme element_text element_rect element_text
#'
#' @examples
#' library(Covid19CasesPlot)
#' x <- fetch_latest_covid19_data()
#' plot_covid19_data(x, "US")
#' 
#' @export
plot_covid19_data <- function(covid19.data, 
                              geo.area,  
                              y.limit = NULL, 
                              dth.scale.k = NULL) {
  
  day <- NULL; count <- NULL; var <- NULL; COVID19 <- NULL; counts <- NULL
  
  # prep input data
  gl_cse_df = covid19.data$cse.df
  gl_dth_df = covid19.data$dth.df
  
  geoColumn <- "Country.Region"
  if (covid19.data$US.only) {
    geoColumn <- "Province_State"
  }
  
  # cases
  x <- gl_cse_df
  cid <- which(grepl("^X[[:digit:]].*", colnames(x)))
  tots <- as.numeric(apply(x[x[, geoColumn] == geo.area, cid], 2, sum, na.rm = TRUE))
  ln <- length(tots)
  c.diffs <- tots - c(0, tots[-c(length(tots))])
  
  # deaths
  x <- gl_dth_df
  cid <- which(grepl("^X[[:digit:]].*", colnames(x)))
  tots <- as.numeric(apply(x[x[, geoColumn] == geo.area, cid], 2, sum, na.rm = TRUE))
  ln <- length(tots)
  d.diffs <- tots - c(0, tots[-c(length(tots))])
  
  if(length(d.diffs) != length(c.diffs))
    stop("Bad data, try again later")
  
  y <- data.frame(day = 1:length(d.diffs), 
                  new.cases = c.diffs, 
                  deaths = d.diffs)
  
  yy <- reshape2::melt(data = y, 
                       id.vars = "day", 
                       measure.vars = c("new.cases", "deaths"), 
                       variable.name = "COVID19", 
                       value.name = "counts")
  
  # last day
  ycsmx <- yy[yy$day == max(yy$day) & yy$COVID19 == "new.cases",]$counts
  ydtmx <- yy[yy$day == max(yy$day) & yy$COVID19 == "deaths",]$counts
  xmx <- max(yy$day)
  
  # Rearrange deaths scales
  max.nc <- quantile(yy$counts[yy$COVID19 == "new.cases"], probs = 0.925, na.rm = TRUE)
  max.dn <- quantile(yy$counts[yy$COVID19 == "deaths"], probs = 0.925, na.rm = TRUE)
  
  my.k <- ceiling((max.nc - 0.66 * max.nc) / max.dn )
  if (!is.null(dth.scale.k) || is.numeric(dth.scale.k)) {
    my.k <- dth.scale.k
  }
  yy$counts[yy$COVID19 == "deaths"] <- my.k * yy$counts[yy$COVID19 == "deaths"]
  
  yLims <- c(1:9) * 10
  yLims <- do.call(c, lapply(1:5, function(j) {
    as.numeric(paste0(yLims, paste(rep('0', times = j), collapse = "")))
  }))
  
  if (is.null(y.limit) || !is.numeric(y.limit)) {
    y.limit <- 1.15 * as.numeric(quantile(yy$counts, probs = 0.95, na.rm = TRUE))
    i <- min(which(yLims > y.limit), na.rm = TRUE)
    y.limit <- yLims[i]
  }
  
  dtx <- colnames(gl_cse_df)
  dtx <- grep("^X", dtx, value = TRUE)
  dtx <- sub("^X", "", dtx)
  dtxx <- as.Date(dtx, format = "%m.%d.%y")
  dtxxx <- data.frame(i = y$day, 
                      dates = dtxx, 
                      day = as.numeric(format(dtxx, "%d")),
                      month = as.numeric(format(dtxx, "%m")),
                      year = as.numeric(format(dtxx, "%y")),
                      fdat = as.character(format(dtxx, "%b-%d")))
  xbrk.i <- c(min(dtxxx$i), max(dtxxx$i)) 
  xbrk.i <- c(xbrk.i, dtxxx$i[dtxxx$day == 1])
  xbrk.i <- sort(unique(xbrk.i))
  
  # last day 2
  ydtmx2 <- yy[yy$day == max(yy$day) & yy$COVID19 == "deaths",]$counts
  
  plt1 <- suppressWarnings(
    suppressMessages(
      ggplot2::ggplot(yy, 
                      ggplot2::aes(x = day, y = counts, color = COVID19)) +
        ggplot2::geom_point(size  = 2)+ ggplot2::geom_smooth(method = "loess", span = 0.3, na.rm = TRUE) +
        ggplot2::scale_x_continuous(name = "", breaks = xbrk.i, 
                                    labels = dtxxx$fdat[dtxxx$i %in% xbrk.i], 
                                    limits = c(1, (xmx + 4))) +
        
        ggplot2::scale_y_continuous(name = "New Cases per day", 
                                    limits = c(0, max(y.limit, max(yy$counts, na.rm = TRUE))),
                                    sec.axis = ggplot2::sec_axis(~./my.k, name = "Deaths per day")) +
        
        ggplot2::coord_cartesian(ylim = c(0, y.limit)) +
        
        ggplot2::scale_color_manual(values = c("#ff7f00", "#386cb0")) +
        ggplot2::ggtitle(label = paste0("COVID19 in ", geo.area)) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                       legend.position = "bottom",
                       panel.background = ggplot2::element_rect(fill = "gray95"), 
                       axis.text.x = ggplot2::element_text(angle = 90, hjust = 0, vjust = 0.5))
    )
  )
  
  plt1 <-  plt1 + 
    ggplot2::annotate(geom = 'segment' , x=xmx, xend = Inf, y = ydtmx2, yend = ydtmx2, 
                      size = 0.45, alpha = 0.9, color = "#386cb0", linetype="dotted") +
    ggplot2::annotate(geom = 'segment' , x=xmx, xend = -Inf, y = ycsmx, yend = ycsmx, 
                      size = 0.45, alpha = 0.9, color = "#ff7f00", linetype="dotted") +
    ggplot2::annotate(geom = 'text' , x=-Inf, y = ycsmx, hjust = -0.25, vjust = -0.25, 
                      size = 3.5, fontface = "bold.italic", alpha = 0.9, color = "#de740b", 
                      label = ycsmx) +
    ggplot2::annotate(geom = 'text' , x=+Inf, y = ydtmx2, hjust = 1.25, vjust = -0.25, 
                      size = 3.5, fontface = "bold.italic", alpha = 0.9, color = "#235597", 
                      label = ydtmx) + 
    ggplot2::theme(axis.title.y.left = ggplot2::element_text(color = "#de740b"), 
                   axis.text.y.left =  ggplot2::element_text(color = "#de740b"),
                   axis.title.y.right  = ggplot2::element_text(color = "#235597"), 
                   axis.text.y.right =  ggplot2::element_text(color = "#235597"))
  
  return(plt1)
}


#' Plot COVID19 Patterns Using Data from John Hopkins University.  
#' 
#' Retrieve latest COVID-19 data provided by John Hopkins University (Baltimore, US), 
#' obtained from GitHub: "CSSEGISandData/COVID-19/". For more info, please
#' see: \url{https://systems.jhu.edu/research/public-health/ncov/}. 
#' Visualize the trend of new cases and deaths due to COVID-19 infection
#' in a country or state of interest. Points indicate 
#' the number of new cases or deaths measured day by day. Trendlines are 
#' computed by 'loess'.
#' 
#' @references 
#' Info and resources are available at the following URLs:
#' \enumerate{
#'   \item{\strong{JHU website}: \url{https://systems.jhu.edu/research/public-health/ncov/} }
#'   \item{\strong{GitHub library}: \url{https://github.com/dami82/Covid19CasesPlot} }
#'   \item{\strong{Daily Charts}: \url{https://systems.jhu.edu/research/public-health/ncov/} }
#'  }
#'
#' @examples 
#' library(Covid19CasesPlot)
#' library(ggplot2)
#' x1 <- fetch_latest_covid19_data()
#' get_available_dates(x1)
#' get_available_geo_areas(x1)[1:10]
#' p1 <- plot_covid19_data(covid19.data = x1, geo.area = "Italy")
#' p1
#'
#' @docType package
#' @name Covid19CasesPlot-package
NULL




