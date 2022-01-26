

#' df_prep
#'
#' Prepares raw data to run mood homeostasis estimation.
#' @param data Data frame. Experience sampling/day reconstruction data that includes variables collecting each individual ID, current mood, current activity, time and any additional variables that the users might want to include in their analyses.
#' @param id String. Name of participant id variable in data.
#' @param activity String. Name of activity variable in data.
#' @param time String. Name of time stamp variable in data.Variable must be of POSIXct class.
#' @param mood String. Name of mood variable in data.
#' @param max.time Maximum time allowed between two consecutive mood observations (in hours).
#' @keywords Mood Homeostasis, Affect Dynamics
#' @import dplyr
#' @import rlang
#' @import progress
#' @importFrom lubridate is.POSIXct
#' @export

df_prep <- function(data, id, activity, time, mood, max.time){

  #WARNING MESSAGES/ERROR MESSAGES
  if(missing(data) | missing(id) | missing(activity)| missing(time)| missing(mood)| missing(max.time)){stop("Missing function argument.")}
  if(sum(id==names(data)) != 1 | sum(mood==names(data)) != 1 | sum(time==names(data)) != 1){stop("Wrong name specified. Arguments 'id', 'mood', or 'time' do not correspond to the name of unique variables in data.")}
  if(length(activity)<3){stop("Too few activities specified. Need a minimum of 3 activities to estimate mood homeostasis.")}

  #Progress Bar:
  pb <- progress::progress_bar$new(
    format = " :what [:bar] :percent",
    clear = FALSE, total = 4 + length(activity))


  #Name variables:
  pb$tick(tokens = list(what = "Renaming and grouping variables   "))
  names(data)[which(names(data)==id)] <- "id"
  names(data)[which(names(data)==time)] <- "time"
  names(data)[which(names(data)==mood)] <- "mood"
  if(lubridate::is.POSIXct(data$time) == FALSE){stop("Time variable is not POSIXct class. Use as.POSIXct() to convert time variable before running df_prep().")}

  #Group data:
  data <- dplyr::group_by(data, id)


  #Create new variables:
  pb$tick(tokens = list(what = "Creating new variables   "))
  data$day <- format(data$time, format = "%m/%d/%Y")
  data <- dplyr::group_by(data, id, day)
  data <- dplyr::mutate(data, mood.av = mean(mood, na.rm=T))
  data <- dplyr::ungroup(data)
  data <- dplyr::group_by(data, id)
  data <- dplyr::mutate(data, time.diff = dplyr::lead(time, order_by = time) - time )
  data <- dplyr::mutate(data, mood.diff = dplyr::lead(mood, order_by = time) - mood)

  #Create leads of activity variables:
  new.names.activity <- c()
  for (i in 1:length(activity)) {
    pb$tick(tokens = list(what = "Creating new variables   "))
    name.new <- paste0("lead.", activity[i])
    data <- dplyr::mutate(data, !!name.new := dplyr::lead(!!rlang::sym(activity[i]), order_by = time))
    new.names.activity[i] <- name.new
  }

  #Drop observations with excessive time difference
  pb$tick(tokens = list(what = "Dropping observations with excessive time difference  "))
  data <- dplyr::ungroup(data)
  max.time2 <- max.time * 60 * 60
  data <- dplyr::filter(data, time.diff<max.time2)

  #Code time bins
  data$hour <- as.numeric(format(data$time, format = "%H"))
  data$time.bin <- NA
  data$time.bin[data$hour == 0 | data$hour == 1] <- 1
  data$time.bin[data$hour == 2 | data$hour == 3] <- 2
  data$time.bin[data$hour == 4 | data$hour == 5] <- 3
  data$time.bin[data$hour == 6 | data$hour == 7] <- 4
  data$time.bin[data$hour == 8 | data$hour == 9] <- 5
  data$time.bin[data$hour == 10 | data$hour == 11] <- 6
  data$time.bin[data$hour == 12 | data$hour == 13] <- 7
  data$time.bin[data$hour == 14 | data$hour == 15] <- 8
  data$time.bin[data$hour == 16 | data$hour == 17] <- 9
  data$time.bin[data$hour == 18 | data$hour == 19] <- 10
  data$time.bin[data$hour == 20 | data$hour == 21] <- 11
  data$time.bin[data$hour == 22 | data$hour == 23] <- 12

  #Keep variables needed
  pb$tick(tokens = list(what = "Finalizing resulting dataframe   "))
  variables.keep <- c("id", "time","time.diff", "time.bin","day", "mood","mood.av", "mood.diff", activity, new.names.activity)
  data <- data[,variables.keep]

  #remove missing data
  data <- na.omit(data)

  #Need at least 2 observations per individual
  data <- dplyr::ungroup(data)
  data <- dplyr::group_by(data, id)
  data <- dplyr::filter(data, dplyr::n()>2)

  #Drop individuals with no variation in mood:
  data <- dplyr::filter(data, sd(mood)>0)

  return(data)}
