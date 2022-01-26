

#' mood_homeostasis
#'
#' Uses the data frame obtained from df_prep to obtain estimates of mood homeostasis for each participant.
#' @param data Data frame. Experience sampling/day reconstruction data that includes variables collecting each individual ID, current mood, current activity, time and any additional variables that the users might want to include in their analyses.
#' @param activity String. Name of activity variable in data.
#' @param global Whether global estimates of the effect of activity on mood are used in the computation.If TRUE, the effect fo each activity on mood is estimated using the pooled data. If FALSE, an individual-specific estimate of the effect of each activity on mood is estimated and used in the computation.
#' @keywords Mood Homeostasis, Affect Dynamics
#' @import dplyr
#' @import rlang
#' @importFrom data.table rbindlist
#' @export

mood_homeostasis <- function(data, activity, global = TRUE){

  #Show Errors/Warnings:

  #Give format to variables
  data$time.bin <- as.factor(data$time.bin)
  data$day <- as.factor(data$day)
  data$id <- as.factor(data$id)

  #Split data by individual:
  data <- dplyr::group_by(data, id)
  split.id <- dplyr::group_split(data)

  #Function to create list of formulas:
  create.formula <- function(df, activity.selected){
    name.lead <- paste0("lead.", activity.selected)
    if(length(unique(df$time.bin))==1 & length(unique(df$day))==1){formula.glm <- as.formula(paste(name.lead, "~", "+ mood +",activity.selected,  " + mood.av"))}
    else if(length(unique(df$time.bin))==1 & length(unique(df$day))!=1){formula.glm <- as.formula(paste(name.lead, "~", "+ mood +",activity.selected,  " + day + mood.av"))}
    else if(length(unique(df$time.bin))!=1 & length(unique(df$day))==1){formula.glm <- as.formula(paste(name.lead, "~", "+ mood +",activity.selected,  " + time.bin + mood.av"))}
    else if(length(unique(df$time.bin))!=1 & length(unique(df$day))!=1){formula.glm <- as.formula(paste(name.lead, "~", "+ mood +",activity.selected,  " + time.bin + day + mood.av"))}
    return(formula.glm)
  }

  #Create df of results (betas):
  var1 <- rep(NA, length(split.id))
  betas.df <- as.data.frame(var1)


  #For each activity:
  for (i in 1:length(activity)) {

    #Get Formula for each individual:
    formulas.ind <- lapply(split.id, create.formula, activity.selected=activity[i])

    #Run regression:
    reg <- function(df, form){
      reg <- stats::glm(formula = form, data = df, family = binomial)
      reg <- summary(reg)
      return(reg$coefficients[2,1])
    }

    res <- Map(reg, split.id, formulas.ind)

    #Save results in betas.df. Each row represents and individual and each variable an activity.
    res <- unlist(res)
    betas.df[,i] <- res
  }

  #Estimate effect of activities in mood:
  #######################################

  if(global == FALSE){
  #Get formula:
  activities.for.formula <- paste0("lead.", activity)
  activities.for.formula <- paste0(activity, sep =" + ", collapse=" ")

  create.formula <- function(df){
    if(length(unique(df$time.bin))==1 & length(unique(df$day))==1){formula.lm <- as.formula(paste("mood.diff ~", activities.for.formula, collapse = ""))}
    else if(length(unique(df$time.bin))==1 & length(unique(df$day))!=1){formula.lm <- as.formula(paste("mood.diff ~", activities.for.formula, " + day", collapse = ""))}
    else if(length(unique(df$time.bin))!=1 & length(unique(df$day))==1){formula.lm <- as.formula(paste("mood.diff ~", activities.for.formula, " + time.bin", collapse = ""))}
    else if(length(unique(df$time.bin))!=1 & length(unique(df$day))!=1){formula.lm <- as.formula(paste("mood.diff ~", activities.for.formula, "+ time.bin + day", collapse = ""))}
    return(formula.lm)
  }

  formulas.ind <- lapply(split.id, create.formula)

  #Prepare function to estimate lm:
  reg2 <- function(df, form2){
    reg <- lm(formula=form2 , data = df)
    return(as.numeric(reg$coefficients[c(2:(length(activity)+1))]))
  }

  #Estimate lm:
  res <- Map(reg2, split.id, formulas.ind)
  res<-lapply(res, t)
  res<-lapply(res, as.data.frame)
  deltas.df <- data.table::rbindlist(res)
  }else if(global == TRUE){

    #Define Formula:
    activities.for.formula <- paste0("lead.", activity)
    activities.for.formula <- paste0(activity, sep =" + ", collapse=" ")
    formula.lm <- as.formula(paste("mood.diff ~", activities.for.formula, "+ time.bin + day", collapse = ""))

    #Regress
    reg <- lm(formula=formula.lm , data = data)
    reg <- as.numeric(reg$coefficients[c(2:(length(activity)+1))])
    reg <- as.data.frame(t(reg))
    deltas.df <- reg[rep(1, each = nrow(betas.df)), ]
  }

  #Split deltas and betas by row:
  betas.df <- split(betas.df, seq(nrow(betas.df)))
  deltas.df <- split(deltas.df, seq(nrow(deltas.df)))

  #Define function for correlation:
  cor.fun <- function(a,b){
    if(length(na.omit(as.numeric(a)))>2){
      co <- stats::cor.test(as.numeric(a), as.numeric(b))
      return(as.numeric(co$estimate))}else{return(NA)}}

  #Run Correlations for all individuals:
  res <- Map(cor.fun, deltas.df, betas.df)
  res<-lapply(res, as.data.frame)
  res <- data.table::rbindlist(res)

  #Add ids
  id.from.list <- sapply(split.id, function(df){df$id[1]})
  res$id <- id.from.list

  #Names:
  names(res) <- c("MoodHomeostasis", "id")
  res <- res[,c("id", "MoodHomeostasis")]

  #Multiply correlation by (-1) to get Mood Homeostasis:
  res$MoodHomeostasis <- (-1)*res$MoodHomeostasis
  return(res)

}
