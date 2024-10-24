#' DII_NHANES_PLUS_RESULT: Extract Precomputed DII Results for NHANES Cycles
#' @description
#' This function allows researchers to retrieve precomputed Dietary Inflammatory Index (DII) results for specified NHANES cycles from stored data. It provides a convenient way to access DII values without recalculating them, saving time and computational resources.
#'
#' @param NHANESCycle A character string indicating the NHANES cycle of interest (e.g., "0102", "0708").
#' @param DAY A character string indicating which day's result to retrieve ('first', 'second', 'both').
#'
#' @return A dataframe containing the DII and its component values for the specified NHANES cycle. DII represents the sum of all component values, while DII_NOETOH is the DII value excluding alcohol.
#' @export
#'
#' @examples
#' # Retrieve DII values for NHANES 2009-2010 on Day 1
#' DII_DAY1_0910 <- DII_NHANES_PLUS_RESULT(NHANESCycle = "2009-2010", DAY = c('first'))
#' # Retrieve DII values for a NHANES 2015-2016 on Day 2
#' DII_DAY2_1112 <- DII_NHANES_PLUS_RESULT(NHANESCycle = "2011-2012", DAY = c('second'))
#' # Retrieve DII values for a NHANES 2007-2008 on both Day 1 and Day 2
#' DII_Both_0708 <- DII_NHANES_PLUS_RESULT(NHANESCycle = "2007-2008", DAY = c('both'))
DII_NHANES_PLUS_RESULT <- function(NHANESCycle = c('0102', '0304', '0506', '0708', '0910',
                                                   '1112', '1314', '1516', '1718', '1720'),
                                   DAY = c('first', 'second', 'both')){
  cycle <- NHANESCycle
  day <- DAY
  if(!cycle %in% c('0102', '0304', '0506', '0708', '0910',
                   '1112', '1314', '1516', '1718', '1720')){
    stop('  NHANESCycle should be one of the following values: 0102, 0304, 0506, 0708, 0910, 1112, 1314, 1516, 1718 or 1720')
  }
  if(!day %in% c('first', 'second', 'both')){
    stop("  DAY should be either 'first', 'second', or 'both'.")
  }

  if(cycle == c('0102')){
    if(day %in% c('second', 'both')){
      cat('\n  Since there is only dietary intake data of one day included in NHANES 2001-2002, only the DII for that day will be retrieved.\n')
    }
    load(file = paste0(system.file(package = 'dietaryindexNDP'), '/data/DII_Both_', cycle, ".RData"))
    DII_Both <- get(paste0('DII_', cycle))
    remove(list = paste0('DII_', cycle))
    return(DII_Both)
  }else{
    if(day == 'both'){
      load(file = paste0(system.file(package = 'dietaryindexNDP'), '/data/DII_Both_', cycle, ".RData"))
      DII_Both <- get(paste0('DII_', cycle))
      remove(list = paste0('DII_', cycle))
      return(DII_Both)
    }
    if(day == 'first'){
      load(file = paste0(system.file(package = 'dietaryindexNDP'), '/data/DII_Day1_', cycle, ".RData"))
      DII_Day1 <- get(paste0('DII_Day1_', cycle))
      remove(list = paste0('DII_Day1_', cycle))
      return(DII_Day1)
    }
    if(day == 'second'){
      load(file = paste0(system.file(package = 'dietaryindexNDP'), '/data/DII_Day2_', cycle, ".RData"))
      DII_Day2 <- get(paste0('DII_Day2_', cycle))
      remove(list = paste0('DII_Day2_', cycle))
      return(DII_Day2)
    }
  }
}

