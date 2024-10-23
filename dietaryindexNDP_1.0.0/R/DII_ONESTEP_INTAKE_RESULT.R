#' DII_ONESTEP_INTAKE_RESULT: Retrieve Pre-computed Additional Components within the DII Calculation
#' @description
#' This function retrieves pre-computed results from the DII_ONESTEP_INTAKE() function, allowing researchers to extract additional components within the Dietary Inflammatory Index(DII) calculation for specific NHANES cycles as needed for their analysis.
#'
#' @param NHANESCycle A character string indicating the NHANES cycle (e.g., "0102", "0304").
#' @param DAY A character string indicating which day's result to retrieve ('first', 'second', 'both').
#' @return A data frame containing the additional components within the DII calculation for the specified NHANES cycle. If the specified cycle is not found, an error message will be returned.
#' @export
#'
#' @examples
#' DII_ONESTEP_INTAKE_RESULT(NHANESCycle = "1112")
DII_ONESTEP_INTAKE_RESULT <- function(NHANESCycle = c('0102', '0304', '0506', '0708', '0910',
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
      cat('\n  Since there is only dietary intake data of one day included in NHANES 2001-2002, only the additional components within the DII calculation for that day will be retrieved.\n')
    }
    load(file = paste0(system.file(package = 'dietaryindexNDP'), '/data/ONESTEP_INTAKE_', cycle, ".RData"))
    ONESTEP_INTAKE <- get(paste0('ONESTEP_INTAKE_', cycle))
    remove(list = paste0('ONESTEP_INTAKE_', cycle))
    cat('\n  Note: The additional components from NHANES 2000-2020 include GARLIC, GINGER, PEPPER, ONION, and THYME/OREGANO. Whereas, flavonoids are included only in NHANES 2007-2010 and 2017-2020.\n')
    return(ONESTEP_INTAKE)
  }else{
    if(day == 'both'){
      load(file = paste0(system.file(package = 'dietaryindexNDP'), '/data/ONESTEP_INTAKE_', cycle, ".RData"))
      ONESTEP_INTAKE <- get(paste0('ONESTEP_INTAKE_', cycle))
      remove(list = paste0('ONESTEP_INTAKE_', cycle))
      ONESTEP_INTAKE_Both <- ONESTEP_INTAKE %>%
        dplyr::filter(!if_all(-SEQN, is.na))
      cat('\n  Note: The additional components from NHANES 2000-2020 include GARLIC, GINGER, PEPPER, ONION, and THYME/OREGANO. Whereas, flavonoids are included only in NHANES 2007-2010 and 2017-2020.\n')
      return(ONESTEP_INTAKE_Both)
    }

    if(day == 'first'){
      load(file = paste0(system.file(package = 'dietaryindexNDP'), '/data/ONESTEP_INTAKE_', cycle, ".RData"))
      ONESTEP_INTAKE <- get(paste0('ONESTEP_INTAKE_', cycle))
      remove(list = paste0('ONESTEP_INTAKE_', cycle))
      ONESTEP_INTAKE_DAY1 <- ONESTEP_INTAKE %>%
        dplyr::select(SEQN, starts_with("DR1"), starts_with("dr1")) %>%
        dplyr::filter(!if_all(-SEQN, is.na))
      cat('\n  Note: The additional components from NHANES 2000-2020 include GARLIC, GINGER, PEPPER, ONION, and THYME/OREGANO. Whereas, flavonoids are included only in NHANES 2007-2010 and 2017-2020.\n')
      return(ONESTEP_INTAKE_DAY1)
    }

    if(day == 'second'){
      load(file = paste0(system.file(package = 'dietaryindexNDP'), '/data/ONESTEP_INTAKE_', cycle, ".RData"))
      ONESTEP_INTAKE <- get(paste0('ONESTEP_INTAKE_', cycle))
      remove(list = paste0('ONESTEP_INTAKE_', cycle))
      ONESTEP_INTAKE_DAY2 <- ONESTEP_INTAKE %>%
        dplyr::select(SEQN, starts_with("DR2"), starts_with("dr2")) %>%
        dplyr::filter(!if_all(-SEQN, is.na))
      cat('\n  Note: The additional components from NHANES 2000-2020 include GARLIC, GINGER, PEPPER, ONION, and THYME/OREGANO. Whereas, flavonoids are included only in NHANES 2007-2010 and 2017-2020.\n')
      return(ONESTEP_INTAKE_DAY2)
    }
  }
}
