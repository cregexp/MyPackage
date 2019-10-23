#' Read file 
#'
#' This function reads data from .csv file from the US
#' National Highway Traffic Safety Administration's Fatality Analysis
#' Reporting System (FARS), which is a nationwide census, providing the
#' American public yearly data, regarding fatal injuries suffered in motor
#' vehicle traffic crashes.
#'
#' @details For more information, see:
#'   \item{\url{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}}
#'  
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @param filename A character string with the name of the file to read
#' 
#'
#' @return A data frame with data readed from the csv file, or an error if the
#'   file does not exists.
#'
#' @examples
#' library(dplyr)
#' library(readr)
#' yr <- 2015
#' data <- yr %>% fars_read
#' head(data)
#'
#' @export

fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' Make file name
#
#' This funciotn makes .csv data file name taking into account the given.
#'
#' @param year An integer with the input
#'
#' @return This function returns a string with the data file name for a given
#'   year, and the file path within the package.
#'
#' @examples
#' make_filename(2013)
#'
#' @export

make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}


#' Read years
#'
#' This function selects data taking into account the month
#'
#' @param years A vector with a list of years
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr "%>%"
#' @return A data.frame including entries in data by month, or NULL if the
#'  year is not valid
#'
#' @examples
#' fars_read_years(2013)
#' @export

fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>% 
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}


#' Summarize data by years
#'
#' This function summarizes yearly accidents, by month
#'
#' @param years A vector with a list of years to summarize 
#'
#' @return A data.frame with number of accidents by years summarized by month
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @importFrom magrittr "%>%"
#' @examples
#' fars_summarize_years(c(2015, 2014))
#' @export


fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}


#' Accidents by state and year
#'
#' A funcion to make a plot with a state map showing the accidents location by year
#' If the state.num is invalid the function shows an error
#'
#' @param state.num An Integer with the State Code
#' @param year A string, or an integer, with the input year
#'
#' @importFrom maps map
#' @importFrom dplyr filter
#' @importFrom graphics points
#'
#' @return A plot showing a state map with the accidents location by year
#'
#' fars_map_state(49, 2015)
#' @export

fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}