# Collecting the LREERS time series data
# Wrangling the data into the correct format for use throuhgout the assignment

lreer_fetch <- function(data_path="data/HistoricalRateDetail(1970Q1-2022Q1).csv"){
    dataset = read_csv(data_path,
                       skip = 2, show_col_types = F)

    return(dataset)
}

# dataset[dataset$Date == lubridate::ymd(19840930),]; dataset[dataset$Date == lubridate::ymd(19840430),];
# dataset[dataset$Date == lubridate::ymd(19840531),]
#
# head(dataset)




#
# REERS <- read_csv("data/HistoricalRateDetail(1970Q1-2002Q1).csv",
#                                                 skip = 2)
#
# REERS <- REERS |> transmute(Date = as.Date(paste("01-", Date, sep = ""), format = "%d-%b-%Y"), Value = Value) |>
#     transform( Date = ceiling_date(Date, "month") - days(1) ) |>
#     arrange(Date)