# Collecting the LREERS time series data
# Wrangling the data into the correct format for use throuhgout the assignment

# lreer_fetch <- function(data_path="data/reer(1970Q1-2022Q1).csv"){
#     dataset = read_csv(data_path,
#                        skip = 2, show_col_types = F)
#
#     return(dataset)
# }
#
# rirr_fetch <- function(path = "./data/nominal_gov_bond_10year.csv"){
#     df <- read_csv(path, show_col_types = F) %>%
#         rename(RIRR = IRLTLT01ZAQ156N) %>%
#         rename(date = DATE) %>%
#         mutate(date = date - 1) %>%
#         filter(date >= lubridate::ymd(19700131))
#
#     return(df)
# }

fetch_full <- function(path = "./data/full_dataset.xls"){
    df <- readxl::read_xls(path)

    return(df)
}