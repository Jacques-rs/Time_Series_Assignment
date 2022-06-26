

joint_adf <- function(df){

    logs <- df %>% dplyr::select(1:13) %>%
        dplyr::select("date" | starts_with("L"))
    non_logs <- df %>% dplyr::select(1:13) %>%
        dplyr::select(date | !starts_with("L"))


    adf_logs <- logs %>%
        mutate(across(names(.)[2:9], function(x) exp(x))) %>%
        mutate(across(names(.)[2:9], function(x) x - lag(x))) %>%
        filter(date > first(date)) %>%
        dplyr::select(-date) %>%
        ts(.) %>%
        sapply(., function(x) adf.test(x))

    adf_non_logs <- non_logs %>%
        mutate(across(names(.)[2:4], function(x) x - lag(x))) %>%
        filter(date > first(date)) %>%
        dplyr::select(-date) %>%
        ts(.) %>%
        sapply(., function(x) adf.test(x))

    df2 <- data.frame(matrix(unlist(adf_logs), nrow=6, byrow=F),
                      row.names = c("statistic", "Lag order",
                                       "Alternative Hypothesis", "p.value",
                                       "delete1", "delete2"))

    df <- data.frame(matrix(unlist(adf_non_logs), nrow=6, byrow=F),
                     row.names = c("statistic", "Lag order",
                                      "Alternative Hypothesis", "p.value",
                                      "delete1", "delete2"))

    names(df2) = names(logs)[2:9]
    names(df) = names(non_logs)[2:5]

    final <- cbind(df, df2)[1:4,]

    return(final)
}