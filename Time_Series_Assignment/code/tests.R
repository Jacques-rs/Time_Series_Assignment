

joint_adf_diff <- function(df){

    df <- df %>% dplyr::select(1:8)
    logs <- df %>%
        dplyr::select("date" | starts_with("L"))
    non_logs <- df %>%
        dplyr::select(date | !starts_with("L"))


    adf_logs <- logs %>%
        # mutate(across(names(.)[2:9], function(x) exp(x))) %>%
        mutate(across(names(.)[2:4], function(x) x - lag(x))) %>%
        filter(date > first(date)) %>%
        dplyr::select(-date) %>%
        ts(.) %>%
        sapply(., function(x) adf.test(x))

    adf_non_logs <- non_logs %>%
        mutate(across(names(.)[2:5], function(x) x - lag(x))) %>%
        filter(date > first(date)) %>%
        dplyr::select(-date) %>%
        ts(.) %>%
        sapply(., function(x) adf.test(x))

    df2 <- data.frame(matrix(unlist(adf_logs), nrow=3, byrow=T),
                      row.names = c("D_LREERS",
                                    "D_LRGDPPCR",
                                    "D_LPR2COMM5")) %>%
        dplyr::select(-c(X5, X6)) %>%
        rename(Statistic = X1, Lags = X2, Null_Hypothesis = X3,
               p_value = X4) %>%
        mutate(across(names(.)[-3], function(x) as.double(x))) %>%
        mutate(across(names(.)[-3], function(x) round(x, 4)))

    df3 <- data.frame(matrix(unlist(adf_non_logs), nrow=4, byrow=T),
                      row.names = c("D_RIRR",
                                    "D_OPENY",
                                    "D_FBYA",
                                    "D_NFAOFPY")) %>%
        dplyr::select(-c(X5, X6)) %>%
        rename(Statistic = X1, Lags = X2, Null_Hypothesis = X3,
               p_value = X4) %>%
        mutate(across(names(.)[-3], function(x) as.double(x))) %>%
        mutate(across(names(.)[-3], function(x) round(x, 4)))

    final <- rbind(df3, df2)

    return(final)
}


joint_adf <- function(df){
    df <- df %>% dplyr::select(1:8)
    logs <- df %>%
        dplyr::select("date" | starts_with("L"))
    non_logs <- df %>%
        dplyr::select(date | !starts_with("L"))


    adf_logs <- logs %>%
        # mutate(across(names(.)[2:9], function(x) exp(x))) %>%
        dplyr::select(-date) %>%
        ts(.) %>%
        sapply(., function(x) adf.test(x))

    adf_non_logs <- non_logs %>%
        dplyr::select(-date) %>%
        ts(.) %>%
        sapply(., function(x) adf.test(x))

    df2 <- data.frame(matrix(unlist(adf_logs), nrow=3, byrow=T),
                      row.names = c("LREERS",
                                    "LRGDPPCR",
                                    "LPR2COMM5")) %>%
        dplyr::select(-c(X5, X6)) %>%
        rename(Statistic = X1, Lags = X2, Null_Hypothesis = X3,
               p_value = X4) %>%
        mutate(across(names(.)[-3], function(x) as.double(x))) %>%
        mutate(across(names(.)[-3], function(x) round(x, 4)))

    df3 <- data.frame(matrix(unlist(adf_non_logs), nrow=4, byrow=T),
                     row.names = c("RIRR",
                                   "OPENY",
                                   "FBYA",
                                   "NFAOFPY")) %>%
        dplyr::select(-c(X5, X6)) %>%
        rename(Statistic = X1, Lags = X2, Null_Hypothesis = X3,
               p_value = X4) %>%
        mutate(across(names(.)[-3], function(x) as.double(x))) %>%
        mutate(across(names(.)[-3], function(x) round(x, 4)))

    final <- rbind(df3, df2)

    return(final)
}

# joint_gls <- function(df){
#
#     df <- df %>% dplyr::select(1:8)
#     drift <- df %>%
#         dplyr::select(LRGDPPCR)
#     constant <- df %>%
#         dplyr::select(!LRGDPPCR|!date)
#
#
#     adf_constant <- constant %>%
#         # mutate(across(names(.)[2:9], function(x) exp(x))) %>%
#         ts(.) %>%
#         sapply(., function(x) ur.ers(x, type = "DF-GLS", model = "constant"))
#
#     adf_drift <- drift %>%
#         ts(.) %>%
#         sapply(., function(x) ur.ers(x, type = "DF-GLS", model = "trend"))
#
#     df_constant <- data.frame(matrix(unlist(adf_constant), nrow=6, byrow=T),
#                       row.names = c("LREERS",
#                                     "LPR2COMM5",
#                                     "RIRR",
#                                     "OPENY",
#                                     "FBYA",
#                                     "NFAOFPY")) %>%
#         dplyr::select(-c(X5, X6)) %>%
#         rename(Statistic = X1, Lags = X2, Null_Hypothesis = X3,
#                p_value = X4) %>%
#         mutate(across(names(.)[-3], function(x) as.double(x))) %>%
#         mutate(across(names(.)[-3], function(x) round(x, 4)))
#
#     df_drift <- data.frame(matrix(unlist(adf_drift), nrow=1, byrow=T),
#                       row.names = c("LRGDPPCR")) %>%
#         dplyr::select(-c(X5, X6)) %>%
#         rename(Statistic = X1, Lags = X2, Null_Hypothesis = X3,
#                p_value = X4) %>%
#         mutate(across(names(.)[-3], function(x) as.double(x))) %>%
#         mutate(across(names(.)[-3], function(x) round(x, 4)))
#
#     final <- rbind(df3, df2)
#
#     return(final)
#
# }
#
# ca.jo_custom <- function(df = full_df, i = c("8", "9", "10", "11", "12", "13"),
#                          outliers = T){
#     i <- match.arg(i)
#     # i <- as.integer(i)
#     if(outliers){
#         mod <- df %>% dplyr::select(2:7|as.integer(i)) %>%
#             ca.jo(x = ., type = "eigen",
#                   ecdet = "const",
#                   spec = "transitory", K = 2,
#                   dumvar = exog_df[,2:8])
#     }else{
#         mod <- df %>% dplyr::select(2:7|as.integer(i)) %>%
#             ca.jo(x = ., type = "eigen",
#                   ecdet = "const",
#                   spec = "transitory", K = 2,
#                   dumvar = exog_df[,2:4])
#     }
#     return(mod)
# }

# trend_test <- function(df){
#
#     joint_adf <- function(df){
#
#         logs <- df %>% dplyr::select(1:13) %>%
#             dplyr::select("date" | starts_with("L"))
#         non_logs <- df %>% dplyr::select(1:13) %>%
#             dplyr::select(date | !starts_with("L"))
#
#
#         adf_logs <- logs %>%
#             mutate(across(names(.)[2:9], function(x) exp(x))) %>%
#             dplyr::select(-date) %>%
#             ts(.) %>%
#             sapply(., function(x) adf.test(x))
#
#         adf_non_logs <- non_logs %>%
#             dplyr::select(-date) %>%
#             ts(.) %>%
#             sapply(., function(x) adf.test(x))
#
#         df2 <- data.frame(matrix(unlist(adf_logs), nrow=6, byrow=F),
#                           row.names = c("statistic", "Lag order",
#                                         "Alternative Hypothesis", "p.value",
#                                         "delete1", "delete2"))
#
#         df <- data.frame(matrix(unlist(adf_non_logs), nrow=6, byrow=F),
#                          row.names = c("statistic", "Lag order",
#                                        "Alternative Hypothesis", "p.value",
#                                        "delete1", "delete2"))
#
#         names(df2) = names(logs)[2:9]
#         names(df) = names(non_logs)[2:5]
#
#         final <- cbind(df, df2)[1:4,]
#
#         return(final)
#     }
#
# }