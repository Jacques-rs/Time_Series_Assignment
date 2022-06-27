


plot_endog1 <- function(df){

    plot <- df %>%
        dplyr::select(date, LREERS) %>%
        mutate(REERS = exp(LREERS), .keep = "unused") %>%
        ggplot() +
        geom_line(aes(x = date, y = REERS)) +
        scale_y_continuous("Real Effective Exchange Rate") +
        geom_vline(xintercept = c(as.Date("1985-12-31"), as.Date("1986-03-31")),
                    color = "red", show.legend = T)

    return(plot)
}


plot_endog2 <- function(df){

    plot <- df %>%
        mutate(across(names(.[,grepl(x = names(.),
                                     pattern = "^L.*")]),
                      function(x) x = exp(x)),
               .keep = "unused") %>%
        gather(Econ_Measure, Value, 2:13) %>%
        dplyr::select(date, Econ_Measure, Value) %>%
        filter(!Econ_Measure %in% c("LREERS", "LPR2GOLD", "LPR2COMM3",
                                   "LPRCOMM3", "LPRCOMM5", "LPRGOLD")) %>%
        ggplot() +
        geom_line(aes(x = date, y = Value, linetype = Econ_Measure,
                      color = Econ_Measure)) +
        facet_wrap(~Econ_Measure, scales = "free_y", ncol = 2) +
        scale_color_hue(labels = c("Fiscal Balance",
                                       "Real Commodity\nPrices",
                                       "Relative Real\nGDP per Capita",
                                       "Net Foreign\nAssets",
                                       "Openness Indicator",
                                       "Relative Real\nInterest Rate")) +
        theme(legend.title = element_blank()) +
        scale_linetype(guide = "none") #+
        # ggthemes::theme_economist()

    return(plot)
}

plot_endog_joint <- function(df){

    plot <- df %>%
        # mutate(across(names(.[,grepl(x = names(.),
        #                              pattern = "^L.*")]),
        #               function(x) x = exp(x)),
        #        .keep = "unused") %>%
        # # To allow for a more clear comparison of the co-movements
        # mutate(OPENY = log(OPENY)) %>%
        gather(Econ_Measure, Value, 2:13) %>%
        dplyr::select(date, Econ_Measure, Value) %>%
        filter(!Econ_Measure %in% c("LPR2GOLD", "LPR2COMM3",
                                    "LPRCOMM3", "LPRCOMM5", "LPRGOLD")) %>%
        ggplot() +
        geom_line(aes(x = date, y = Value, linetype = Econ_Measure,
                      color = Econ_Measure)) +
        facet_wrap(~Econ_Measure, scales = "free", ncol = 2) +
        scale_color_hue(labels = c("Fiscal Balance",
                                   "Real Commodity\nPrices",
                                   "REER",
                                   "Relative Real\nGDP per Capita",
                                   "Net Foreign Assets\nincluding the\nForward Book",
                                   "Openness Indicator",
                                   "Relative Real\nInterest Rate")) +
        theme(legend.title = element_blank()) +
        scale_linetype(guide = "none") #+
    # ggthemes::theme_economist()

    return(plot)
}

plot_endog1_log <- function(df){

    plot <- df %>%
        dplyr::select(date, LREERS) %>%
        # mutate(REERS = exp(LREERS), .keep = "unused") %>%
        ggplot() +
        geom_line(aes(x = date, y = LREERS)) +
        scale_y_continuous("Real Effective Exchange Rate") +
        geom_vline(xintercept = c(as.Date("1985-12-31"), as.Date("1986-03-31")),
                   color = "red", show.legend = T)

    return(plot)
}

plot_endog2_log <- function(df){

    plot <- df %>%
        # mutate(across(names(.[,grepl(x = names(.),
        #                              pattern = "^L.*")]),
        #               function(x) x = exp(x)),
        #        .keep = "unused") %>%
        gather(Econ_Measure, Value, 2:13) %>%
        dplyr::select(date, Econ_Measure, Value) %>%
        filter(!Econ_Measure %in% c("LREERS", "LPR2GOLD", "LPR2COMM3",
                                    "LPRCOMM3", "LPRCOMM5", "LPRGOLD")) %>%
        ggplot() +
        geom_line(aes(x = date, y = Value, linetype = Econ_Measure,
                      color = Econ_Measure)) +
        facet_wrap(~Econ_Measure, scales = "free_y", ncol = 2) +
        scale_color_hue(labels = c("Fiscal Balance",
                                   "Real Commodity\nPrices",
                                   "Relative Real\nGDP per Capita",
                                   "Net Foreign Assets\nincluding the\nForward Book",
                                   "Openness Indicator",
                                   "Relative Real\nInterest Rate")) +
        theme(legend.title = element_blank()) +
        scale_linetype(guide = "none") #+
    # ggthemes::theme_economist()

    return(plot)
}
