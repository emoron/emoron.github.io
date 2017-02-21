popreportdata <- dlply(population, .(continent),
                       +     function(df) {
                         +         continent <- gsub(" ", "_", unique(df$continent))
                         +         filename <- function(y) {
                           +             paste("graphs\", continent, y, ".pdf",
                                               +                 sep = "")
                                               +         }
                                               +         forecast <- subset(df, variant != "Estimate variant")
                                               +         forecast$variant <- forecast$variant[,
                                               +             drop = TRUE]
                                               +         historic <- subset(df, variant == "Estimate variant")
                                               +         historic <- ddply(historic, .(continent,
                                               +             year), transform, cont_value = sum(value))
                                               +         current <- subset(df, year == 2005)
                                               +         growthrate <- function(df) {
                                               +             rng <- range(df$year)
                                               +             min_value <- df[df$year == rng[1],
                                               +                 "value"]
                                               +             max_value <- df[df$year == rng[2],
                                               +                 "value"]
                                               +             abs_growth <- max_value/min_value
                                               +             yr5_growth <- abs_growth^(1/length(df$year))
                                               +             growthdf <- data.frame(min_value,
                                               +                 max_value, abs_growth, yr5_growth)
                                               +             names(growthdf)[1:2] <- c(rng[1],
                                               +                 rng[2])
                                               +             growthdf
                                               +         }
                                               +         growth <- ddply(forecast, .(continent,
                                               +             country, variant), growthrate)
                                               +         growth$variant <- factor(growth$variant,
                                               +             levels = c("Constant-fertility scenario",
                                               +                 "High variant", "Medium variant",
                                               +                 "Low variant"))
                                               +         growth <- sort_df(growth, vars = c("continent",
                                               +             "variant", "abs_growth"))
                                               +         blabel <- c(0.01, 0.1, 1, 10, 100)
                                               +         alabel <- "Population (in millions)"
                                               +         phist <- ggplot(current, aes(value)) +
                                               +             geom_histogram(binwidth = 0.5, fill = NA,
                                               +                 colour = "black") + scale_x_log10(breaks = blabel,
                                               +             labels = blabel) + labs(x = alabel)
                                               +         ggsave(filename("_hist"), phist, dpi = 100)
                                               +         prank <- ggplot(current, aes(seq_along(country),
                                               +             rev(sort(value)))) + geom_point() +
                                               +             scale_y_log10(breaks = blabel, labels = blabel) +
                                               +             labs(x = "Rank", y = alabel)
                                               +         ggsave(filename("_rank"), prank, dpi = 100)
                                               +         pbox <- ggplot(historic, aes(factor(year),
                                               +             value)) + geom_boxplot() + labs(x = "",
                                               +             y = alabel)
                                               +         ggsave(filename("_box"), pbox, dpi = 100)
                                               +         ptrend <- ggplot(historic, aes(year, value)) +
                                               +             stat_summary(fun.y = "sum", geom = "line",
                                               +                 colour = "red", size = 1) + stat_summary(data = forecast,
                                               +             aes(y = value, group = variant, colour = variant),
                                               +             fun.y = "sum", geom = "line", size = 1) +
                                               +             labs(y = alabel, x = "") + opts(legend.position = c(0.8,
                                               +             0.3), legend.background = theme_blank(),
                                               +             legend.key = theme_blank()) + scale_colour_hue("Forecast")
                                               +         ggsave(filename("_trend"), ptrend, width = 8,
                                               +             height = 4, dpi = 100)
                                               +         pgrowth <- ggplot(growth, aes(variant,
                                               +             abs_growth, colour = variant)) + geom_boxplot() +
                                               +             xlab("") + opts(legend.position = "none")
                                               +         ggsave(filename("_abs_growth"), pgrowth,
                                               +             dpi = 100)
                                               +         pann_growth <- ggplot(growth, aes(variant,
                                               +             yr5_growth, colour = variant)) + geom_boxplot() +
                                               +             xlab("") + opts(legend.position = "none")
                                               +         ggsave(filename("_ann_growth"), pann_growth,
                                               +             dpi = 100)
                                               +         current <- current[with(current, order(-value)),
                                               +             ]
                                               +         top <- head(current, 5)[, c("country",
                                               +             "value")]
                                               +         bottom <- tail(current, 5)[, c("country",
                                               +             "value")]
                                               +         growth <- growth[growth$variant == "Medium variant",
                                               +             c(2, 4:7)]
                                               +         growth <- growth[order(-growth$abs_growth),
                                               +             ]
                                               +         names(growth)[c(4:5)] <- c("Abs.Growth",
                                               +             "Compound Growth")
                                               +         growthtop <- head(growth, 5)
                                               +         growthbottom <- tail(growth, 5)
                                               +         list(top = top, bottom = bottom, growthtop = growthtop,
                                               +             growthbottom = growthbottom)
                                               +     })