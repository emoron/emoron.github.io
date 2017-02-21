library(ggplot2)
library(ISOcodes)
file <- c("UNdata_Population.csv")
population <- read.csv(file)
names(population) <- c("code", "country", "year",
                         +     "variant", "value")
df <- subset(population, year <= 2005)
data("UN_M.49_Countries")
data("UN_M.49_Regions")
Regions <- subset(UN_M.49_Regions, Type == "Region")
regionsdf <- ddply(Regions, .(Code, Name, Parent),

function(x) {
         df <- data.frame(strsplit(x$Children,
             ", "))
         names(df) <- "countrycode"
         df
     })
 countries <- merge(regionsdf, UN_M.49_Countries,
     by.x = "countrycode", by.y = "Code")
 countries <- rename(countries, c(Name.x = "region",
     Name.y = "country", Code = "regioncode", Parent = "parentcode"))
 countries <- merge(countries, Regions[, 1:2],
     by.x = "parentcode", by.y = "Code")
 countries <- rename(countries, c(Name = "continent"))
 countries$countrycode <- as.numeric(as.character(countries$countrycode))

 population <- merge(population, countries[, c("countrycode",
                                               +     "continent")], by.x = "code", by.y = "countrycode")
 population$value <- population$value/1000