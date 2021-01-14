# by country----

countries<-tibble(Country.g6pd.db=str_split(paste(a, collapse = ";"), pattern = ";")[[1]])

countries %<>% mutate(Country.g6pd.db = case_when(
  Country.g6pd.db == "(India)" ~ "India",
  Country.g6pd.db == "United Kingdom (Great Britain)" ~ "United Kingdom",
  Country.g6pd.db == "Croatia (Hrvatska)" ~ "Croatia",
  Country.g6pd.db != "(India)" &
    Country.g6pd.db != "United Kingdom (Great Britain)" &
    Country.g6pd.db != "Croatia (Hrvatska)" ~ Country.g6pd.db
))

countries <- tibble(Country.g6pd.db = levels(as.factor(countries$Country.g6pd.db)))
countries$Country <- countries$Country.g6pd.db
countries[countries$Country.g6pd.db == "Viet Nam",][["Country"]] <- "Vietnam"

countries_continent<-read.csv("continents-according-to-our-world-in-data.csv")
countries_who <-read.csv("who-regions.csv")
countries_wbank <-read.csv("world-regions-according-to-the-world-bank.csv")
countries_maddison <- read.csv("world-regions-according-to-maddison.csv")

countries <- merge(countries, countries_continent[c(1,3)], by.x = "Country", by.y = "Entity", all.x = T, all.y = F)
countries <- merge(countries, countries_who[c(1,3)], by.x = "Country", by.y = "Entity", all.x = T, all.y = F)
countries <- merge(countries, countries_wbank[c(1,3)], by.x = "Country", by.y = "Entity", all.x = T, all.y = F)
countries <- merge(countries, countries_maddison[c(1,3)], by.x = "Country", by.y = "Entity", all.x = T, all.y = F)

names(countries) <- c("Country", "Country.g6pd.db", "Continent", "Region_WHO", "Region_WBank", "Region_Maddison")

countries[countries$Country=="Czech Republic",][["Continent"]] <- "Europe"
countries[countries$Country=="Czech Republic",][["Region_WHO"]] <- "Europe"
countries[countries$Country=="Czech Republic",][["Region_WBank"]] <- "Europe and Central Asia"
countries[countries$Country=="Czech Republic",][["Region_Maddison"]] <- "Western Europe Maddison definition"

countries[countries$Country=="Taiwan",][["Continent"]] <- "Asia"
countries[countries$Country=="Taiwan",][["Region_WHO"]] <- "Western Pacific"
countries[countries$Country=="Taiwan",][["Region_WBank"]] <- "East Asia and Pacific"
countries[countries$Country=="Taiwan",][["Region_Maddison"]] <- "East Asia Maddison definition"

countries[countries$Country=="Wales",][["Continent"]] <- "Europe"
countries[countries$Country=="Wales",][["Region_WHO"]] <- "Europe"
countries[countries$Country=="Wales",][["Region_WBank"]] <- "Europe and Central Asia"
countries[countries$Country=="Wales",][["Region_Maddison"]] <- "Western Europe Maddison definition"

g6pd.dna.var_df$Country <- as.character(g6pd.dna.var_df$Country)

g6pd.dna.var_df %<>% mutate(Country = case_when(
  is.na(Country) ~ "Undefined",
  !is.na(Country) ~ Country
))

g6pd.dna.var_df$Country <- as.factor(g6pd.dna.var_df$Country)

summary(g6pd.dna.var_df$Country)

g6pd.dna.var_country<-NULL

for (i in countries$Country.g6pd.db){
  temp <- g6pd.dna.var_df %>% filter(grepl(Country, pattern = i)) %>% select(-Country)
  temp <- cbind(temp, countries[countries$Country.g6pd.db == i,])
  g6pd.dna.var_country <- rbind(g6pd.dna.var_country, temp)
  }

g6pd.dna.var_country <- as_tibble(g6pd.dna.var_country)


