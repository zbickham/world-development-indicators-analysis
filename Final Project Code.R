library(tidyverse)
library(janitor)
library(dplyr)
library(cluster)
library(ggplot2)
library(ggmap)


# Data Cleaning -----------------------------------------------------------

wdi_data <- as_tibble(SDS_322E_Final_Project_Data)
wdi_regions <- as_tibble(SDS_322E_Final_Project_Region_Data)

wdi_data <- wdi_data %>% 
  rename(
    time = `Time`,
    country = `Country Name`,
    country_code = `Country Code`,
    population = `Population, total [SP.POP.TOTL]`,
    gdp_per_capita = `GDP per capita, PPP (current international $) [NY.GDP.PCAP.PP.CD]`,
    gni_per_capita = `GNI per capita, PPP (current international $) [NY.GNP.PCAP.PP.CD]`,
    death_rate = `Death rate, crude (per 1,000 people) [SP.DYN.CDRT.IN]`,
    birth_rate = `Birth rate, crude (per 1,000 people) [SP.DYN.CBRT.IN]`,
    fertility_rate = `Fertility rate, total (births per woman) [SP.DYN.TFRT.IN]`,
    life_expectancy = `Life expectancy at birth, total (years) [SP.DYN.LE00.IN]`,
    water = `People using at least basic drinking water services (% of population) [SH.H2O.BASW.ZS]`,
    sanitation = `People using at least basic sanitation services (% of population) [SH.STA.BASS.ZS]`,
    literacy_rate = `Literacy rate, adult total (% of people ages 15 and above) [SE.ADT.LITR.ZS]`,
    poverty_gap = `Poverty gap at $2.15 a day (2017 PPP) (%) [SI.POV.GAPS]`,
    poverty_national = `Poverty headcount ratio at national poverty lines (% of population) [SI.POV.NAHC]`,
    electricity = `Access to electricity (% of population) [EG.ELC.ACCS.ZS]`,
    co2 = `CO2 emissions (metric tons per capita) [EN.ATM.CO2E.PC]`,
    gender_equality = `CPIA gender equality rating (1=low to 6=high) [IQ.CPA.GNDR.XQ]`,
    undernourishment = `Prevalence of undernourishment (% of population) [SN.ITK.DEFC.ZS]`,
    mortality_rate = `Mortality rate, infant (per 1,000 live births) [SP.DYN.IMRT.IN]`
  ) %>%
  filter(!is.na(country)) %>%
  mutate(fertility_rate = as.numeric(fertility_rate),
         population = as.numeric(population))

wdi_regions <- wdi_regions %>% 
  rename(
    time = `Time`,
    country = `Country Name`,
    country_code = `Country Code`,
    population = `Population, total [SP.POP.TOTL]`,
    gdp_per_capita = `GDP per capita, PPP (current international $) [NY.GDP.PCAP.PP.CD]`,
    gni_per_capita = `GNI per capita, PPP (current international $) [NY.GNP.PCAP.PP.CD]`,
    death_rate = `Death rate, crude (per 1,000 people) [SP.DYN.CDRT.IN]`,
    birth_rate = `Birth rate, crude (per 1,000 people) [SP.DYN.CBRT.IN]`,
    fertility_rate = `Fertility rate, total (births per woman) [SP.DYN.TFRT.IN]`,
    life_expectancy = `Life expectancy at birth, total (years) [SP.DYN.LE00.IN]`,
    water = `People using at least basic drinking water services (% of population) [SH.H2O.BASW.ZS]`,
    sanitation = `People using at least basic sanitation services (% of population) [SH.STA.BASS.ZS]`,
    literacy_rate = `Literacy rate, adult total (% of people ages 15 and above) [SE.ADT.LITR.ZS]`,
    poverty_gap = `Poverty gap at $2.15 a day (2017 PPP) (%) [SI.POV.GAPS]`,
    poverty_national = `Poverty headcount ratio at national poverty lines (% of population) [SI.POV.NAHC]`,
    electricity = `Access to electricity (% of population) [EG.ELC.ACCS.ZS]`,
    co2 = `CO2 emissions (metric tons per capita) [EN.ATM.CO2E.PC]`,
    gender_equality = `CPIA gender equality rating (1=low to 6=high) [IQ.CPA.GNDR.XQ]`,
    undernourishment = `Prevalence of undernourishment (% of population) [SN.ITK.DEFC.ZS]`,
    mortality_rate = `Mortality rate, infant (per 1,000 live births) [SP.DYN.IMRT.IN]`
  ) %>%
  filter(!is.na(country)) %>%
  mutate(death_rate = as.numeric(death_rate),
         birth_rate = as.numeric(birth_rate), 
         fertility_rate = as.numeric(fertility_rate),
         life_expectancy = as.numeric(life_expectancy))


# Correlation Heatmap -----------------------------------------------------

# Create wdi correlation matrix
wdi_data_cor <- wdi_data %>%
  as.data.frame() %>%
  filter(time == 2018) %>%
  select(-c(time, country_code, population)) %>%
  remove_rownames %>%
  column_to_rownames(var="country")

cormat <- cor(wdi_data_cor, use="pairwise.complete.obs")

# Tidy the data
tidycor <- cormat %>%
  as.data.frame %>%
  rownames_to_column("var1") %>%
  pivot_longer(-1, names_to = "var2", values_to = "correlation")

# Create a correlation heatmap of the wdi data in 2018
tidycor %>%
  ggplot(aes(var1, var2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue") +
  geom_text(aes(label = round(correlation, 2)),
            color = "black", size = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Correlation Heatmap of WDI in 2018",
       x = "variables",
       y = "variables") + 
  coord_fixed() 


# PAM ---------------------------------------------------------------------

# function to calculate number of NAs for a
# variable
count_missing <- function(x) {
  sum(is.na(x))
}

# function to calculate proportion of NAs for a
# variable
compute_proportion <- function(x) {
  mean(is.na(x))
}

# apply NA functions to every column in `wdi_data`
wdi_na_2018 <- wdi_data %>%
  filter(time == 2018) %>%
  summarize_all(count_missing)

wdi_data_pam <- wdi_data %>%
  filter(time == 2018) %>%
  mutate_at(c("population",  "gdp_per_capita", "gni_per_capita",
              "death_rate", "life_expectancy", "water",
              "sanitation", "electricity", "co2", "mortality_rate"), log10) %>%
  select('country', "population",  "gdp_per_capita",
         "gni_per_capita", "death_rate", "birth_rate",
         "fertility_rate", "life_expectancy", "water",
         "sanitation", "electricity", "co2", "mortality_rate") %>%
  column_to_rownames('country') 

wdi_data_pam <- na.omit(wdi_data_pam)
wdi_data_pam[] <- scale(wdi_data_pam)
wdi_data_pam

# Empty vector to hold mean sil width
sil_width <- vector()

for (i in 2:10) {
  # Compute PAM solution
  pam_sol <- pam(wdi_data_pam, k = i)
  # Get sil widths
  sil <- silhouette(pam_sol$cluster, dist(wdi_data_pam))
  # Take averages (higher is better)
  sil_width[i] <- mean(sil[, 3])
}

# Plot k values to find highest
ggplot() + geom_line(aes(x = 1:10, y = sil_width)) +
  scale_x_continuous(name = "k", breaks = 1:10)

# Compute PAM with k = 2
wdi_pam <- pam(wdi_data_pam, k = 2)

# Create silhouette plot
plot(wdi_pam, which = 2)

# Find medoids
wdi_pam["medoids"]

library(GGally)
# Plot pairwise clusters
wdi_data_pam %>%
  mutate(cluster = as.factor(wdi_pam$clustering)) %>%
  ggpairs(columns = c("population",  "gdp_per_capita", "gni_per_capita",
                      "death_rate", "birth_rate", "fertility_rate",
                      "life_expectancy", "water", "sanitation",
                      "electricity", "co2", "mortality_rate"),
          aes(color = cluster))


# World Map ---------------------------------------------------------------

# Geographic coordinates about countries in the world
mapWorld <- map_data("world")

# check if information from `wdi_data` is not contained in `mapWorld`
wdi_data %>% 
  filter(time == 2018) %>%
  anti_join(mapWorld, by = c("country" = "region")) %>%
  select(country) %>%
  print(n = 100)

mapWorld %>%
  distinct(region) %>%
  arrange(region)

# join data from `mapWorld` to `wdi_data`
mymap <- wdi_data %>%
  filter(time == 2018) %>%
  mutate(country = recode(country,
                          "Bahamas, The" = "Bahamas",
                          "Brunei Darussalam" = "Brunei",
                          "Cabo Verde" = "Cape Verde",
                          "Congo, Dem. Rep." = "Democratic Republic of the Congo",
                          "Congo, Rep." = "Republic of Congo",
                          "Cote d'Ivoire" = "Ivory Coast",
                          "Czechia" = "Czech Republic",
                          "Egypt, Arab Rep." = "Egypt",
                          "Eswatini" = "Swaziland",
                          "Gambia, The" = "Gambia",
                          "Iran, Islamic Rep." = "Iran",
                          "Korea, Dem. People's Rep." = "North Korea",
                          "Korea, Rep." = "South Korea",
                          "Kyrgyz Republic" = "Kyrgyzstan",
                          "Lao PDR" = "Laos",
                          "Micronesia, Fed. Sts." = "Micronesia",
                          "Russian Federation" = "Russia",
                          "Sint Maarten (Dutch part)" = "Sint Maarten",
                          "Slovak Republic" = "Slovakia",
                          "St. Lucia" = "Saint Lucia",
                          "St. Martin (French part)" = "Saint Martin",
                          "Syrian Arab Republic" = "Syria",
                          "Turkiye" = "Turkey",
                          "United Kingdom" = "UK",
                          "United States" = "USA",
                          "Venezuela, RB" = "Venezuela",
                          "Viet Nam" = "Vietnam",
                          "West Bank and Gaza" = "Palestine",
                          "Yemen, Rep." = "Yemen")) %>%
  left_join(mapWorld, by = c("country" = "region"))

# Paste and run the following into your console (NOT HERE): install.packages("ggmap")

# Build a map!
mymap %>%
  # create a grid with longitude on the x-axis and latitude on the y-axis that is filled by one of the wdi
  ggplot(aes(x = long, y = lat, group = group, fill = life_expectancy)) +
  # insert a map of the world with a black outline
  geom_polygon(colour = "black") +
  # change the wdi color gradient
  scale_fill_gradient(low = "white", high = "blue") +
  # change the graph labels
  labs(fill = "Life Expectancy" ,title = "Life Expectancy in 2018", 
       x ="Longitude", y ="Latitude") +
  coord_fixed()


# Graphs ------------------------------------------------------------------

wdi_data %>%
  ggplot(aes(x = gdp_per_capita, y = life_expectancy)) +
  geom_point() +
  geom_smooth(method = "loess",
              formula = y ~ x)

# create a bar graph of gender equality per country in 2018
wdi_data %>%
  filter(time == 2018) %>%
  drop_na(gender_equality) %>%
  ggplot(aes(x = reorder(country_code, gender_equality), y = gender_equality)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Country") +
  ylab("Gender Equality")

wdi_data %>%
  group_by(time) %>%
  drop_na(co2) %>%
  summarise(mean_co2 = mean(co2)) %>%
  ggplot(aes(x = time, y = mean_co2)) +
  geom_line() +
  xlab("Year") +
  ylab("Average CO2 Emissions (metric tons per capita)")

wdi_data %>%
  filter(time == 2022) %>%
  ggplot(aes(gdp_per_capita)) +
  geom_histogram()

wdi_regions %>%
  filter(country != "World") %>%
  # Color by region
  ggplot(aes(x = sanitation, y = water, color = country)) +
  geom_point() +
  # Add a regression trend line
  geom_smooth(method = "lm") +
  # Add labels
  xlab("People Using at Least Basic Sanitation Services (% of population)") +
  ylab("People Using at Least Basic Drinking Water Services (% of population)") +
  ggtitle("Access to Drinking Water vs Sanitation")

wdi_regions %>%
  filter(country != "World") %>%
  # Color by region
  ggplot(aes(x = time, y = population, color = country)) +
  geom_point() +
  # Add a regression trend line
  geom_smooth(method = "lm") +
  # Add labels
  xlab("Year") +
  ylab("Population") +
  labs(color = "Region") +
  ggtitle("Population Over Time")

wdi_regions %>%
  filter(country != "World") %>%
  # Color by region
  ggplot(aes(x = undernourishment, y = death_rate, color = country)) +
  geom_point() +
  geom_smooth(method = "lm")

wdi_regions %>%
  filter(country != "World") %>%
  drop_na(poverty_gap) %>%
  ggplot(aes(x = time, y = poverty_gap)) +
  geom_line() +
  facet_wrap(~country)
