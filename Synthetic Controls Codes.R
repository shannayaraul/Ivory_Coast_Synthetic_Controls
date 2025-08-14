install.packages("tidyverse")
install.packages('Synth')
install.packages('devtools')
install_github("bcastanho/SCtools")
install.packages("haven")
install.packages("modelsummary")
install.packages("gt")
install.packages("gtable")
install.packages("gtsummary")
install.packages("ggplot")
install.packages("dagitty")
install.packages("ggdist")
install.packages("knitr")
install.packages("kableExtra")
install.packages("SynthTools")
library(SynthTools)
library(knitr)
library(kableExtra)
library(ggplot2)
library(ggdist)
library(dagitty)
library(ggplot2)
library(dplyr)
library(SCtools)
library(haven)
library(tidyverse)
library(devtools)
library(Synth)
library(modelsummary)
library(gt)
library(gtable)
library(gtsummary)
library(ggplot2)
library(tidyverse)
library(dplyr)


#########Data Cleaning##############
getwd()
Population_Data <- read_csv("Population.csv", skip = 4) #first load the data
Long_data <- Population_Data %>% #then clean it and add to the general data frame
  pivot_longer(
    cols = `1960`:`2023`,
    names_to = "Year",
    values_to = "Population"
  ) %>%
  filter(as.integer(Year) >= 2000)

Long_data <- Long_data %>%
  select(-"Indicator Name", -"Indicator Code", -"Country Code", -"...69")

######

Life_Expectancy <- read_csv("Life_Expectancy.csv", skip = 4) %>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"),   # or use `cols = starts_with("X")` if years are prefixed
    names_to = "Year",
    values_to = "Life_Expct"
  ) %>%
  mutate(Year = as.integer(str_remove(Year, "X"))) %>%
  filter(Year >= 2000)
Life_Expectancy <- Life_Expectancy %>%
  select(-"Indicator Name", -"Indicator Code", -"Country Code", -"...69")

Long_data <- Long_data %>%
  mutate(Year = as.integer(Year))

Long_data <- Long_data %>%
  left_join(Life_Expectancy, by = c("Country Name", "Year"))

#######

Mortality <- read_csv("Mortality.csv", skip = 4) %>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"),   # or use `cols = starts_with("X")` if years are prefixed
    names_to = "Year",
    values_to = "Mortality"
  ) %>%
  mutate(Year = as.integer(str_remove(Year, "X"))) %>%
  filter(Year >= 2000)

Mortality <- Mortality %>%
  select(-"Indicator Name", -"Indicator Code", -"Country Code", -"...69")

Long_data <- Long_data %>%
  left_join(Mortality, by = c("Country Name", "Year"))

###########

Poverty <- read_csv("Poverty.csv", skip = 4) %>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"),   # or use `cols = starts_with("X")` if years are prefixed
    names_to = "Year",
    values_to = "PovertyHCR"
  ) %>%
  mutate(Year = as.integer(str_remove(Year, "X"))) %>%
  filter(Year >= 2000)

Poverty <- Poverty %>%
  select(-"Indicator Name", -"Indicator Code", -"Country Code", -"...69")

Long_data <- Long_data %>%
  left_join(Poverty, by = c("Country Name", "Year"))

#########

Land_km2 <- read_csv("Land Area.csv", skip = 4) %>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"),   # or use `cols = starts_with("X")` if years are prefixed
    names_to = "Year",
    values_to = "Land_km2"
  ) %>%
  mutate(Year = as.integer(str_remove(Year, "X"))) %>%
  filter(Year >= 2000)

Land_km2 <- Land_km2 %>%
  select(-"Indicator Name", -"Indicator Code", -"Country Code", -"...69")

Long_data <- Long_data %>%
  left_join(Land_km2, by = c("Country Name", "Year"))

#############

Education <- read_csv("Education.csv", skip = 4) %>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"),   # or use `cols = starts_with("X")` if years are prefixed
    names_to = "Year",
    values_to = "Primary Education"
  ) %>%
  mutate(Year = as.integer(str_remove(Year, "X"))) %>%
  filter(Year >= 2000)

Education <- Education %>%
  select(-"Indicator Name", -"Indicator Code", -"Country Code", -"...69")

Long_data <- Long_data %>%
  left_join(Education, by = c("Country Name", "Year"))

############

Employment <- read_csv("Employment.csv", skip = 4) %>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"),   # or use `cols = starts_with("X")` if years are prefixed
    names_to = "Year",
    values_to = "Employment Rate"
  ) %>%
  mutate(Year = as.integer(str_remove(Year, "X"))) %>%
  filter(Year >= 2000)

Employment <- Employment %>%
  select(-"Indicator Name", -"Indicator Code", -"Country Code", -"...69")

Long_data <- Long_data %>%
  left_join(Employment, by = c("Country Name", "Year"))
##########

GNI <- read_csv("GNIpc.csv", skip = 4) %>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"),   # or use `cols = starts_with("X")` if years are prefixed
    names_to = "Year",
    values_to = "GNI"
  ) %>%
  mutate(Year = as.integer(str_remove(Year, "X"))) %>%
  filter(Year >= 2000)

GNI <- GNI %>%
  select(-"Indicator Name", -"Indicator Code", -"Country Code", -"...69")

Long_data <- Long_data %>%
  left_join(GNI, by = c("Country Name", "Year"))

#########

HDI <- read_csv("human_development_index.csv") %>%
  select(-"Code")
HDI <- rename(HDI, "Country Name" = "Entity")

Long_data <- Long_data %>%
  left_join(HDI, by = c("Country Name", "Year"))

###########

GDPpc <- read_csv("GDPpc.csv", skip = 4) %>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"),   # or use `cols = starts_with("X")` if years are prefixed
    names_to = "Year",
    values_to = "GDPpc"
  ) %>%
  mutate(Year = as.integer(str_remove(Year, "X"))) %>%
  filter(Year >= 2000)

GDPpc <- GDPpc %>%
  select(-"Indicator Name", -"Indicator Code", -"Country Code", -"...69")

Long_data <- Long_data %>%
  left_join(GDPpc, by = c("Country Name", "Year"))

######

Agriculture <- read_csv("Agriculture.csv", skip = 4) %>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"),   # or use `cols = starts_with("X")` if years are prefixed
    names_to = "Year",
    values_to = "Agriculture+Fishing+Forestry"
  ) %>%
  mutate(Year = as.integer(str_remove(Year, "X"))) %>%
  filter(Year >= 2000)

Agriculture <- Agriculture %>%
  select(-"Indicator Name", -"Indicator Code", -"Country Code", -"...69")

Long_data <- Long_data %>%
  left_join(Agriculture, by = c("Country Name", "Year"))

#########

Exports <- read_csv("Exports.csv", skip = 4) %>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"),   # or use `cols = starts_with("X")` if years are prefixed
    names_to = "Year",
    values_to = "Exports US$"
  ) %>%
  mutate(Year = as.integer(str_remove(Year, "X"))) %>%
  filter(Year >= 2000)

Exports <- Exports %>%
  select(-"Indicator Name", -"Indicator Code", -"Country Code", -"...69")

Long_data <- Long_data %>%
  left_join(Exports, by = c("Country Name", "Year"))

###########

Imports <- read_csv("Imports.csv", skip = 4) %>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"),   # or use `cols = starts_with("X")` if years are prefixed
    names_to = "Year",
    values_to = "Imports US$"
  ) %>%
  mutate(Year = as.integer(str_remove(Year, "X"))) %>%
  filter(Year >= 2000)

Imports <- Imports %>%
  select(-"Indicator Name", -"Indicator Code", -"Country Code", -"...69")

Long_data <- Long_data %>%
  left_join(Imports, by = c("Country Name", "Year"))

#########

Services <- read_csv("Services.csv", skip = 4) %>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"),   # or use `cols = starts_with("X")` if years are prefixed
    names_to = "Year",
    values_to = "Services US$"
  ) %>%
  mutate(Year = as.integer(str_remove(Year, "X"))) %>%
  filter(Year >= 2000)

Services <- Services %>%
  select(-"Indicator Name", -"Indicator Code", -"Country Code", -"...69")

Long_data <- Long_data %>%
  left_join(Services, by = c("Country Name", "Year"))

############

Industry <- read_csv("Industry.csv", skip = 4) %>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"),   # or use `cols = starts_with("X")` if years are prefixed
    names_to = "Year",
    values_to = "Industry US$"
  ) %>%
  mutate(Year = as.integer(str_remove(Year, "X"))) %>%
  filter(Year >= 2000)

Industry <- Industry %>%
  select(-"Indicator Name", -"Indicator Code", -"Country Code", -"...69")

Long_data <- Long_data %>%
  left_join(Industry, by = c("Country Name", "Year"))

###########

GDP <- read_csv("GDP.csv", skip = 4) %>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"),   # or use `cols = starts_with("X")` if years are prefixed
    names_to = "Year",
    values_to = "GDP US$"
  ) %>%
  mutate(Year = as.integer(str_remove(Year, "X"))) %>%
  filter(Year >= 2000)

GDP <- GDP %>%
  select(-"Indicator Name", -"Indicator Code", -"Country Code", -"...69")

Long_data <- Long_data %>%
  left_join(GDP, by = c("Country Name", "Year"))

#########
GDI <- read_csv("GDI_Final.csv")
Long_data <- Long_data %>%
  left_join(GDI, by = c("Country Name", "Year"))




#########CLEANING UNECCESSARY ROWS#########

Long_data <- Long_data %>%
  filter(!`Country Name` %in% c("Arab World", "Africa Western and Central", "Africa Eastern and Southern"))

######### Saving the general data
install.packages("openxlsx")
library(openxlsx)
write.xlsx(Long_data, file = "General Data.csv", rowNames = FALSE)


######RANKING DATA#######

Long_data$Rank <- seq_along(Long_data$"Country Name")

######Analysing rates of HDI in IC overtime

#Adding a binary treatment indicator for Côte d'Ivoire
Long_data <- Long_data %>%
  mutate(
    Cote_dIvoire = if_else(`Country Name` %in% c("Cote d'Ivoire"), 1, 0)
  )

#Inspect HDI range to confirm it's normalized (optional)
summary(Long_data$`Human Development Index`)

# Step 4: Optional — visualize HDI over time for Côte d'Ivoire vs others- OPTION 1
Long_data %>%
  filter(`Country Name` %in% c("Cote d'Ivoire", "Ghana", "Senegal")) %>%
  ggplot(aes(x = Year, y = `Human Development Index`, color = `Country Name`)) +
  geom_line(size = 1.2) +
  labs(title = "HDI Over Time", y = "HDI", x = "Year") +
  theme_minimal()

#####OPTION 2- I LIKE THIS ONE BETTER

ggplot(Long_data, 
       aes(x = Year, y = `Human Development Index`, 
           shape = factor(Cote_dIvoire, labels = 
                            c("Other Countries", "Cote d'Ivoire")))) +
  stat_summary(fun.y = "mean", geom = "point") +
  stat_summary(fun.y = "mean", geom = "line")  +
  labs(x = "Year", y = "Human Development Index", shape = "") +
  theme_bw() + theme(legend.position = "bottom")


######################SYNTHETIC CONTROLSSSS###########################
Synthdata <- Long_data%>%
  filter(!is.na("Human Development Index")) %>%
  mutate(country_code = 
           as.numeric(factor(Rank, levels = unique(Rank))))

##### convert long data as data frame
Long_data <- as.data.frame(Long_data)
Long_data <- Long_data %>%
  mutate(country_code = dense_rank(`Country Name`))

######Setting our initial year of interest 2000
Data2000 <- Long_data %>%
  filter(Year == 2000, !is.na(`Human Development Index`)) %>%
  select(country_code, `Human Development Index`) %>%
  rename(HDI_2000 = `Human Development Index`)

Long_data <- left_join(x = Long_data, y = Data2000, 
                          by = c('country_code')) 

print(Data2000)


print(Long_data)

Long_data <- Long_data[, -ncol(Long_data)]
    #### Divide the current value of HDI to the one in 2000
Long_data <- Long_data %>%
  mutate(HDI_idx = `Human Development Index` / HDI_2000)
#######Removing Ivory Coast from the dataste only
Long_data %>%
  select(`Country Name`, country_code) %>%
  distinct() %>%
  arrange(country_code) %>%
  print(n = Inf)

Backup_data <- Long_data %>% filter(Year != 2000) %>%
  filter(country_code != 50)

###NOW WE NEED THE FINAL DONOR POOL
### Filtering the Long_data so that I only have CDI and 
### Plausible countries for my donor pool. They should be countries that:
### A) DID NOT IMPLEMENT GOLD MINING POLICIES between 2000 and 2022.
### or
### B) Have similar economic potential as CDI but gold

# List of countries to include
Countries_to_include <- c("Angola", "Cote d'Ivoire", "Togo", "Mozambique", "Iraq", "Kuwait", "Libya", "Oman", "Djibouti", "Malaysia", "Myanmar", "Vietnam")

# Filtering the dataset to include only the specified countries
Filtered_data <- Long_data %>%
  filter(`Country Name` %in% Countries_to_include)

List_countries <- unique(Filtered_data$country_code)

Filtered_data %>%
  select(`Country Name`, country_code) %>%
  distinct() %>%
  arrange(country_code)



List_countries <- List_countries[-2] ### Check what the country_code for CDI is after filtering the data above, TAKE IT OUT OF THE LIST COUNTRIES.

#Preparing data as an input for the synthetic control method using the dataprep function:
Filtered_data %>%
  filter(country_code == 50)

Filtered_data <- Filtered_data %>%
  mutate(
    country_code = as.integer(country_code),  # force to integer
    Year = as.integer(Year)                   # also clean up Year to be safe
  ) 

List_countries <- List_countries[List_countries != 50]
print(List_countries)

Filtered_data <- Filtered_data %>%
  mutate(country_code = as.numeric(country_code))  # Ensures it's numeric

str(Filtered_data)

##Preparing my Donor Pool


glimpse(Filtered_data)
table(is.na(Filtered_data$HDI_idx))
table(is.na(Filtered_data$country_code))
unique(Filtered_data$country_code)
class(Filtered_data$country_code)
class(Filtered_data$Year)

Filtered_data <- as.data.frame(Filtered_data)

#####REMOVING GUINEA AND GREECE BECAUSE THEY DO NOT HAVE DATA IN 2000####NO NEED FOR THIS ANYMORE

List_countries <- List_countries[List_countries != 89]
List_countries <- List_countries[List_countries != 95]
Filtered_data <- Filtered_data %>%
  filter(country_code != 95)
Filtered_data <- Filtered_data %>%
  filter(country_code != 89)
print(89 %in% List_countries)  # should return FALSE
print(95 %in% Filtered_data)  # should return FALSE


Filtered_data %>%
  filter(country_code == 95) %>%
  select(`Country Name`) %>%
  distinct()

dataprep.out <- dataprep( 
  foo = Filtered_data,
  dependent = "HDI_idx", 
  time.variable = "Year", 
  unit.variable = "country_code",
  special.predictors = list( 
    list("HDI_idx", 2001, "mean"),
    list("HDI_idx", 2002, "mean"),
    list("HDI_idx", 2003, "mean"),
    list("HDI_idx", 2004, "mean"),
    list("HDI_idx", 2005, "mean"),
    list("HDI_idx", 2006, "mean"),
    list("HDI_idx", 2007, "mean"),
    list("HDI_idx", 2008, "mean"),
    list("HDI_idx", 2009, "mean"),
    list("HDI_idx", 2010, "mean"),
    list("HDI_idx", 2011, "mean"),
    list("HDI_idx", 2012, "mean"),
    list("HDI_idx", 2013, "mean"),
    list("HDI_idx", 2014, "mean"),
    list("HDI_idx", 2015, "mean"),
    list("HDI_idx", 2016, "mean"),
    list("HDI_idx", 2017, "mean"),
    list("HDI_idx", 2018, "mean"),
    list("HDI_idx", 2019, "mean"),
    list("HDI_idx", 2020, "mean"),
    list("HDI_idx", 2021, "mean"),
    list("HDI_idx", 2022, "mean")
  ),
  treatment.identifier = 50,  # Côte d'Ivoire
  controls.identifier = List_countries, 
  time.predictors.prior = c(2001:2022), 
  time.optimize.ssr = c(2001:2022), 
  unit.names.variable = "Country Name",
  time.plot = 2001:2022
)

valid_years <- Filtered_data %>%
  filter(country_code %in% List_countries) %>%
  group_by(Year) %>%
  summarise(var_HDI_idx = var(HDI_idx, na.rm = TRUE)) %>%
  filter(!is.na(var_HDI_idx), var_HDI_idx > 0) %>%
  pull(Year)
print(valid_years)

#Passing the output of dataprep as an input for the synth function
synth.out <- synth(dataprep.out)
synth.out
#Producing the table of results with the synth.tab function and print it
synth.table <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out)
print(synth.table)

## Producing the synthetic control plot and the gaps plot with the path.plot and the
#gaps.plot functions. 

#path.plot
path.plot(dataprep.res = dataprep.out,synth.res = synth.out)


## gaps.plot
gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out)


#Producing the gaps plot with the placebo estimates for other cities using the
#generate.placebos and plot_placebos functions.
Placebo <- generate.placebos(dataprep.out, synth.out, Sigf.ipop = 2)

## Comparing differences with synthetic versions: Liberia vs. placebos
P1 <- plot_placebos(Placebo,discard.extreme=TRUE, 
                    mspe.limit=10, xlab='Year')
plot(P1)

# Manually setting the placebo year
Placebo$placebo.year <- 2020  

## Comparing differences with synthetic versions: Liberia vs. placebos
P1 <- plot_placebos(Placebo, discard.extreme = TRUE, mspe.limit = 10, xlab = 'Year')
plot(P1)


#Testing and plotting the change in predictability before and after the treatment for CDI and for other countries using the mspe.test and mspe.plot functions.


#####struggling here######
Ratio <- mspe.test(Placebo)

Ratio$p.val

mspe.plot(Placebo, discard.extreme = FALSE)


unique(gap_df$Country)


placebo_results <- list()

for (c in unique(Filtered_data$country_code)) {
  if (c == 50) next  # Skip CDI
  
  tryCatch({
    dp <- dataprep(
      foo = Filtered_data,
      dependent = "HDI_idx",
      time.variable = "Year",
      unit.variable = "country_code",
      special.predictors = lapply(2001:2022, function(y) list("HDI_idx", y, "mean")),
      treatment.identifier = c,
      controls.identifier = setdiff(unique(Filtered_data$country_code), c),
      time.predictors.prior = 2001:2022,
      time.optimize.ssr = 2001:2022,
      unit.names.variable = "Country Name",
      time.plot = 2001:2022
    )
    
    synth_out <- synth(dp)
    
    gap <- as.numeric(dp$Y1plot - (dp$Y0plot %*% synth_out$solution.w))
    
    placebo_results[[as.character(c)]] <- data.frame(
      Year = dp$tag$time.plot,
      Gap = gap,
      Country = paste0("Placebo_", c)
    )
  }, error = function(e) {
    message("Failed for country code ", c, ": ", e$message)
  })
}


gap_df <- bind_rows(placebo_results, treated_df)
# Then re-run the MSPE calculation and plot as before

# Define treatment year
treatment_year <- 2011

# gap_df must have: Country, Year, Gap
mspe_df <- gap_df %>%
  mutate(period = ifelse(Year < treatment_year, "pre", "post")) %>%
  group_by(Country, period) %>%
  summarise(mspe = mean(Gap^2, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = mspe) %>%
  mutate(ratio = post / pre)


ggplot(mspe_df, aes(x = reorder(Country, -ratio), y = ratio,
                    fill = Country == "Cote d'Ivoire")) +
  geom_bar(stat = "identity", na.rm = TRUE) +
  scale_fill_manual(values = c("gray", "darkred")) +
  labs(title = "Post/Pre-Treatment MSPE Ratios",
       x = "Country", y = "MSPE Ratio",
       fill = "Treated Unit") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom")

##############################ALTERNATIVE PLACEBO#######
library(Synth)

# Create a list to store results
placebo_results <- list()

# Loop through each donor country (excluding treated unit)
for (c in unique(Filtered_data$country_code)) {
  if (c == 50) next  # Skip Côte d'Ivoire
  
  try({
    dp <- dataprep(
      foo = Filtered_data,
      dependent = "HDI_idx",
      time.variable = "Year",
      unit.variable = "country_code",
      special.predictors = lapply(valid_years, function(y) list("HDI_idx", y, "mean")),
      treatment.identifier = c,
      controls.identifier = setdiff(unique(Filtered_data$country_code), c),
      time.predictors.prior = valid_years,
      time.optimize.ssr = valid_years,
      unit.names.variable = "Country Name",
      time.plot = 2000:2022
    )
    
    synth_out <- synth(dp)
    
    gap <- as.numeric(dp$Y1plot - (dp$Y0plot %*% synth_out$solution.w))
    
    placebo_results[[as.character(c)]] <- data.frame(
      Year = dp$tag$time.plot,
      Gap = gap,
      Country = paste0("Placebo_", c)
    )
  }, silent = TRUE)
}


##################

# Original Cote d'Ivoire model
dp_civ <- dataprep.out
synth_civ <- synth.out

treated_gap <- as.numeric(dp_civ$Y1plot - (dp_civ$Y0plot %*% synth_civ$solution.w))

treated_df <- data.frame(
  Year = dp_civ$tag$time.plot,
  Gap = treated_gap,
  Country = "Cote d'Ivoire"
)


gap_df <- bind_rows(placebo_results, treated_df)


ggplot(gap_df, aes(x = Year, y = Gap, group = Country)) +
  geom_line(alpha = 0.4, color = "gray") +
  geom_line(data = filter(gap_df, Country == "Cote d'Ivoire"),
            aes(color = "Cote d'Ivoire"), size = 1.2) +
  geom_vline(xintercept = 2011, linetype = "dashed", color = "red") +
  annotate("text", x = 2011.5, y = 0, label = "Treatment", angle = 90, vjust = -0.5) +
  labs(title = "Synthetic Control Placebo Test",
       y = "Gap: Treated - Synthetic",
       x = "Year", color = "") +
  theme_minimal() +
  theme(legend.position = "bottom")


###Summary Statistics####

Filtered_data$GDI <- as.numeric(unlist(Filtered_data$GDI))
library(purrr)
Filtered_data$GDI <- map_dbl(Filtered_data$GDI, ~ as.numeric(.x))


summary_stats <- Filtered_data %>%
  mutate(
    GDI = as.numeric(GDI),  # convert to numeric just in case
    `Employment Rate` = as.numeric(`Employment Rate`)
  ) %>%
  group_by(Year) %>%
  summarise(
    Population_mean = mean(Population, na.rm = TRUE),
    Population_sd = sd(Population, na.rm = TRUE),
    GNI_mean = mean(GNI, na.rm = TRUE),
    GNI_sd = sd(GNI, na.rm = TRUE),
    GDP_mean = mean(`GDP US$`, na.rm = TRUE),
    GDP_sd = sd(`GDP US$`, na.rm = TRUE),
    Exports_mean = mean(`Exports US$`, na.rm = TRUE),
    Exports_sd = sd(`Exports US$`, na.rm = TRUE),
    Industry_mean = mean(`Industry US$`, na.rm = TRUE),
    Industry_sd = sd(`Industry US$`, na.rm = TRUE),
    Agriculture_mean = mean(`Agriculture+Fishing+Forestry`, na.rm = TRUE),
    Agriculture_sd = sd(`Agriculture+Fishing+Forestry`, na.rm = TRUE),
    GDI_mean = mean(GDI, na.rm = TRUE),
    GDI_sd = sd(GDI, na.rm = TRUE),
    Imports_mean = mean(`Imports US$`, na.rm = TRUE),
    Imports_sd = sd(`Imports US$`, na.rm = TRUE),
    Life_Expct_mean = mean(Life_Expct, na.rm = TRUE),
    Life_Expct_sd = sd(Life_Expct, na.rm = TRUE),
    Land_km2_mean = mean(Land_km2, na.rm = TRUE),
    Land_km2_sd = sd(Land_km2, na.rm = TRUE),
    Services_mean = mean(`Services US$`, na.rm = TRUE),
    Services_sd = sd(`Services US$`, na.rm = TRUE),
    Mortality_mean = mean(Mortality, na.rm = TRUE),
    Mortality_sd = sd(Mortality, na.rm = TRUE),
    PovertyHCR_mean = mean(PovertyHCR, na.rm = TRUE),
    PovertyHCR_sd = sd(PovertyHCR, na.rm = TRUE),
    PrimaryEd_mean = mean(`Primary Education`, na.rm = TRUE),
    PrimaryEd_sd = sd(`Primary Education`, na.rm = TRUE),
    Employ_mean = mean(`Employment Rate`, na.rm = TRUE),
    Employ_sd = sd(`Employment Rate`, na.rm = TRUE),
    GDPpc_mean = mean(GDPpc, na.rm = TRUE),
    GDPpc_sd = sd(GDPpc, na.rm = TRUE),
    HDI_mean = mean(`Human Development Index`, na.rm = TRUE),
    HDI_sd = sd(`Human Development Index`, na.rm = TRUE),
    HDI_idx_mean = mean(HDI_idx, na.rm = TRUE),
    HDI_idx_sd = sd(HDI_idx, na.rm = TRUE)
  )


# View summary statistics
print(summary_stats)
write.csv(summary_stats, "General_summary_statistics.csv", row.names = FALSE)

#Summary Statistics for Liberia

CDI_Sum <- Filtered_data %>%
  filter(`Country Name` == "Cote d'Ivoire")

CDI_Sum <- CDI_Sum %>%
  group_by(Year) %>%
  summarise(
    Population_mean = mean(Population, na.rm = TRUE),
    Population_sd = sd(Population, na.rm = TRUE),
    GNI_mean = mean(GNI, na.rm = TRUE),
    GNI_sd = sd(GNI, na.rm = TRUE),
    GDP_mean = mean(`GDP US$`, na.rm = TRUE),
    GDP_sd = sd(`GDP US$`, na.rm = TRUE),
    Exports_mean = mean(`Exports US$`, na.rm = TRUE),
    Exports_sd = sd(`Exports US$`, na.rm = TRUE),
    Industry_mean = mean(`Industry US$`, na.rm = TRUE),
    Industry_sd = sd(`Industry US$`, na.rm = TRUE),
    Agriculture_mean = mean(`Agriculture+Fishing+Forestry`, na.rm = TRUE),
    Agriculture_sd = sd(`Agriculture+Fishing+Forestry`, na.rm = TRUE),
    GDI_mean = mean(GDI, na.rm = TRUE),
    GDI_sd = sd(GDI, na.rm = TRUE),
    Imports_mean = mean(`Imports US$`, na.rm = TRUE),
    Imports_sd = sd(`Imports US$`, na.rm = TRUE),
    Life_Expct_mean = mean(Life_Expct, na.rm = TRUE),
    Life_Expct_sd = sd(Life_Expct, na.rm = TRUE),
    Land_km2_mean = mean(Land_km2, na.rm = TRUE),
    Land_km2_sd = sd(Land_km2, na.rm = TRUE),
    Services_mean = mean(`Services US$`, na.rm = TRUE),
    Services_sd = sd(`Services US$`, na.rm = TRUE),
    Mortality_mean = mean(Mortality, na.rm = TRUE),
    Mortality_sd = sd(Mortality, na.rm = TRUE),
    PovertyHCR_mean = mean(PovertyHCR, na.rm = TRUE),
    PovertyHCR_sd = sd(PovertyHCR, na.rm = TRUE),
    PrimaryEd_mean = mean(`Primary Education`, na.rm = TRUE),
    PrimaryEd_sd = sd(`Primary Education`, na.rm = TRUE),
    Employ_mean = mean(`Employment Rate`, na.rm = TRUE),
    Employ_sd = sd(`Employment Rate`, na.rm = TRUE),
    GDPpc_mean = mean(GDPpc, na.rm = TRUE),
    GDPpc_sd = sd(GDPpc, na.rm = TRUE),
    HDI_mean = mean(`Human Development Index`, na.rm = TRUE),
    HDI_sd = sd(`Human Development Index`, na.rm = TRUE),
    HDI_idx_mean = mean(HDI_idx, na.rm = TRUE),
    HDI_idx_sd = sd(HDI_idx, na.rm = TRUE)
  )

print(CDI_Sum)


# Convert summary_stats to a formatted table using kable
# Save summary statistics as a CSV file
write.csv(CDI_Sum, "summary_statistics_CDI.csv", row.names = FALSE)

library(dplyr)
library(ggplot2)

# Filter for Côte d'Ivoire and select relevant columns
Long_data %>%
  filter(`Country Name` == "Cote d'Ivoire") %>%
  ggplot(aes(x = Year, y = `GDP US$`)) +
  geom_line(color = "darkred", size = 1.2) +
  geom_point(color = "darkred") +
  labs(
    title = "GDP Over Time for Côte d'Ivoire",
    x = "Year",
    y = "GDP (US$)"
  ) +
  theme_minimal()

library(dplyr)
library(ggplot2)

# Filter for Côte d'Ivoire and select relevant columns
Long_data %>%
  filter(`Country Name` == "Cote d'Ivoire") %>%
  ggplot(aes(x = Year, y = `GDP US$`)) +
  geom_line(color = "darkred", size = 1.2) +
  geom_point(color = "darkred") +
  labs(
    title = "GDP Over Time for Côte d'Ivoire",
    x = "Year",
    y = "GDP (US$)"
  ) +
  theme_minimal()

library(dplyr)
library(ggplot2)

# Filter for Côte d'Ivoire and select relevant columns
Long_data %>%
  filter(`Country Name` == "Cote d'Ivoire") %>%
  ggplot(aes(x = Year, y = `GDP US$`)) +
  geom_line(color = "darkblue", size = 1.2) +
  geom_point(color = "darkblue") +
  labs(
    title = "GDP Over Time for Côte d'Ivoire",
    x = "Year",
    y = "GDP (US$)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14)  # 👈 bolds the title
  )


Long_data %>%
  filter(`Country Name` == "Cote d'Ivoire") %>%
  ggplot(aes(x = Year, y = `Human Development Index`)) +
  geom_line(color = "darkred", size = 1.2) +
  geom_point(color = "darkred") +
  labs(
    title = "HDI Over Time for Côte d'Ivoire",
    x = "Year",
    y = "GDP (US$)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14)  # 👈 bolds the title
  )

