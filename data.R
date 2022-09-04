library(tidyverse)
library(ggplot2)
library(data.table)

# Save toxic release data
data = toxics_release_inventory_data %>% filter(CARCINOGEN == "YES")

# Graph of toxic release per year
data %>% group_by(YEAR) %>%
  mutate(total = sum(ON.SITE_RELEASE_TOTAL)) %>%
  ggplot(aes(YEAR, total)) +
  geom_point()

head(NDS2008)

muertesCancer = NDS2008 %>% filter(cod_icd10 %like% "^C" & countryresidence == 1)

muertesCancer %>% group_by(monthdeath) %>%
  summarize(dailyD = n()) %>%
  ggplot(aes(monthdeath, dailyD)) +
  geom_point()

# Toxic releases in interested areas
data %>% filter(CARCINOGEN == "YES") %>%
  filter(COUNTY %in% c("SALINAS", "FAJARDO", "VILLALBA", "ANASCO")) %>%
  ggplot(aes(YEAR, ON.SITE_RELEASE_TOTAL)) +
  geom_line() +
  facet_wrap(~COUNTY)

# Fixes education column, some had char and other data frames had int class
NDS2008$education = as.numeric(NDS2008$education)
NDS2006$education = as.numeric(NDS2006$education)
NDS2005$education = as.numeric(NDS2005$education)
NDS2004$education = as.numeric(NDS2004$education)
NDS2003$education = as.numeric(NDS2003$education)
NDS2002$education = as.numeric(NDS2002$education)
NDS2001$education = as.numeric(NDS2001$education)
NDS2000$education = as.numeric(NDS2000$education)

# bind death data
muertesTotal = bind_rows(NDS2008, NDS2007, NDS2006, NDS2005, NDS2004, 
                         NDS2003, NDS2002, NDS2001, NDS2000)

muertesCancer = muertesTotal %>% filter(cod_icd10 %like% "^C" & countryresidence == 1)
 

muertesCancer %>% filter(placeresidence %in% c(024, 108, 252)) %>%
  group_by(placeresidence, yeardeath) %>% summarize(total = n()) %>%
  ggplot(aes(yeardeath, total)) +
  geom_line() +
  facet_wrap(~placeresidence)



