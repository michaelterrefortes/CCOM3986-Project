library('ggpubr')

#

muertesCancer %>% group_by(cod_icd10) %>%
  summarize(total = n()) %>%
  ggplot(aes(cod_icd10, total)) +
  geom_bar(stat="identity")


# Graph of toxic release per year
data %>% group_by(YEAR) %>%
  mutate(total = sum(ON.SITE_RELEASE_TOTAL)) %>%
  ggplot(aes(YEAR, total)) +
  geom_point()


# Toxic releases in interested areas
data %>% group_by(YEAR, COUNTY) %>% filter(CARCINOGEN == "YES") %>% filter(YEAR >= 2000) %>%
  filter(COUNTY %in% c("SALINAS", "FAJARDO", "VILLALBA", "ANASCO")) %>%
  summarize(total = sum(ON.SITE_RELEASE_TOTAL)) %>%
  ggplot(aes(YEAR, total)) +
  geom_line() +
  facet_wrap(~COUNTY)

# Toxic releases in San Juan
data %>% group_by(YEAR, COUNTY) %>% filter(CARCINOGEN == "YES") %>% filter(YEAR >= 2000) %>%
  filter(COUNTY %in% c("SAN JUAN")) %>%
  summarize(total = sum(ON.SITE_RELEASE_TOTAL)) %>%
  ggplot(aes(YEAR, total)) +
  geom_line() +
  facet_wrap(~COUNTY)

# Plot cancer deaths by year in specific cities
muertesCancer %>% filter(pueblo %in% c("ANASCO", "FAJARDO", "SALINAS")) %>%
  group_by(pueblo, yeardeath) %>% summarize(total = n()) %>%
  ggplot(aes(yeardeath, total)) +
  geom_line() +
  facet_wrap(~pueblo,  scales = "free_y")


# Cancer deaths in selected cities
muertesCancer %>% filter(pueblo %in% c("ANASCO", "FAJARDO", "SALINAS")) %>%
  group_by(date, pueblo) %>% summarize(total = n()) %>%
  ggplot(aes(date, total)) +
  geom_smooth() +
  facet_wrap(~pueblo)

# Analysis in Salinas

salinasMuertes = muertesCancer %>% filter(pueblo == "SALINAS")%>% group_by(YEAR = yeardeath) %>%
  summarize(totalDeaths = n())

salinasEmisiones = data %>% filter(COUNTY == "SALINAS") %>% group_by(YEAR) %>%
  summarize(totalEmissions = sum(ON.SITE_RELEASE_TOTAL))

datosSalina = full_join(salinasEmisiones, salinasMuertes)

datosSalina %>% filter(YEAR >= 2000 & YEAR <= 2008) %>%
  pivot_longer(-YEAR) %>%
  ggplot(aes(x=YEAR, y = value)) +
  geom_line() +
  facet_wrap(~name, ncol = 1, scales = "free_y")

muertesCancer %>% group_by(yeardeath, pueblo) %>%
  summarize(total = n()) %>%
  ggplot(aes(yeardeath, y = total)) +
  geom_line() +
  facet_wrap(~pueblo, scales = "free_y")

data %>% filter(YEAR >= 2000 & YEAR <= 2008) %>% group_by(YEAR, COUNTY) %>%
  summarize(total = sum(ON.SITE_RELEASE_TOTAL)) %>%
  ggplot(aes(YEAR, y = total)) +
  geom_line() +
  facet_wrap(~COUNTY, scales = "free_y")

## Other

emisionesToxicas = data %>% group_by(YEAR, COUNTY) %>% filter(YEAR >= 2000 & YEAR <= 2008) %>%
  summarize(totalEmisions = sum(ON.SITE_RELEASE_TOTAL))

cancerTotal = muertesCancer %>% group_by(yeardeath, pueblo) %>% filter(yeardeath >= 2000 & yeardeath <= 2008) %>%
  summarize(totalDeaths = n())

colnames(cancerTotal)[1] <- "YEAR"
colnames(cancerTotal)[2] <- "COUNTY"

analysisData = full_join(emisionesToxicas, cancerTotal)

analysisData %>% filter(COUNTY == "SALINAS") %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*400), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 20000) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./400, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))



analysisData %>% filter(COUNTY == "FAJARDO") %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*1), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 50) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./1, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))

analysisData %>% filter(COUNTY == "ANASCO") %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*100), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 3000) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./100, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))

analysisData %>% filter(COUNTY == "AIBONITO") %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*10), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 300) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./10, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))

analysisData %>% filter(COUNTY == "ARECIBO") %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*1000), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 300000) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./1000, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))

analysisData %>% filter(COUNTY == "BARCELONETA") %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*10000), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 300000) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./10000, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))

analysisData %>% filter(COUNTY == "BAYAMON") %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*50), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 20000) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./50, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))

analysisData %>% filter(COUNTY == "CAGUAS") %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*10), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 1000) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./10, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))

analysisData %>% filter(COUNTY == "SAN JUAN") %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*1), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 300) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./1, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))

analysisData %>% filter(COUNTY == "MAYAGUEZ") %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*10), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 3000) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./10, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))


analysisData %>% filter(COUNTY == "PONCE") %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*10), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 3000) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./10, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))


analysisData %>% filter(COUNTY == "FAJARDO") %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*1), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 30) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./1, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))

analysisData %>% filter(COUNTY == "LAS PIEDRAS") %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*10000), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 300000) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./10000, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))

analysisData %>% filter(COUNTY == "YAUCO") %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*1000), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 30000) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./1000, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))

analysisData %>% filter(COUNTY == "HUMACAO") %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*1000), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 30000) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./1000, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))

analysisData %>% filter(COUNTY == "GUAYAMA") %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*1000), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 30000) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./1000, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))

# Map Spacial Statistics
cn <- spat_data("pt_countries")

plot(cn, xlim=c(-67.5, -65.5), ylim=c(17.5,19), axes=TRUE)
points(data$Lon, data$Lat, cex=.5, col='blue')
points(muertesCancer$lon, muertesCancer$lat, cex=.5, col='red')



