install.packages("tidycensus")
library(tidycensus)
library(tidyverse)
library(tigris)

census_api_key("a40e01788505e7389eb03ba617b5916eab80c2a8", install = TRUE)

municipios = get_estimates(geography = "county", 
                           product = "population", 
                           state="PR", 
                           geometry = TRUE) 
                  

plot(municipios["value"])

allTimeDeathsByCounty = analysisData %>% group_by(COUNTY) %>%
  summarize(n = sum(totalDeaths, na.rm = TRUE))

# Cleaning data
municipios = na.omit(municipios)

tab = strsplit(municipios$NAME, split = " Municipio,")

j = 1
for (x in tab){
  municipios$NAME[j] = tab[[j]][1]
  j = j + 1
}

rm(j, tab, x)

municipios$NAME = toupper(municipios$NAME)


municipios$NAME[municipios$NAME=="AÑASCO"] = "ANASCO"
municipios$NAME[municipios$NAME=="BAYAMÓN"] = "BAYAMON"
municipios$NAME[municipios$NAME=="CANÓVANAS"] = "CANOVANAS"
municipios$NAME[municipios$NAME=="CATAÑO"] = "CATANO"
municipios$NAME[municipios$NAME=="COMERÍO"] = "COMERIO"
municipios$NAME[municipios$NAME=="GUÁNICA"] = "GUANICA"
municipios$NAME[municipios$NAME=="JUANA DÍAZ"] = "JUANA DIAZ"
municipios$NAME[municipios$NAME=="LOÍZA"] = "LOIZA"
municipios$NAME[municipios$NAME=="MANATÍ"] = "MANATI"
municipios$NAME[municipios$NAME=="MAYAGÜEZ"] = "MAYAGUEZ"
municipios$NAME[municipios$NAME=="PEÑUELAS"] = "PENUELAS"
municipios$NAME[municipios$NAME=="RINCÓN"] = "RINCON"
municipios$NAME[municipios$NAME=="RÍO GRANDE"] = "RIO GRANDE"
municipios$NAME[municipios$NAME=="SAN GERMÁN"] = "SAN GERMAN"
municipios$NAME[municipios$NAME=="SAN SEBASTIÁN"] = "SAN SEBASTIAN"
municipios$NAME[municipios$NAME=="LAS MARÍAS"] = "LAS MARIAS"

analysisData$COUNTY[analysisData$COUNTY=="LAS MARÍAS"] = "LAS MARIAS"

allTimeDeathsByCounty = allTimeDeathsByCounty %>% filter(!row_number() %in% c(82, 26, 28, 31))

colnames(allTimeDeathsByCounty)[1] = "NAME"
colnames(allTimeDeathsByCounty)[2] = "TotalCancerDeaths"

municipios = merge(municipios, allTimeDeathsByCounty, by = "NAME")

# Analysis

municipios = municipios %>% group_by(NAME) %>% mutate(rateCancer = TotalCancerDeaths/value*1000)

plot(municipios["TotalCancerDeaths"])

plot(municipios["rateCancer"])

ggplot(data = municipios, aes(fill = rateCancer)) + 
  geom_sf()

