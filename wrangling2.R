
newDeaths2007_2014 = X2007_2014 %>% filter(conicd == "C")

newDeaths2015_2020 = X2015_2020 %>% filter(`DeathCause_I (ID)` %like% "^C" & ResidencePlace %like% "PUERTO RICO")

dat = strsplit(newDeaths2015_2020$ResidencePlace, split = "PUERTO RICO, ")
data2 = data.frame(ResidencePlace = newDeaths2015_2020$ResidencePlace)

j = 1
for(x in dat){
  newDeaths2015_2020$ResidencePlace[j] = x[2]
  j = j + 1
}

rm(data2, dat,i,j,x, data3)

newDeaths2007_2014 = newDeaths2007_2014 %>% group_by(dthyear, `municipio de residencia`) %>%
  summarize(total = n())

newDeaths2015_2020 = newDeaths2015_2020 %>% group_by(DeathDate_Year, ResidencePlace) %>%
  summarize(total = n())


newDeaths2007_2014$`municipio de residencia` = toupper(newDeaths2007_2014$`municipio de residencia`)

newDeaths2007_2014$`municipio de residencia`[newDeaths2007_2014$`municipio de residencia`=="AÑASCO"] = "ANASCO"
newDeaths2007_2014$`municipio de residencia`[newDeaths2007_2014$`municipio de residencia`=="BAYAMÓN"] = "BAYAMON"
newDeaths2007_2014$`municipio de residencia`[newDeaths2007_2014$`municipio de residencia`=="CANÓVANAS"] = "CANOVANAS"
newDeaths2007_2014$`municipio de residencia`[newDeaths2007_2014$`municipio de residencia`=="CATAÑO"] = "CATANO"
newDeaths2007_2014$`municipio de residencia`[newDeaths2007_2014$`municipio de residencia`=="COMERÍO"] = "COMERIO"
newDeaths2007_2014$`municipio de residencia`[newDeaths2007_2014$`municipio de residencia`=="GUÁNICA"] = "GUANICA"
newDeaths2007_2014$`municipio de residencia`[newDeaths2007_2014$`municipio de residencia`=="JUANA DÍAZ"] = "JUANA DIAZ"
newDeaths2007_2014$`municipio de residencia`[newDeaths2007_2014$`municipio de residencia`=="LOÍZA"] = "LOIZA"
newDeaths2007_2014$`municipio de residencia`[newDeaths2007_2014$`municipio de residencia`=="MANATÍ"] = "MANATI"
newDeaths2007_2014$`municipio de residencia`[newDeaths2007_2014$`municipio de residencia`=="MAYAGÜEZ"] = "MAYAGUEZ"
newDeaths2007_2014$`municipio de residencia`[newDeaths2007_2014$`municipio de residencia`=="PEÑUELAS"] = "PENUELAS"
newDeaths2007_2014$`municipio de residencia`[newDeaths2007_2014$`municipio de residencia`=="RINCÓN"] = "RINCON"
newDeaths2007_2014$`municipio de residencia`[newDeaths2007_2014$`municipio de residencia`=="RÍO GRANDE"] = "RIO GRANDE"
newDeaths2007_2014$`municipio de residencia`[newDeaths2007_2014$`municipio de residencia`=="SAN GERMÁN"] = "SAN GERMAN"
newDeaths2007_2014$`municipio de residencia`[newDeaths2007_2014$`municipio de residencia`=="SAN SEBASTIÁN"] = "SAN SEBASTIAN"

colnames(newDeaths2007_2014)[1] <- "YEAR"
colnames(newDeaths2007_2014)[2] <- "COUNTY"

colnames(newDeaths2015_2020)[1] <- "YEAR"
colnames(newDeaths2015_2020)[2] <- "COUNTY"

totalDeathsPerYearandTown = full_join(newDeaths2007_2014, newDeaths2015_2020)

colnames(totalDeathsPerYearandTown)[3] <- "totalDeaths"

cancerTotal2 = cancerTotal %>% filter(YEAR < 2007)

totalDeathsPerYearandTown = full_join(totalDeathsPerYearandTown, cancerTotal2)

analysisData = full_join(emisionesToxicas, totalDeathsPerYearandTown)


