library(tidyverse)
library(ggplot2)
library(data.table)
install.packages("terra")
library(terra)
if (!require("rspat")) remotes::install_github('rspatial/rspat')
## Loading required package: rspat
library(rspat)

pueblos <- c("4" = "ADJUNTAS",
            "8" = "AGUADA",
            "12" = "AGUADILLA",
            "16" = "AGUAS BUENAS",
            "20" = "AIBONITO",
            "24" = "ANASCO",
            "28" = "ARECIBO",
            "32" = "ARROYO",
            "36" = "BARCELONETA",
            "40" = "BARRANQUITAS",
            "44" = "BAYAMON",
            "48" = "CABO ROJO",
            "52" = "CAGUAS",
            "56" = "CAMUY",
            "60" = "CANOVANAS",
            "64" = "CAROLINA",
            "68" = "CATANO",
            "72" = "CAYEY",
            "76" = "CEIBA",
            "80" = "CIALES",
            "84" = "CIDRA",
            "88" = "COAMO",
            "92" = "COMERIO",
            "96" = "COROZAL",
            "100" = "CULEBRA",
            "104" = "DORADO",
            "108" = "FAJARDO",
            "112" = "FLORIDA",
            "116" = "GUANICA",
            "120" = "GUAYAMA",
            "124" = "GUAYANILLA",
            "128" = "GUAYNABO",
            "132" = "GURABO",
            "136" = "HATILLO",
            "140" = "HORMIGUEROS",
            "144" = "HUMACAO",
            "148" = "ISABELA",
            "152" = "JAYUYA",
            "156" = "JUANA DIAZ",
            "160" = "JUNCOS",
            "164" = "LAJAS",
            "168" = "LARES",
            "172" = "LAS MARIAS",
            "176" = "LAS PIEDRAS",
            "180" = "LOIZA",
            "184" = "LUQUILLO",
            "188" = "MANATI",
            "192" = "MARICAO",
            "196" = "MAUNABO",
            "200" = "MAYAGUEZ",
            "204" = "MOCA",
            "208" = "MOROVIS",
            "212" = "NAGUABO",
            "216" = "NARANJITO",
            "220" = "OROCOVIS",
            "224" = "PATILLAS",
            "228" = "PENUELAS",
            "232" = "PONCE",
            "236" = "QUEBRADILLAS",
            "240" = "RINCON",
            "244" = "RIO GRANDE",
            "248" = "SABANA GRANDE",
            "252" = "SALINAS",
            "256" = "SAN GERMAN",
            "260" = "SAN JUAN",
            "272" = "SAN JUAN",
            "274" = "SAN JUAN",
            "276" = "SAN LORENZO",
            "280" = "SAN SEBASTIAN",
            "284" = "SANTA ISABEL",
            "288" = "TOA ALTA",
            "292" = "TOA BAJA",
            "296" = "TRUJILLO ALTO",
            "300" = "UTUADO",
            "304" = "VEGA ALTA",
            "308" = "VEGA BAJA",
            "312" = "VIEQUES",
            "316" = "VILLALBA",
            "320" = "YABUCOA",
            "324" = "YAUCO",
            "346" = "EN PUERTO RICO")

# Save toxic release data
data = toxics_release_inventory_data %>% filter(CARCINOGEN == "YES")


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

# All deaths by cancer and residing in PR
muertesCancer = muertesTotal %>% filter(cod_icd10 %like% "^C" & countryresidence == 1)

# All this chunk of code makes sure city names are without "MUNICIPIO" at the end
# some cities had "BARCELONETA" and "BARCELONETA MUNICIPIO"
dat = strsplit(data$COUNTY, split = " MUNICIPIO")
data2 = data$COUNTY
data2$COUNTY = data$COUNTY

j = 1
for(x in dat) {
  for(i in x){
    #data2$COUNTY[j] = i
    print(i)
    j = j + 1
  }
}

sort(data2$COUNTY)
unique(data2$COUNTY)

data$COUNTY = data2$COUNTY

rm(data2, dat,i,j,x, data3)


# Adds city names to each death. This is done to not have to deal with integer city ID
muertesCancer = muertesCancer %>% group_by(placeresidence) %>% mutate(pueblo = pueblos[as.character(placeresidence)])

# Add monthly deaths date
muertesCancer$date <- as.Date(with(muertesCancer, paste(yeardeath, monthdeath, 1, sep="-")),"%Y-%m-%d")


# All this chunk of code is used to separate the Latitude column into
# Lat and Lon columns in order to plot in map on the analysis file
data = data[, c("Location.1", names(data)[names(data)!="Location.1"])]

dat = strsplit(data$Location.1, split = ",")
data2 = as.data.frame(data$Location.1)
names(data2)[1] = "Location.1"

j = 1
for (i in dat){
  data2$Lat[j] = i[1]
  data2$Lon[j] = i[2]
  j = j + 1
}

data2 = na.omit(data2)
y = 1
for (k in data2$Lat){
  if(strsplit(k, "")[[1]][1] == "("){
     data2$Lat[y] = strsplit(k, "\\(")[[1]][2]
  }
  y = y + 1
  
}

y = 1
for (k in data2$Lon){
  v = strsplit(k, "")[[1]]
  if(tail(v,n=1) == ")"){
    data2$Lon[y] = (strsplit(k, "\\)")[[1]])
  }
  y = y+1
}

rm(i, j, k, v, y, dat)

data = full_join(data, data2)

data = data[, c("Lon", names(data)[names(data)!="Lon"])]
data = data[, c("Lat", names(data)[names(data)!="Lat"])]
rm(data2)


# Adding Lat and Lng to deaths by town
PRcities = uscities %>% filter(state_id == "PR")

PRcities = PRcities %>% distinct(county_name, .keep_all= TRUE)

PRcities$county_name = toupper(PRcities$county_name)

PRcities$county_name[PRcities$county_name=="A??ASCO"] = "ANASCO"
PRcities$county_name[PRcities$county_name=="BAYAM??N"] = "BAYAMON"
PRcities$county_name[PRcities$county_name=="CAN??VANAS"] = "CANOVANAS"
PRcities$county_name[PRcities$county_name=="CATA??O"] = "CATANO"
PRcities$county_name[PRcities$county_name=="COMER??O"] = "COMERIO"
PRcities$county_name[PRcities$county_name=="GU??NICA"] = "GUANICA"
PRcities$county_name[PRcities$county_name=="JUANA D??AZ"] = "JUANA DIAZ"
PRcities$county_name[PRcities$county_name=="LO??ZA"] = "LOIZA"
PRcities$county_name[PRcities$county_name=="MANAT??"] = "MANATI"
PRcities$county_name[PRcities$county_name=="MAYAG??EZ"] = "MAYAGUEZ"
PRcities$county_name[PRcities$county_name=="PE??UELAS"] = "PENUELAS"
PRcities$county_name[PRcities$county_name=="RINC??N"] = "RINCON"
PRcities$county_name[PRcities$county_name=="R??O GRANDE"] = "RIO GRANDE"
PRcities$county_name[PRcities$county_name=="SAN GERM??N"] = "SAN GERMAN"
PRcities$county_name[PRcities$county_name=="SAN SEBASTI??N"] = "SAN SEBASTIAN"

PRcities = data.frame(pueblo = PRcities$county_name, lat = PRcities$lat, lon = PRcities$lng)

muertesCancer = full_join(muertesCancer, PRcities)



