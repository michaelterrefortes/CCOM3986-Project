install.packages('tidygeocoder')
devtools::install_github("azizka/speciesgeocodeR")
library(tidygeocoder)
library(readr)
library(ggplot2)
library(magrittr)
library(speciesgeocodeR)
library(raster)
library(rgdal)

# getting 2015-2020 complete address for geolocate

new2015_2020 = X2015_2020 %>% filter(`DeathCause_I (ID)` %like% "^C" & ResidencePlace %like% "PUERTO RICO") 

new2015_2020 = new2015_2020 %>%
  mutate(zipcode = paste0("00", as.character(new2015_2020$ResidencePlaceAddressZip)))

new2015_2020 = new2015_2020 %>% 
  mutate(address = paste(ResidencePlaceAddress1, ResidencePlaceAddress2)) %>%
  mutate(addressComplete = paste(address, ResidencePlace, zipcode, sep = ", "))

new2015_2020 = new2015_2020 %>%
  geocode(addressComplete, method = 'arcgis', lat = latitude , long = longitude)


# getting 2007-2014 complete address for geolocate

new2007_2014 = X2007_2014 %>% filter(causes %like% "^C" & ctryres %like% "Puerto Rico") 

new2007_2014 = new2007_2014 %>%
  mutate(addressComplete = paste(dadress, `municipio de residencia`, "Puerto Rico", sep=", "))

new2007_2014 = new2007_2014 %>%
  geocode(addressComplete, method = 'arcgis', lat = latitude , long = longitude)

#Salinas Analysis 2015-2020

salinas = new2015_2020 %>% filter(ResidencePlace %like% "SALINAS") %>%
  geocode(addressComplete, method = 'arcgis', lat = latitude , long = longitude)

salinas2 = new2007_2014 %>% filter(`municipio de residencia` == "Salinas") %>%
  geocode(addressComplete, method = 'arcgis', lat = latitude , long = longitude)


salEmi = data %>% filter(COUNTY == "SALINAS" & FACILITY_NAME %like% "STERI" & CARCINOGEN == "YES") %>% 
  distinct(FACILITY_NAME, .keep_all = TRUE)



#plot(salinas2$longitude, salinas2$latitude,  cex=.5, col = "red", xlim=c(-66.35, -66.18), ylim=c(17.93,18.1))
plot(salinas$longitude, salinas$latitude,  cex=.5, col = "blue")
#plot(salinas2$longitude, salinas2$latitude,  cex=.5, col = "red")
points(salEmi$Lon, salEmi$Lat, cex=2, col="black", pch=6)
lines(cn)

# Fajardo 2015-2020

fajardo = new2015_2020 %>% filter(ResidencePlace %like% "FAJARDO") %>%
  geocode(addressComplete, method = 'arcgis', lat = latitude , long = longitude) 

fajemi = data %>% filter(COUNTY == "FAJARDO" & CARCINOGEN == "YES") %>% 
  distinct(FACILITY_NAME, .keep_all = TRUE)

#plot(cn, xlim=c(-65.7, -65.6), ylim=c(18.2,18.5))
plot(fajardo$longitude, fajardo$latitude,  cex=.5, col = "red")
points(fajemi$Lon, fajemi$Lat, cex=.5, col="blue")

# Anasco 2015-2020

anasco = new2015_2020 %>% filter(ResidencePlace %like% "ANASCO") %>%
  geocode(addressComplete, method = 'arcgis', lat = latitude , long = longitude) 

anasEmi = data %>% filter(COUNTY == "ANASCO" & CARCINOGEN == "YES") %>% 
  distinct(FACILITY_NAME, .keep_all = TRUE)

plot(anasco$longitude, anasco$latitude,  cex=.5, col = "red")
points(anasEmi$Lon, anasEmi$Lat, cex=.5, col="blue")




## Spacial
# my_data = as_tibble(salinas)
# my_data = bind_cols(my_data$DeathNumber, my_data$longitude, my_data$latitude)
# colnames(my_data)[1] = "DeathNumber"
# colnames(my_data)[2] = "lon"
# colnames(my_data)[3] = "lat"
# 
# sp <- vect(my_data, crs="+proj=longlat +datum=WGS84")
# cn <- spat_data("pt_countries")
# 
# 
# laea <-"+proj=laea  +lat_0=0 +lon_0=-80"
# clb <- project(cn, laea)
# pts <- project(sp, laea)
# plot(pts, xlim=c(1430000, 1480000), ylim=c(1990000, 2015000))
# lines(clb)
# 
# r <- rast(clb)
# 
# # 200 km = 200000 m
# res(r) <- 9990
# rich <- rasterize(pts, r, "DeathNumber", function(x, ...) length(unique(na.omit(x))))
# plot(rich,  xlim=c(1430000, 1490000), ylim=c(1990000, 2015000))
# lines(clb)
# 
# plot(clb, xlim=c(1430000, 1480000), ylim=c(1990000, 2015000))
# 

## other raster of Salinas

my_data = bind_cols(salinas$DeathNumber, salinas$longitude, salinas$latitude)
my_data = bind_rows(my_data, bind_cols(salinas2$natnum, salinas2$longitude, salinas2$latitude))

gocc <- RichnessGrid(bind_cols(salinas$DeathNumber, salinas$longitude, salinas$latitude), res=0.015)

plot(gocc)
lines(cn)
points(salEmi$Lon, salEmi$Lat, cex=2, col="blue", pch=6)

# Raster in Fajardo

gocc <- RichnessGrid(bind_cols(fajardo$DeathNumber, fajardo$longitude, fajardo$latitude), res=0.015)
plot(gocc)
lines(cn)


# Raster in Añasco

gocc <- RichnessGrid(bind_cols(anasco$DeathNumber, anasco$longitude, anasco$latitude), res=0.015)
plot(gocc)
lines(cn)



## Other analysis

new2015_2020 %>% filter(ResidencePlace %like% "BARCELONETA") %>% group_by(DeathDate_Year, ResidenceZone) %>%
  summarize(total = n()) %>%
  ggplot(aes(DeathDate_Year, total)) +
  geom_line() +
  facet_wrap(~ResidenceZone,  scales = "free_y")



