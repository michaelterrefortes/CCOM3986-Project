install.packages('tidygeocoder')
library(tidygeocoder)

# getting 2015-2020 complete address for geolocate

new2015_2020 = X2015_2020 %>% filter(`DeathCause_I (ID)` %like% "^C" & ResidencePlace %like% "PUERTO RICO") 

new2015_2020 = new2015_2020 %>%
  mutate(zipcode = paste0("00", as.character(new2015_2020$ResidencePlaceAddressZip)))

new2015_2020 = new2015_2020 %>% 
  mutate(address = paste(ResidencePlaceAddress1, ResidencePlaceAddress2)) %>%
  mutate(addressComplete = paste(address, ResidencePlace, zipcode, sep = ", "))

new2015_2020 = new2015_2020 %>%
  geocode(addressComplete, method = 'arcgis', lat = latitude , long = longitude)


#Salinas Analysis 2015-2020

salinas = new2015_2020 %>% filter(ResidencePlace %like% "SALINAS") %>%
  geocode(addressComplete, method = 'arcgis', lat = latitude , long = longitude)

salinas2 = X2007_2014 %>% filter(`municipio de residencia` %like% "Salinas" & causes %like% "^C") %>%
  geocode(dadress, method = 'arcgis', lat = latitude , long = longitude)

salEmi = data %>% filter(COUNTY == "SALINAS" & FACILITY_NAME %like% "STERI" & CARCINOGEN == "YES") %>% 
  distinct(FACILITY_NAME, .keep_all = TRUE)


plot(salinas$longitude, salinas$latitude,  cex=.5, col = "red")
#plot(salinas2$longitude, salinas2$latitude,  cex=.5, col = "red")
points(salEmi$Lon, salEmi$Lat, cex=.5, col="blue")

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
#sp <- vect(salinas, crs="+proj=longlat +datum=WGS84")

laea <-"+proj=laea  +lat_0=0 +lon_0=-80"
clb <- project(cn, laea)
pts <- project(salinas, laea)
plot(clb)

r <- rast(cn)
# 200 km = 200000 m
res(r) <- 100
rich <- rasterize(salinas, r, c("longitude", "latitude"), function(x, ...) length(unique(na.omit(x))))
plot(rich, xlim=c(-67.5, -65.5), ylim=c(17.5,19))
cbind(salinas$longitude, salinas$latitude)

## Other analysis

new2015_2020 %>% filter(ResidencePlace %like% "BARCELONETA") %>% group_by(DeathDate_Year, ResidenceZone) %>%
  summarize(total = n()) %>%
  ggplot(aes(DeathDate_Year, total)) +
  geom_line() +
  facet_wrap(~ResidenceZone,  scales = "free_y")



