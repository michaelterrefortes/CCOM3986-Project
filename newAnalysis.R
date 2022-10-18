
analysisData %>% filter(COUNTY == "SALINAS") %>% filter(YEAR >= 2000) %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*4000), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 20000) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./4000, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))

analysisData %>% filter(COUNTY == "FAJARDO") %>% filter(YEAR >= 2000) %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*1), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 50) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./1, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))

analysisData %>% filter(COUNTY == "ANASCO") %>% filter(YEAR >= 2000) %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*1000), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 3000) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./1000, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))

analysisData %>% filter(COUNTY == "AIBONITO") %>% filter(YEAR >= 2000) %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*150), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 300) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./150, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))

analysisData %>% filter(COUNTY == "ARECIBO") %>% filter(YEAR >= 2000) %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*10000), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 300000) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./10000, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))

analysisData %>% filter(COUNTY == "BARCELONETA") %>% filter(YEAR >= 2000) %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*100000), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 300000) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./100000, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))

analysisData %>% filter(COUNTY == "BAYAMON") %>% filter(YEAR >= 2000) %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*10000), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 20000) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./10000, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))

analysisData %>% filter(COUNTY == "CAGUAS") %>% filter(YEAR >= 2000) %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*10), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 1000) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./10, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))

analysisData %>% filter(COUNTY == "SAN JUAN") %>% filter(YEAR >= 2000) %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*10), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 300) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./10, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))

analysisData %>% filter(COUNTY == "MAYAGUEZ") %>% filter(YEAR >= 2000) %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*100), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 3000) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./100, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))


analysisData %>% filter(COUNTY == "PONCE") %>% filter(YEAR >= 2000) %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*1000), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 3000) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./1000, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))


analysisData %>% filter(COUNTY == "FAJARDO") %>% filter(YEAR >= 2000) %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*1), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 30) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./1, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))


analysisData %>% filter(COUNTY == "LAS PIEDRAS") %>% filter(YEAR >= 2000) %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*100000), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 300000) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./100000, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))


analysisData %>% filter(COUNTY == "YAUCO") %>% filter(YEAR >= 2000) %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*10000), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 30000) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./10000, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))


analysisData %>% filter(COUNTY == "HUMACAO") %>% filter(YEAR >= 2000) %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*10000), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 30000) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./10000, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))


analysisData %>% filter(COUNTY == "GUAYAMA") %>% filter(YEAR >= 2000) %>%
  ggplot(aes(x = YEAR)) +
  geom_line(aes(y = totalEmisions), color = "blue") +
  geom_line(aes(y = totalDeaths*1000), color = "red") +
  stat_cor(mapping = aes(totalDeaths, totalEmisions), label.x = 2001, label.y = 30000) +
  scale_y_continuous(name = "Pounds of Emissions per year",
                     sec.axis = sec_axis(~./1000, name = "Total Deaths per year")) +
  theme(axis.title.y = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))

