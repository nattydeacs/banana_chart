library(tidyverse)
library(ggstream)
library(scales)
setwd("/Users/natdeacon/Desktop/GitHub/banana_chart")
df <- read.csv("FAOSTAT_data_7-7-2022.csv")
sort(unique(df$Partner.Countries))

nations <- c("Ecuador", "Philippines", "Costa Rica", "Colombia", "Guatemala", "Luxembourg", 
             "Honduras", "Unites States of America", "United Arab Emerates", "Panama", "Cameroon")

df <- filter(df, Reporter.Countries %in% nations) %>%
  filter(Element == "Export Quantity")%>%
  group_by(Reporter.Countries, Year) %>%
  summarise(tonnes_exports = sum(Value, na.rm = TRUE))

banancols <- c("#7c9800", "#dd9821", "#804a2d", "#e4cf9e", "#f0eacf",
              "#dbc900", "#ae6c38", "#acc600", "#938413", "#d48f1a")

chart <- ggplot(df, aes(x= Year, y = tonnes_exports, fill = Reporter.Countries)) +
  geom_stream(type = "ridge") +
  guides(fill = guide_legend(title = "Exporting Nation")) +
  scale_fill_manual(values = banancols) +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6, accuracy = 1)) +
  


chart