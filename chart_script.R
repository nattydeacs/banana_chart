library(tidyverse)
library(ggstream)
library(scales)
setwd("/Users/natdeacon/Desktop/GitHub/banana_chart")
df <- read.csv("FAOSTAT_data_7-7-2022.csv")
sort(unique(df$Partner.Countries))

nations <- c("Ecuador", "Philippines", "Costa Rica", "Colombia", "Guatemala", "Luxembourg", 
             "Honduras", "United States of America", "Panama", "Cameroon")

df <- filter(df, Reporter.Countries %in% nations) %>%
  filter(Element == "Export Quantity")%>%
  group_by(Reporter.Countries, Year) %>%
  summarise(tonnes_exports = sum(Value, na.rm = TRUE))

banancols <- c("#7c9800", "#dd9821", "#804a2d", "#e4cf9e", "#f0eacf",
              "#dbc900", "#ae6c38", "#acc600", "#fee528", "#d48f1a")

chart <- ggplot(df, aes(x= Year, y = tonnes_exports, fill = Reporter.Countries)) +
  geom_stream(type = "ridge") +
  guides(fill = guide_legend(title = "Land")) +
  scale_fill_manual(values = banancols) +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6, accuracy = 2)) +
  scale_x_continuous(breaks = seq(1986, 2020, 2)) +
  xlab("")+
  ylab("Export Von Banaanen (tonnes)")+
  theme(legend.position = "right", panel.grid = element_blank(), 
        panel.background = element_rect("#fcf9f3"),
        plot.background = element_rect(fill = "#fcf9f3"),
        legend.background = element_rect(fill = "#fcf9f3")) +
  coord_cartesian(xlim = c(1987.5, 2018.75))+
  ggtitle("Jährliche Bananenexporte, ausgewählte Länder")


chart