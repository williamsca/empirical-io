# Replication files 
# Author: Colin Williams
# Last updated: 25 Feb 2023

rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, haven, stargazer, ggplot2)

# Question 1: Download airline dataset ----
dt <- as.data.table(read_dta("HW3/data/airline1997data.dta"))

# NOTE: year, quarter, origin, dest, tkcarrier, T100nonstop

# Question 3: Summary statistics for prices and quantities ----
stargazer(dt, summary = TRUE, keep =  c("MdW_oneway_itinfare_ticket", "totalpassengers"),
          type = "text", summary.stat = c("n", "mean", "median", "sd", "min", "max"))

ggplot(dt, aes(x = MdW_oneway_itinfare_ticket)) + 
  geom_histogram() +
  labs(title = "", x = "Price ($)", y = "Frequency", caption = "") +
  theme_light() + theme(plot.caption = element_text(hjust = 0)) # left-align caption

ggplot(dt, aes(x = totalpassengers)) + 
  geom_histogram() +
  labs(title = "", x = "Total Passengers", y = "Frequency", caption = "") +
  theme_light() + theme(plot.caption = element_text(hjust = 0)) # left-align caption

ggplot(dt, aes(x = marketdistanceticket, y  = MdW_oneway_itinfare_ticket)) +
  geom_point(size = 1, alpha = .5) +
  geom_line(stat = "smooth", method = "loess", color = "red", size = 1) +
  labs(x = "Nonstop Distance (mi.)",  y = "Ticket Fare ($)") +
  theme_light()

lm.distance <- lm(MdW_oneway_itinfare_ticket ~ marketdistanceticket, data = dt)
stargazer(lm.distance, type = "text")

# Question 4: Summary statistics by firm type ----
dt[tkcarrier %in% c("AA", "CO", "DL", "NW", "TW", "UA", "US"), carrierType := "Legacy"]
dt[tkcarrier == "WN", carrierType :=  "Southwest"] # TODO: this code doesn't appear...
dt[is.na(carrierType), carrierType := "Other LCC"]

stargazer(dt[carrierType == "Legacy"], summary = TRUE, keep =  c("MdW_oneway_itinfare_ticket", "totalpassengers"),
          type = "text", summary.stat = c("n", "mean", "median", "sd", "min", "max"))

stargazer(dt[carrierType == "Southwest"], summary = TRUE, keep =  c("MdW_oneway_itinfare_ticket", "totalpassengers"),
          type = "text", summary.stat = c("n", "mean", "median", "sd", "min", "max"))

stargazer(dt[carrierType == "Other LCC"], summary = TRUE, keep =  c("MdW_oneway_itinfare_ticket", "totalpassengers"),
          type = "text", summary.stat = c("n", "mean", "median", "sd", "min", "max"))

dt[, isDup := .N, by = .(year, quarter, origin, dest, tkcarrier, T100nonstop)]

