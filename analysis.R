

library(readr)
library(ggplot2)
library(dplyr)
library(stringr)

url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQtDsTt9VVyn-wptsMT6vxtCM2J1dPYWji90OOWj1v4c4pPz6Kwa6mQtOk096EU3npvJ9g4fcjnwLpp/pub?gid=1445637369&single=true&output=csv"

df <- read_csv(url, show_col_types = F)

to_plot <-
  df %>%
  filter(str_detect(Outcome, "Private motor vehicle")) %>%
  group_by(`Project name`, Outcome) %>%
  summarize(change = mean(Change, na.rm=T)) %>%
  ungroup() %>%
  mutate(Outcome = ifelse(Outcome == "Private motor vehicle AM peak travel time (seconds)", 
                          "AM traffic travel time",
                          "PM traffic travel time"))

ggplot(to_plot) +
  geom_point(
    aes(x=change, color=Outcome, y=`Project name`), size=3, alpha=.7
  ) +
  geom_vline(aes(xintercept=0), color="black") +
  geom_vline(aes(xintercept=mean(to_plot$change[to_plot$Outcome == "AM traffic travel time"], na.rm=T)), color="#F8766D", linetype="dashed") +
  geom_vline(aes(xintercept=mean(to_plot$change[to_plot$Outcome == "PM traffic travel time"], na.rm=T)), color="#00BFC4", linetype="dashed") +
  labs(x="Change in travel time in seconds", y="") +
  theme_minimal() +
  xlim(c(min(to_plot$change)-10, max(to_plot$change)+25)) +
  theme(legend.title = element_blank()) +
  ggtitle("Drivers' travel times faster on average after project completion")

to_plot <-
  df %>%
  filter(Outcome %in% c("Injury crashes", "All crashes")) %>%
  mutate(Change = Change*-1)

ggplot(to_plot) +
  geom_bar(aes(x=`Project name`, y=Change, fill=Outcome), stat="identity", position="dodge") +
  labs(y="% decrease in crashes", x="") +
  ggtitle("All projects saw decreases in crashes") +
  geom_hline(aes(yintercept=mean(to_plot$Change[to_plot$Outcome == "All crashes"], na.rm=T)), color="#F8766D", linetype="dashed", size=.8) +
  geom_hline(aes(yintercept=mean(to_plot$Change[to_plot$Outcome == "Injury crashes"], na.rm=T)), color="#00BFC4", linetype="dashed", size=.8) +
  theme_minimal() +
  theme(legend.title = element_blank())

to_plot <-
  df %>%
  filter(Outcome %in% c("Pedestrian injury crashes")) %>%
  mutate(Change = Change*-1)

ggplot(to_plot) +
  geom_bar(aes(x=`Project name`, y=Change), stat="identity", fill="seagreen") +
  labs(y="% decrease in crashes", x="") +
  ggtitle("Pedestrian crashes fell, sometimes to zero, after project completion") +
  geom_hline(aes(yintercept=mean(to_plot$Change, na.rm=T)), color="grey", linetype="dashed", size=1) +
  theme_minimal() +
  theme(legend.title = element_blank())

