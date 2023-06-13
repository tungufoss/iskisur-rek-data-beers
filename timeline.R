library(wesanderson)
# load data
events <- read.csv("special_events.csv") %>%
  mutate(date = as_date(date),
         enddate = as_date(enddate),
         period = ifelse(year(date) < 1990, '80s', 'Present Day'),
  )

# separate the data into birth, death and period events
births <- events %>%
  filter(event == "birth")
# for each row in birth, find the latest event in the timeline that is before the birth
births <- births %>%
  mutate(book = map_chr(enddate, ~timeline %>%
    filter(date < .x & ((type == 'book' & subtype == 'written') | type == 'podcast')
    ) %>%
    pull(event) %>%
    last))

deaths <- events %>%
  filter(event == "In Memoriam")

oneoff <- events %>%
  filter(event != "In Memoriam" &
           event != "birth" &
           is.na(enddate))

periods <- events %>%
  filter(!is.na(enddate) & event != "birth")

# First plot
df <- timeline %>%
  filter(subtype == 'written') %>%
  merge(births %>% select(book, period, remark),
        by.x = 'event', by.y = 'book', suffixes = c('', '.births'),
        all.x = T)

colorsbooks = wes_palette(n = 3, name = "GrandBudapest1")
p1 <- ggplot() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank()) +
  geom_curve(data = births %>% filter(period == '80s'),
             aes(x = date, y = 0.5, xend = enddate, color = book),
             curvature = -0.5, yend = 0.5,
             arrow = arrow(ends = "first", length = unit(0.02, "npc"))) +
  geom_text(data = births %>% filter(period == '80s'),
            aes(x = date), label = '\U0001F467',
            y = 0.5, hjust = 0.3, vjust = 1.9) +
  scale_y_continuous(limits = c(0.4, 1), expand = c(0, 0)) +
  labs(x = NULL, y = NULL, color = NULL) +
  geom_point(data = df, aes(x = date, y = 0.55), alpha = 0.4) +
  geom_text(data = df, aes(x = date, y = 0.6, label = event),
            hjust = 0, vjust = .5, size = 2, angle = 90, alpha = .2) +
  geom_text(data = df %>% filter(period.births == '80s'),
            aes(x = date, y = 0.6, label = event, color = event),
            hjust = 0, vjust = .5, size = 2, angle = 90) +
  geom_point(
    data = df %>% filter(period.births == '80s'),
    aes(x = date, y = 0.55, color = event), size = 3) +
  theme(legend.position = "none") +
  scale_color_manual(values = colorsbooks)

# Second plot
mycolors = wes_palette(n = 5, name = "Zissou1")[c(1, 3, 5)]
df <- timeline %>% filter(period != '80s')
df2 <- df %>%
  filter(subtype == 'Storytel') %>%
  group_by(event) %>%
  summarise(
    date = mean(date), n = n(), vjust = ifelse(n > 1, 0.5, ifelse(event == 'Special: Introduction', -1, +1))) %>%
  merge(births %>% select(book, period, remark),
        by.x = 'event', by.y = 'book', suffixes = c('', '.births'),
        all.x = T)
p2 <- ggplot() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank()) +
  geom_rect(data = periods %>%
    filter(period != '80s') %>%
    arrange(date) %>%
    mutate(ymid = c(.3, .4, .5)),
            aes(xmin = date, xmax = enddate, ymin = ymid - 0.05, ymax = ymid + 0.05, fill = event),
            alpha = 0.2) +
  scale_fill_manual(values = mycolors) +
  geom_curve(data = births %>% filter(period != '80s'),
             aes(x = date, y = 0.5, xend = enddate, yend = 0.5),
             curvature = -0.4, color = colorsbooks[3],
             arrow = arrow(ends = "first", length = unit(0.02, "npc")),
  ) +
  geom_text(data = births %>% filter(period != '80s'),
            aes(x = date), y = 0.45, label = '\U0001F476',
            hjust = 0, vjust = 0.5) +
  geom_text(data = deaths %>% filter(period != '80s'),
            aes(x = date, y = -Inf, label = paste0('\U1F47B', remark)),
            hjust = 0, vjust = .5, angle = 90, size = 2) +
  scale_y_continuous(limits = c(0.15, 1), expand = c(0, 0)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  geom_point(data = df %>% filter(subtype == 'audio'), aes(x = date, y = 0.4), alpha = 0.5, color = mycolors[1]) +
  geom_point(data = df %>% filter(subtype == 'Alvarp'), aes(x = date, y = 0.3), alpha = 0.5, color = mycolors[2]) +
  geom_point(data = df %>% filter(subtype == 'Storytel'), aes(x = date, y = 0.5), alpha = 0.5, color = mycolors[3]) +
  geom_text(data = df2,
            aes(x = date, y = 0.55, label = event, vjust = vjust),
            hjust = 0, size = 2, angle = 90, alpha = .2) +
  geom_text(data = df2 %>% filter(period != '80s'),
            aes(x = date, y = 0.55, label = event), color = colorsbooks[3],
            show.legend = F, hjust = 0, vjust = .5, size = 2, angle = 90) +
  geom_text(data = oneoff, aes(x = date, y = -Inf, label = event),
            hjust = 0, vjust = .5, size = 2, angle = 90) +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  # expand 2016 into the margin
  scale_x_date(limits = c(as.Date("2016-08-15"), as.Date("2021-1-15")), expand = c(0, 0))

# Combine plots
p <- plot_grid(p1, p2, ncol = 1, rel_heights = c(1, 1.4))
# Save the combined plot
ggsave('charts/timeline.png', p, width = 10, height = 5, dpi = 300)
