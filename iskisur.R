library(tidyverse)
library(lubridate)
dat <- read.csv("books.csv", sep = ',') %>%
  mutate(
    is_release = ymd(is_release),
    est_publication = ymd(est_publication),
    next_release = lead(is_release),
    time_to_release = next_release - is_release,
  )

dat %>% summarise(
  avg_time_to_release = mean(time_to_release, na.rm = TRUE),
  avg_page_count = mean(pages),
  avg_chapters = mean(chapters)
)

years <- min(dat$year):max(dat$year)
ggplot(dat, aes(x = year)) +
  geom_histogram(bins = length(years)) +
  labs(x = "Year", y = "Number of books", title = "Number of books published by year")

# change in translator
dat %>%
  group_by(translator) %>%
  summarise(n = n(),
            first_date = min(est_publication),
            last_date = max(est_publication),
            period = last_date - first_date,
            # convert period to years
            period = as.numeric(period, units = "days") / 365.25,
  ) %>%
  arrange(first_date) %>%
  summarise(
    started = first(interaction(translator, n, sep = ' #')),
    finished = last(interaction(translator, n, sep = ' #')),
    change = as.Date('1987-03-06')
  ) -> translator

dat %>%
  # plot the release date of each book
  ggplot(aes(x = est_publication, y = pages)) +
  geom_point() +
  geom_smooth() +
  geom_vline(xintercept = translator$change, color = "red", linetype = 'dashed') +
  annotate("text", x = translator$change, y = Inf, label = translator$started, vjust = -1, hjust = 1, color = 'red', angle = 90) +
  annotate("text", x = translator$change, y = Inf, label = translator$finished, vjust = 1.5, hjust = 1, color = 'red', angle = 90) +
  scale_x_date("", date_breaks = "1 year", date_labels = "%Y")

dat %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(y = pages, x = year)) +
  geom_boxplot() +
  geom_point() +
  labs(x = NULL, y = "Page Count", title = "Page Count Distribution by Year")


family <- read.csv('family.csv', sep = ',') %>%
  mutate(age = ifelse(is.na(death), 1960, death) - birth)

family %>% arrange(-age) %>% head()

family %>%
  filter(!is.na(birth)) %>%
  ggplot(aes(x = birth, y = age)) + geom_point()


# Create the Gantt chart
ggplot(family, aes(y = name, yend = name, x = birth, xend = death)) +
  geom_segment(size = 10) +
  geom_text(aes(label = name), vjust = 0.5, hjust = -0.1, size = 3, nudge_x = 365) +
#  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  labs(x = "Date", y = NULL, title = "Lifespan Gantt Chart") +
  theme_minimal()

family %>%
  filter(!is.na(dead)) %>%
  summarise(mean(age, na.rm = TRUE))

family %>% group_by(gender) %>% tally()

marriage <- read.csv('marriage.csv')


family %>%
  select(id, children) %>%
  filter(!is.na(children)) %>%
  separate_rows(children, sep = ";") %>%
  group_by(id) %>%
  summarise(children = list(children))




