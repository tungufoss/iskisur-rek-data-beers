iceland.names <-
  read_csv('../data/first_names_freq.csv', locale = locale(encoding = 'utf-8')) %>% mutate(type = 'first') %>%
    rbind(
      read_csv('../data/middle_names_freq.csv', locale = locale(encoding = 'utf-8')) %>% mutate(type = 'middle')
    ) %>%
    group_by(name,year) %>% summarise(count = sum(count)) %>% ungroup()

isfolk.names <- read_csv('../data/books.csv', locale = locale(encoding = 'iso-8859-1')) %>%
  mutate(characters = str_split(characters, ",\\s*|\\s+")) %>%
  unnest(characters) %>%
  rename(name = characters) %>%
  filter(str_length(name) > 2) %>%
  group_by(name) %>%
  summarise(is_year = min(is_year))

missing_names <- isfolk.names %>%
  filter(!name %in% iceland.names$name) %>%
  pull(name)

names <- isfolk.names %>% merge(iceland.names, by='name')

p1 <- names %>%
  ggplot(aes(x=year, y=count, color=name)) + geom_line() +
  labs(y='Frequency of First and Middle Names', x=NULL,
       title='Name Trends in Iceland') +
  theme(legend.position = 'bottom', legend.title = element_blank())

window_size <- 10  # lets consider the frequency of names 5 years before and after the book was published
name_impacted <-
  names %>%
  group_by(name) %>%
  filter(between(year, first(is_year) - window_size, first(is_year) + window_size)) %>%
  summarize(
    before = sum(count[year < is_year]),
    after = sum(count[year >= is_year])
  ) %>%
  rowwise() %>%
  mutate(
    p_value = chisq.test(c(before, after))$p.value
  )

# Only include names with a statistically significant difference
df <- names %>%
  inner_join(name_impacted, by = "name") %>%
  filter(p_value < 0.0001)
p2 <- df %>%
  ggplot(aes(x=year, y=count, color=name)) + geom_line() +
  geom_point(data=df %>% filter(is_year==year), size=4, shape=8) +
  labs(y='Frequency of First and Middle Names', x=NULL,
       title='Significant Difference in Frequency Post-Publication') +
  scale_x_continuous(limits = c(1975,1995), expand = c(0,0)) +
    theme(legend.position = 'bottom', legend.title = element_blank())+
  scale_color_manual(values = long_palette)
cowplot::plot_grid(p1, p2, ncol=2)
ggsave(filename = 'figures/iceland_names.png', width = 12, height = 6, units = 'in', dpi = 300)
name_impacted %>%
  filter(before==0 & after>0)
