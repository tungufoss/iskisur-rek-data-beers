library(kinship2)

family_tree <- family %>%
  filter(id > 0 & id < 200) %>%
  mutate(
    sex = ifelse(is.na(gender), 1, ifelse(gender == 'M', 1, 2)),
    dadid = ifelse(!is.na(dad), dad, ifelse(is.na(mom), 0, id + 1000)),
    momid = ifelse(!is.na(mom), mom, ifelse(is.na(dad), 0, id + 1000)),
    famid = 1,
    label = paste0(first_name, '\n',
                   ifelse(!is.na(birth), birth, ''),
                   ifelse(!is.na(birth) | !is.na(death), '-', ''),
                   ifelse(!is.na(death), death, '')),
  ) %>%
  select(id, sex, dadid, momid, famid, label, chosen_one)
family_tree <- family_tree %>%
  rbind(
    family_tree %>%
      filter(dadid > 1000) %>%
      mutate(id = dadid, sex = 1, dadid = 0, momid = 0, famid = 1, label = 'Unknown', chosen_one = NA),
    family_tree %>%
      filter(momid > 1000) %>%
      mutate(id = momid, sex = 2, dadid = 0, momid = 0, famid = 1, label = 'Unknown', chosen_one = NA)
  )

relation <- read.csv('marriage.csv') %>%
  rename(id1 = female, id2 = male, code = remark) %>%
  mutate(code = 4, famid = 1)

ped <- pedigree(id = family_tree$id,
                dadid = family_tree$dadid,
                momid = family_tree$momid,
                sex = family_tree$sex,
                relation = relation,
                affected = ifelse(is.na(family_tree$chosen_one), 0, 1)
)

#### plot pedigree
# ------------------------------------------------------------
# Open a pdf device
pdf('charts/family_tree.pdf', width = 12, height = 12)
# Plot with colors
plot(
  ped, cex = 0.6, id = family_tree$label,
  #status = ifelse(is.na(family_tree$death),1,0),
  col = ifelse(is.na(family_tree$chosen_one), 'black',
               ifelse(family_tree$chosen_one == 'Good', three_palette[1],
                      ifelse(family_tree$chosen_one == 'Evil',
                             three_palette[3], three_palette[2]))),
)

# Close the pdf device
dev.off()
