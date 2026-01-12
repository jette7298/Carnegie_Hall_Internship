

set.seed(123)   # for reproducibility

random_subjects <- acc_all_data |>
  distinct(subject_path) |>
  slice_sample(n = 10)

multi_users <- acc_all_data |>
  semi_join(random_subjects, by = "subject_path")


#graphs
multi_users |>
  ggplot() +
  geom_histogram(aes(x), bins = 200, fill = 1, alpha = 0.4) +
  geom_histogram(aes(y), bins = 200, fill = 2, alpha = 0.4) +
  geom_histogram(aes(z), bins = 200, fill = 4, alpha = 0.4) +
  facet_wrap(~ subject_path, ncol = 4, scales = "free_y") +
  coord_cartesian(xlim = c(-20, 20)) +
  theme_minimal() +
  theme(panel.spacing = unit(0.5, "lines")) +
  labs(title = "Acceleration distributions (x, y, z) across 10 random subjects",
       x = "Acceleration", y = "Count")

