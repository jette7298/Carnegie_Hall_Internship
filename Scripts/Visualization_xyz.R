
library(tidyverse)
library(plotly)
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

#energy over time
acc_all_data <- acc_all_data |>
  mutate(magnitude = sqrt(x^2 + y^2 + z^2))

multi_users <- acc_all_data |>
  semi_join(random_subjects, by = "subject_path") |>
  mutate(magnitude = sqrt(x^2 + y^2 + z^2))

multi_users |>
  ggplot(aes(x = timestamp_ms, y = magnitude)) +
  geom_line(alpha = 0.6) +
  facet_wrap(~ subject_path, scales = "free_y") +
  labs(title = "Movement energy over time",
       x = "Time (ms)", y = "Acceleration magnitude")


# 3D-Plot


one <- all_data |>
  filter(subject_path == "April 1/Session 1/1990") |>
  arrange(timestamp_ms) |>
  mutate(t = (timestamp_ms - first(timestamp_ms)) / 1000)

one_win <- one |> slice(1:5000)

plot_ly(one_win,
        x = ~x, y = ~y, z = ~z,
        type = "scatter3d",
        mode = "lines",
        text = ~paste0("t=", round(t, 2), "s"),
        hoverinfo = "text") %>%
  plotly::layout(
    scene = list(
      xaxis = list(title = "x"),
      yaxis = list(title = "y"),
      zaxis = list(title = "z")
    ),
    title = "3D acceleration trajectory (x, y, z)"
  )


#or like this


# 2) fast-forward: keep every k-th sample (increase step = faster)
step <- 20
one_ff <- one |>
  slice(seq(1, n(), by = step)) |>
  mutate(frame = row_number())

# 3) moving dot animation
plot_ly(
  data  = one_ff,
  x     = ~x, y = ~y, z = ~z,
  frame = ~frame,
  type  = "scatter3d",
  mode  = "markers",
  marker = list(size = 4),
  text = ~paste0("t = ", round(t, 2), "s"),
  hoverinfo = "text"
) %>%
  plotly::layout(
    title = "Moving dot through 3D acceleration space",
    scene = list(
      xaxis = list(title = "x"),
      yaxis = list(title = "y"),
      zaxis = list(title = "z")
    )
  ) %>%
  plotly::animation_opts(
    frame = 20,   # ms per frame (smaller = faster)
    transition = 0,
    redraw = TRUE
  ) %>%
  plotly::animation_button(x = 0.05, y = 0.05) %>%
  plotly::animation_slider(currentvalue = list(prefix = "Frame: "))
