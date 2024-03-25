
mtcars
subset_by_cyl <- function(cyl_val) {
  mtcars |> filter(cyl == cyl_val)
}

subset_by_cyl(cyl_val = 8)

subset_by_cyl(cyl_val = "8")


plot_mtscatter <- function(x_var, y_var) {
  ggplot(data = mtcars, aes(x = x_var, y = y_var)) +
    geom_point() +
    theme_minimal()
}
plot_mtscatter(x_var = "hp", y_var = "mpg")


sort_mtcars <- function(var_sort) {
  mtcars |> arrange(var_sort)
}
sort_mtcars(var_sort = "mpg")
