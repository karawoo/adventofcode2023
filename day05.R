library("readr")
library("stringr")
library("purrr")


dat_orig <- read_file("input05.txt") %>%
  str_split("\\n+")

dat_orig <- dat_orig[[1]][!dat_orig[[1]] == ""]

split_data <- function(x) {
  split_points <- which(c(1, diff(cumsum(str_detect(x, "map")))) == 1)
  dat_list <- split(x, cumsum(seq_along(x) %in% split_points))
  names(dat_list)[[1]] <- "seeds:"
  dat_list[[1]] <- str_remove(dat_list[[1]], "seeds: ")
  for (i in 2:length(dat_list)) {
    names(dat_list)[[i]] <- dat_list[[i]][[1]]
    dat_list[[i]] <- dat_list[[i]][-1]
  }
  dat_list
}

dat <- dat_orig %>%
  split_data() %>%
  map(str_split, " +") %>%
  map_depth(.depth = 2, as.numeric)

map_input_to_dest <- function(input, map) {
  result <- input
  for (i in map) {
    if (i[[2]] <= input && input <= i[[2]] + i[[3]]) {
      result <- i[[1]] + (input - i[[2]])
      break
    }
  }
  result
}

results <- vector(mode = "list", length = length(dat))

results[[1]] <- dat[[1]][[1]]

for (i in 2:length(dat)) {
  results[[i]] <- vapply(results[[i - 1]], map_input_to_dest, numeric(1), map = dat[[i]])
}

min(results[[length(results)]])


