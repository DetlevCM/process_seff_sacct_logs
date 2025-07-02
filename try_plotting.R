
# try plotting

data <- read.csv("all_data.csv", header = TRUE)

Users <- unique(data$User)

# size in pixels
height <- 600
width <- 800

# data2 is only required for type="combined"
Make_Plot <- function(data, user, type, width, height, data2) {

  png(file.path("png", type, paste(user, "_cpu_eff.png", sep = "")), width, height)

  plot(
    NULL,
    type = "n",
    xlim = c(0, length(data)),
    ylim = c(0, min(max(data), 100)),
    xlab = "", ylab = "",
    main = user,
    sub = paste(length(data), "jobs", sep = " "))

  # type empty for combined plot
  if (type == "combined") {
    points(data, pch = 1, col = 2)
    points(data2, pch = 4, col = 4)

    legend("topleft",
      c("cpu", "memory"),
      pch = c(1, 4),
      col = c(2, 4)
    )
    type <- "" # for the axis
  } else {
    points(data, pch = 4)
  }

  ### y-axis
  mtext(paste(type, "efficiency", sep = " "), side = 2, line = 2.75)

  ### x-axis
  mtext("job progression", side = 1, line = 2.5)

  dev.off()
}

# check subdirectory exists; delete if yes
if (dir.exists("png")) {
  unlink("png", recursive = TRUE)
}
# make subdirectory:
# https://stackoverflow.com/questions/51044405/create-subdirectories-inside-a-directory-at-once-in-r
dir.create(file.path(getwd(), "png"), recursive = TRUE)
dir.create(file.path(getwd(), "png/cpu"), recursive = TRUE)
dir.create(file.path(getwd(), "png/mem"), recursive = TRUE)
dir.create(file.path(getwd(), "png/combined"), recursive = TRUE)
dir.create(file.path(getwd(), "png/data"), recursive = TRUE)

for (u in Users) {
  jobs <- subset(data, data$User == u)

  Make_Plot(jobs$cpu_eff, u, "cpu", width, height)

  Make_Plot(jobs$mem_eff, u, "mem", width, height)

  Make_Plot(jobs$cpu_eff, u, "combined", width, height, jobs$mem_eff)

  write.csv(jobs, file = file.path("png", "data", paste(u, "_data.csv", sep = "")), row.names = FALSE)
}
