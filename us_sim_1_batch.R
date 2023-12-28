# ###################### SET UP ######################

# restart all variables
rm(list = ls(all.names = TRUE, envir = .GlobalEnv))
options(warn = -1)

# ###################### BATCH SET-UP ######################

# object start position
# video (.mp4) file names
input <- "sim_video.mp4"
info <- av::av_media_info(inputfile)

# parameters for SIMULATION
kernel.range <- seq(from = 21, to = 25, by = 4)
noise.background.range <- seq(from = 0, to = 50, by = 50)
noise.object.range <- seq(from = 0, to = 50, by = 50)
noise.trajectory.range <- seq(from = 0, to = 5, by = 5)
filter.type.sim.range <- c("none", "mean", "median")
filter.size.sim.range <- seq(from = 0, to = 5, by = 5)

# parameters for TRACKING
filter.type.track.range <- c("none", "mean", "median")
filter.size.track.range <- seq(from = 0, to = 5, by = 5)
overlap.track.range <- seq(from = 25, to = 50, by = 25)
jump.range <- seq(from = 0, to = 1, by = 1)

# calculate signal-to-noise-ratio
SNR <- 1

n.sim <-
  length(kernel.range) * length(noise.background.range) * length(noise.object.range) * length(noise.trajectory.range) * length(filter.type.sim.range) *
  length(filter.size.sim.range) * length(SNR) * length(filter.type.track.range) * length(filter.size.track.range) * length(overlap.track.range) * length(jump.range)

print(paste0("Number of simulations: ", n.sim), quote = FALSE)

# ###################### BATCH ANALYSIS ######################

assign("batch.simulation", TRUE, envir = .GlobalEnv)

labels <-
  c(
    "Kernel.sim",
    "Noise.bg",
    "Noise.obj",
    "Noise.path",
    "Filter.type.sim",
    "Filter.size.sim",
    "SNR",
    "Filter.type.track",
    "Filter.size.track",
    "Overlap.track",
    "Jump",
    "R-sqr X",
    "R-sqr Y",
    "Bias X",
    "Bias Y",
    "AVG MaxCrossCorr",
    "SD MaxCrossCorr",
    "AVG Shifts",
    "SD Shifts"
  )
batch.res <-
  as.data.frame(matrix(
    NA,
    nrow = 0,
    ncol = length(labels),
    dimnames = list(c(), labels)
  ))
sim.config <-
  as.data.frame(matrix(
    NA,
    nrow = 1,
    ncol = length(labels[1:7]),
    dimnames = list(c(), labels[1:7])
  ))
track.config <-
  as.data.frame(matrix(
    NA,
    nrow = 1,
    ncol = length(labels[8:11]),
    dimnames = list(c(), labels[8:11])
  ))
track.res <-
  as.data.frame(matrix(
    NA,
    nrow = 1,
    ncol = length(labels[16:19]),
    dimnames = list(c(), labels[16:19])
  ))

for (a in 1:length(kernel.range)) {
  for (b in 1:length(noise.background.range)) {
    for (c in 1:length(noise.object.range)) {
      for (d in 1:length(noise.trajectory.range)) {
        for (e in 1:length(filter.type.sim.range)) {
          for (f in 1:length(filter.size.sim.range)) {
            # lowest level of simulation config
            sim.config <-
              cbind(
                kernel.range[a],
                noise.background.range[b],
                noise.object.range[c],
                noise.trajectory.range[d],
                filter.type.sim.range[e],
                filter.size.sim.range[f],
                SNR
              )
            source("us_sim_2_video.R", local = TRUE)
            for (g in 1:length(filter.type.track.range)) {
              for (h in 1:length(filter.size.track.range)) {
                for (k in 1:length(overlap.track.range)) {
                  for (l in 1:length(jump.range)) {
                    track.config <-
                      cbind(
                        filter.type.track.range[g],
                        filter.size.track.range[h],
                        overlap.track.range[k],
                        jump.range[l]
                      )
                    source("us_track.R", local = TRUE)
                    track.res <-
                      cbind(mean(na.omit(
                        unlist(max.crosscorrel)
                      )),
                      sd(na.omit(
                        unlist(max.crosscorrel)
                      )),
                      mean(na.omit(unlist(
                        shifts.all
                      ))),
                      sd(na.omit(unlist(
                        shifts.all
                      ))))
                    source("us_sim_3_validity.R", local = TRUE)
                    batch.res[nrow(batch.res) + 1,] <-
                      c(sim.config,
                        track.config,
                        loa.results,
                        track.res)
                    write.csv(
                      batch.res,
                      file.path(res.dir, "batch_results.csv"),
                      row.names = FALSE,
                      col.names = TRUE
                    )
                    print(batch.res[nrow(batch.res),], quote = FALSE)
                    print("", quote = FALSE)
                    print(paste0(
                      "Completed: ",
                      (nrow(batch.res) + 1),
                      " of ",
                      n.sim,
                      " (",
                      round((nrow(
                        batch.res
                      ) + 1) / n.sim * 100,
                      0),
                      "%)"
                    ),
                    quote = FALSE)
                    print("", quote = FALSE)
                    print("", quote = FALSE)
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

# reset memory
rm(batch.simulation)

# show results
print(batch.res, quote = FALSE)
print("Finally ;)", quote = FALSE)
