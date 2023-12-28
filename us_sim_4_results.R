# ###################### SET UP ######################
if (!is.na(match("batch.simulation", ls(all.names = TRUE, envir = .GlobalEnv)))) {
  # do nothing on batch run
} else {
  # restart all variables
  rm(list = ls(all.names = TRUE, envir = .GlobalEnv))
  options(warn = -1)
}

# tipe-I error
alpha <- 0.05

# dir for storing results
res.dir <- "7 CSV"

# read batch simulation data from CSV file
simulation <-
  read.csv(file.path(res.dir, "batch_results.csv"), sep = ",")

# decriptive
print(paste("Levels:", names(simulation)[1]), quote = FALSE)
print(levels(as.factor(simulation$Kernel.sim)), quote = FALSE)
print(paste("Levels:", names(simulation)[2]), quote = FALSE)
print(levels(as.factor(simulation$Noise.bg)), quote = FALSE)
print(paste("Levels:", names(simulation)[3]), quote = FALSE)
print(levels(as.factor(simulation$Noise.obj)), quote = FALSE)
print(paste("Levels:", names(simulation)[4]), quote = FALSE)
print(levels(as.factor(simulation$Noise.path)), quote = FALSE)
print(paste("Levels:", names(simulation)[5]), quote = FALSE)
print(levels(as.factor(simulation$Filter.type.sim)), quote = FALSE)
print(paste("Levels:", names(simulation)[6]), quote = FALSE)
print(levels(as.factor(simulation$Filter.size.sim)), quote = FALSE)
print(paste("Levels:", names(simulation)[7]), quote = FALSE)
print(levels(as.factor(simulation$SNR)), quote = FALSE)
print(paste("Levels:", names(simulation)[8]), quote = FALSE)
print(levels(as.factor(simulation$Filter.type.track)), quote = FALSE)
print(paste("Levels:", names(simulation)[9]), quote = FALSE)
print(levels(as.factor(simulation$Filter.size.track)), quote = FALSE)
print(paste("Levels:", names(simulation)[10]), quote = FALSE)
print(levels(as.factor(simulation$Overlap.track)), quote = FALSE)
print(paste("Levels:", names(simulation)[11]), quote = FALSE)
print(levels(as.factor(simulation$Jump)), quote = FALSE)
print("", quote = FALSE)

dev.new()
layout(matrix(
  c(1, 2, 3, 4, 5, 6, 7, 8),
  nrow = 2,
  ncol = 4,
  byrow = TRUE
))

# model analysis
fit.R.sqr.X <-
  lm(
    simulation$R.sqr.X ~ simulation$Kernel.sim + simulation$Noise.bg + simulation$Noise.obj + simulation$Noise.path + simulation$Filter.type.sim + simulation$Filter.size.sim + simulation$SNR + simulation$Filter.type.track + simulation$Filter.size.track + simulation$Overlap.track + simulation$Jump
  )
print("R.sqr.X", quote = FALSE)
df1 <- coef(summary(fit.R.sqr.X))
df2 <- confint(fit.R.sqr.X, level = 0.95)
df <-
  round(cbind(df1[complete.cases(df1), 1], df2[complete.cases(df2),], df1[complete.cases(df1), 2:4]), digits = 3)
df <- as.data.frame(df)
colnames(df)[1] <- "Coef."
df[, 7] <- ifelse(df[, 6] < 0.05, "*", "")
df[, 7] <- ifelse(df[, 6] < 0.01, "**", "")
df[, 7] <- ifelse(df[, 6] < 0.001, "***", "")
colnames(df)[7] <- "Sig."
print(df, quote = FALSE)
# print(anova(fit.R.sqr.X), quote = FALSE)
print("", quote = FALSE)
hist(simulation$R.sqr.X)

fit.R.sqr.Y <-
  lm(
    simulation$R.sqr.Y ~ simulation$Kernel.sim + simulation$Noise.bg + simulation$Noise.obj + simulation$Noise.path + simulation$Filter.type.sim + simulation$Filter.size.sim + simulation$SNR + simulation$Filter.type.track + simulation$Filter.size.track + simulation$Overlap.track + simulation$Jump
  )
print("R.sqr.Y", quote = FALSE)
df1 <- coef(summary(fit.R.sqr.Y))
df2 <- confint(fit.R.sqr.Y, level = 0.95)
df <-
  round(cbind(df1[complete.cases(df1), 1], df2[complete.cases(df2),], df1[complete.cases(df1), 2:4]), digits = 3)
df <- as.data.frame(df)
colnames(df)[1] <- "Coef."
df[, 7] <- ifelse(df[, 6] < 0.05, "*", "")
df[, 7] <- ifelse(df[, 6] < 0.01, "**", "")
df[, 7] <- ifelse(df[, 6] < 0.001, "***", "")
colnames(df)[7] <- "Sig."
print(df, quote = FALSE)
# print(anova(fit.R.sqr.Y), quote = FALSE)
print("", quote = FALSE)
hist(simulation$R.sqr.Y)

fit.Bias.X <-
  lm(
    simulation$Bias.X ~ simulation$Kernel.sim + simulation$Noise.bg + simulation$Noise.obj + simulation$Noise.path + simulation$Filter.type.sim + simulation$Filter.size.sim + simulation$SNR + simulation$Filter.type.track + simulation$Filter.size.track + simulation$Overlap.track + simulation$Jump
  )
print("Bias.X", quote = FALSE)
df1 <- coef(summary(fit.Bias.X))
df2 <- confint(fit.Bias.X, level = 0.95)
df <-
  round(cbind(df1[complete.cases(df1), 1], df2[complete.cases(df2),], df1[complete.cases(df1), 2:4]), digits = 3)
df <- as.data.frame(df)
colnames(df)[1] <- "Coef."
df[, 7] <- ifelse(df[, 6] < 0.05, "*", "")
df[, 7] <- ifelse(df[, 6] < 0.01, "**", "")
df[, 7] <- ifelse(df[, 6] < 0.001, "***", "")
colnames(df)[7] <- "Sig."
print(df, quote = FALSE)
# print(anova(fit.Bias.X), quote = FALSE)
print("", quote = FALSE)
hist(simulation$Bias.X)

fit.Bias.Y <-
  lm(
    simulation$Bias.Y ~ simulation$Kernel.sim + simulation$Noise.bg + simulation$Noise.obj + simulation$Noise.path + simulation$Filter.type.sim + simulation$Filter.size.sim + simulation$SNR + simulation$Filter.type.track + simulation$Filter.size.track + simulation$Overlap.track + simulation$Jump
  )
print("Bias.Y", quote = FALSE)
df1 <- coef(summary(fit.Bias.Y))
df2 <- confint(fit.Bias.Y, level = 0.95)
df <-
  round(cbind(df1[complete.cases(df1), 1], df2[complete.cases(df2),], df1[complete.cases(df1), 2:4]), digits = 3)
df <- as.data.frame(df)
colnames(df)[1] <- "Coef."
df[, 7] <- ifelse(df[, 6] < 0.05, "*", "")
df[, 7] <- ifelse(df[, 6] < 0.01, "**", "")
df[, 7] <- ifelse(df[, 6] < 0.001, "***", "")
colnames(df)[7] <- "Sig."
print(df, quote = FALSE)
# print(anova(fit.Bias.Y), quote = FALSE)
print("", quote = FALSE)
hist(simulation$Bias.Y)

fit.AVG.MaxCrossCorr <-
  lm(
    simulation$AVG.MaxCrossCorr ~ simulation$Kernel.sim + simulation$Noise.bg + simulation$Noise.obj + simulation$Noise.path + simulation$Filter.type.sim + simulation$Filter.size.sim + simulation$SNR + simulation$Filter.type.track + simulation$Filter.size.track + simulation$Overlap.track + simulation$Jump
  )
print("AVG.MaxCrossCorr", quote = FALSE)
df1 <- coef(summary(fit.AVG.MaxCrossCorr))
df2 <- confint(fit.AVG.MaxCrossCorr, level = 0.95)
df <-
  round(cbind(df1[complete.cases(df1), 1], df2[complete.cases(df2),], df1[complete.cases(df1), 2:4]), digits = 3)
df <- as.data.frame(df)
colnames(df)[1] <- "Coef."
df[, 7] <- ifelse(df[, 6] < 0.05, "*", "")
df[, 7] <- ifelse(df[, 6] < 0.01, "**", "")
df[, 7] <- ifelse(df[, 6] < 0.001, "***", "")
colnames(df)[7] <- "Sig."
print(df, quote = FALSE)
# print(anova(fit.AVG.MaxCrossCorr), quote = FALSE)
print("", quote = FALSE)
hist(simulation$AVG.MaxCrossCorr)

fit.SD.MaxCrossCorr <-
  lm(
    simulation$SD.MaxCrossCorr ~ simulation$Kernel.sim + simulation$Noise.bg + simulation$Noise.obj + simulation$Noise.path + simulation$Filter.type.sim + simulation$Filter.size.sim + simulation$SNR + simulation$Filter.type.track + simulation$Filter.size.track + simulation$Overlap.track + simulation$Jump
  )
print("SD.MaxCrossCorr", quote = FALSE)
df1 <- coef(summary(fit.SD.MaxCrossCorr))
df2 <- confint(fit.SD.MaxCrossCorr, level = 0.95)
df <-
  round(cbind(df1[complete.cases(df1), 1], df2[complete.cases(df2),], df1[complete.cases(df1), 2:4]), digits = 3)
df <- as.data.frame(df)
colnames(df)[1] <- "Coef."
df[, 7] <- ifelse(df[, 6] < 0.05, "*", "")
df[, 7] <- ifelse(df[, 6] < 0.01, "**", "")
df[, 7] <- ifelse(df[, 6] < 0.001, "***", "")
colnames(df)[7] <- "Sig."
print(df, quote = FALSE)
# print(anova(fit.SD.MaxCrossCorr), quote = FALSE)
print("", quote = FALSE)
hist(simulation$SD.MaxCrossCorr)

fit.AVG.Shifts <-
  lm(
    simulation$AVG.Shifts ~ simulation$Kernel.sim + simulation$Noise.bg + simulation$Noise.obj + simulation$Noise.path + simulation$Filter.type.sim + simulation$Filter.size.sim + simulation$SNR + simulation$Filter.type.track + simulation$Filter.size.track + simulation$Overlap.track + simulation$Jump
  )
print("AVG.Shifts", quote = FALSE)
df1 <- coef(summary(fit.AVG.Shifts))
df2 <- confint(fit.AVG.Shifts, level = 0.95)
df <-
  round(cbind(df1[complete.cases(df1), 1], df2[complete.cases(df2),], df1[complete.cases(df1), 2:4]), digits = 3)
df <- as.data.frame(df)
colnames(df)[1] <- "Coef."
df[, 7] <- ifelse(df[, 6] < 0.05, "*", "")
df[, 7] <- ifelse(df[, 6] < 0.01, "**", "")
df[, 7] <- ifelse(df[, 6] < 0.001, "***", "")
colnames(df)[7] <- "Sig."
print(df, quote = FALSE)
# print(anova(fit.AVG.Shifts), quote = FALSE)
print("", quote = FALSE)
hist(simulation$AVG.Shifts)

fit.SD.Shifts <-
  lm(
    simulation$SD.Shifts ~ simulation$Kernel.sim + simulation$Noise.bg + simulation$Noise.obj + simulation$Noise.path + simulation$Filter.type.sim + simulation$Filter.size.sim + simulation$SNR + simulation$Filter.type.track + simulation$Filter.size.track + simulation$Overlap.track + simulation$Jump
  )
print("SD.Shifts", quote = FALSE)
df1 <- coef(summary(fit.SD.Shifts))
df2 <- confint(fit.SD.Shifts, level = 0.95)
df <-
  round(cbind(df1[complete.cases(df1), 1], df2[complete.cases(df2),], df1[complete.cases(df1), 2:4]), digits = 3)
df <- as.data.frame(df)
colnames(df)[1] <- "Coef."
df[, 7] <- ifelse(df[, 6] < 0.05, "*", "")
df[, 7] <- ifelse(df[, 6] < 0.01, "**", "")
df[, 7] <- ifelse(df[, 6] < 0.001, "***", "")
colnames(df)[7] <- "Sig."
print(df, quote = FALSE)
# print(anova(fit.SD.Shifts), quote = FALSE)
print("", quote = FALSE)
hist(simulation$SD.Shifts)
