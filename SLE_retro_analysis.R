#---------Importing packages-------------

library("xlsx")      #Reading and writing excel files
library("Hmisc")     #describe() function
library("VIM")       #Visualization and Imputation of Missing data. Provides KNN-imputation function
library("mice")      #Multiple Imputation by Chain Equations. Provides multiple imputation function
library("Amelia")    #Visualization of missing data
library("naniar")    #Little's test for MCAR
library("ggplot2")   #Making graphs
library("corrplot")
library("ggcorrplot")
library("RColorBrewer")
#----------------------------------------

#------------------------Loading and preprocessing data -----------------------------------------------------------

# #Load the data from the Retrospective SLE study and remove potential ghost columns
# retro_sle = read.xlsx("SLE_retro.xlsx", sheetName = "SLE")
# retro_sle <- retro_sle[, !grepl("^NA", names(retro_sle))]
# retro_dc = read.xlsx("SLE_retro.xlsx", sheetName = "DC")
# retro_dc <- retro_dc[, !grepl("^NA", names(retro_dc))]
# retro_hc = read.xlsx("SLE_retro.xlsx", sheetName = "HC")
# retro_hc <- retro_hc[, !grepl("^NA", names(retro_hc))]

#Load the data from the Retrospective SLE study as one dataframe
df_retro <- read.xlsx("SLE_retro.xlsx", sheetName = "all_samples")

#Extract meta data
n_col <- ncol(df_retro)
n_row <- nrow(df_retro)

#Convert categorical variable to factor
df_retro$DiseaseState <- as.factor(df_retro$DiseaseState)

#Dealing with out of range values-----------------------------------------------
## For each column: convert >max into max-value, convert <min into min-value/2
num_colnames <- colnames(df_retro)[3:length(colnames(df_retro))]
for (col in num_colnames){
  min <- min(as.numeric(df_retro[[col]]), na.rm = TRUE)
  max <- max(as.numeric(df_retro[[col]]), na.rm = TRUE)
  indices_too_low <- which(df_retro[[col]] == "< Min")
  indices_too_high <- which(df_retro[[col]]=="NaN")
  df_retro[[col]] <- replace(df_retro[[col]], indices_too_low, min)
  df_retro[[col]] <- replace(df_retro[[col]], indices_too_high, max)
  df_retro[[col]] <- as.numeric(df_retro[[col]])
}
# # Try-out on ELISA.ORGENTEC column
# nan_indices <- which(df_retro$ELISA.ORGENTEC=="NaN")
# df_retro$ELISA.ORGENTEC <- replace(df_retro$ELISA.ORGENTEC, nan_indices, 6667)
# min_indices <- which(df_retro$ELISA.ORGENTEC=="< Min")
# df_retro$ELISA.ORGENTEC <- replace(df_retro$ELISA.ORGENTEC, min_indices, 9.8/2)

#Dealing with missing data------------------------------------------------------
#Get the amount of NAs per column
for (col in colnames(df_retro)){
  nas <- sum(is.na(df_retro[[col]]))
  nas_prc <- paste0(" (", round(nas/nrow(df_retro)*100, 2), "%)")
  print(paste0("Number of NAs in ", col, ": ", nas, nas_prc))
}

#Impute NAs
df_retro_omit <- na.omit(df_retro)
df_retro_k5 <- kNN(df_retro, k=5)[1:n_col]
df_retro_k10 <- kNN(df_retro, k=10)[1:n_col]
df_retro_k150 <- kNN(df_retro, k=150)[1:n_col]
df_retro_mice <- mice(df_retro, m=5)
df_retro_mice1 <- complete(df_retro_mice, 1)
df_retro_mice2 <- complete(df_retro_mice, 2)
#Graphs
ggplot(df_retro, aes(x=ELISA.ORGENTEC, fill="Original")) +
  geom_density(alpha = 0.5) +
  geom_density(data = df_retro_k5, aes(x=ELISA.ORGENTEC, fill = "Imputed", alpha = 0.5)) +
  labs(title = "Density plot of ELISA.ORGENTEC: Original vs. Imputed")
par(mfrow=c(1,2))
hist(df_retro$ELISA.ORGENTEC, main = "Original ELISA.EUROIM", xlab = "ELISA.EUROIM", col = "blue")
hist(df_retro_k5$ELISA.ORGENTEC, main = "Imputed ELISA.EUROIM", xlab = "ELISA.EUROIM", col = "red")
#----------------------------------------------------------------------------------------------------------------

#------------------------Exploratory Data Analysis---------------------------------------------------------------
#Correlations
cor_matrix <- cor(df_retro_k5[3:n_col])
cor_plot <- corrplot(cor_matrix, method = "color")

#Dimensionality reduction
##Remove follow-up samples (T1-5)
followUpIndices <- grep("T[1-5]", df_retro_k5$StudieID)
df_retro_t0 <- df_retro_k5[-followUpIndices,]
##Multi-Dimensional Scaling
retro.pch <- df_retro_t0$DiseaseState
levels(retro.pch) <- list("0" = "SLE", "1" = "DC", "2" = "HC")
retro.pch <- as.numeric(as.character(retro.pch))
brewer.pal(3, "Set2")
retro.col <- df_retro_t0$DiseaseState
levels(retro.col) <- list("#66C2A5"="SLE", "#FC8D62" = "DC", "blue" = "HC" )
retro.col <- as.character(retro.col)
distances <- dist(df_retro_t0[,3:n_col])
fit <- cmdscale(distances, eig = TRUE, k = 2)
fit
par(mfrow=c(1,1))
plot(x=fit$points[,1], y=fit$points[,2], xlab = "Coordinate 1", ylab = "Coordinate 2",
     main = "MDS with euclidean distances", col = retro.col, cex = 0.7)
