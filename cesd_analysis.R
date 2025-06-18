install.packages('gtsummary')

library(gtsummary)
library(dplyr)
library(tidyr)

#READ IN DATA
cesd <- read.csv('C:/Users/diksh/Downloads/Jul 19 CM Depression Combined dataset.csv')
cytokine <- read.csv('C:/Users/diksh/Downloads/rd_dataset_20190528.csv')

#DROP ROWS WITH NO TXGROUP
cesd <- cesd %>% drop_na(txgroup)

#DROP ROWS WITH NO CESD 12
cesd <- cesd %>% drop_na(cesd_12)

#MATCH ON COLUMN WITH ID
merged_df <- merge(cesd, cytokine, by="id")

#CREATE COLUMN FOR CESD SEVERITY
merged_df$cesd_12_categ <- with(merged_df, ifelse(cesd_12>25,2,
                                                  ifelse(cesd_12>15,1,0)))
#SUBSET TO SERTRALINE VS PLACEBO
merged_df <- subset(merged_df, merged_df$txgroup==1)

#CONVERTING ALL CYTOKINE COLUMNS TO NUMERIC
sapply(merged_df, class)
cols_num <- c("rlcmip1b_000", "rlcrantes_000", "rlcpdgfaa_000", "rlcil5_000", "rlcil3_000", "rlcil1ra_000", "rlcifnb_000", "rlcil8_000", "rlcmip3a_000")
merged_df[cols_num] <- sapply(merged_df[cols_num], as.numeric)

#CONSTRUCT SUMMARY TABLE FOR CYTOKINES LEVELS ACROSS DEPRESSION SEVERITY GROUPS
merged_df %>% tbl_summary(by=cesd_12_categ,include=c(colnames(merged_df)[44:87])) %>% add_p()

#Significant difference observed with il1a, il5, il12, il13, ifna

#EXAMINE CORRELATIONS AMONG CYTOKINES
cor(merged_df[44:87])

#There are some high correlations

#SUBSETTING JUST CYTOKINES FOR PCA
pca_subset <- merged_df[44:87]

#PCA
pca <- prcomp(pca_subset, scale=TRUE)

#CALCULATE AND VISUALIZE VARIANCE DUE TO EACH PC 
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100,1)

barplot(pca.var.per,main="Scree plot", xlab="Principal component")

#PC1, PC2, PC3 account for 70% of variance

#IDENTIFY CYTOKINES FOR PC
loading_scores <- pca$rotation[,1]
cytokine_scores <- abs(loading_scores)
cytokine_score_ranked <- sort(cytokine_scores, decreasing=TRUE)
cytokine_score_ranked

#All cytokines are included in PC1






