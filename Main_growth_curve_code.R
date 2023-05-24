library(growthcurver)
library(readr)
library(openxlsx)
library(dplyr)
library(ggplot2)
library('reshape2')
library(ggforce)

# Get file path for growth data and file correlating treatment to well
growthdata <- "/FILE_PATH.csv"
treatment_data <- "/data_table_correlating_treatment_to_well.csv"

# Import data
d <- read.table(growthdata,              
                header = TRUE, 
                sep = ",",
                stringsAsFactors = FALSE)

treatment_well <- read.table(treatment_data,              
                             sep = ",",
                             stringsAsFactors = FALSE)

# Analyze Growth curve
gc_out <- SummarizeGrowthByPlate(d)

# Make a copy of analyze growth data for manipulation
pca_gc_out <- as_data_frame(gc_out)

# Make sure imported treatment data has no column name (only V1, V2,...)
# Turn treatment data into a longer format for manipulation
# Treatment names on 1st column, correlating wells on column 2 onward
long_treatment_well <- treatment_well %>% melt(., 
                                                 id.vars = "V1", 
                                                 variable.name = "N/A", 
                                                 value.name = "well")

# Create a function to apply a treatment to well ID
get_treatment <- function(a) {
  treatment = long_treatment_well %>% filter(well==a) %>% select(V1)
  return(treatment) }

# Make a loop
# Go through each row of analyzed growth data
# Obtain well ID, then use the created function to apply appropriate treatment
# Store in a list
col1 <- c()

for (i in 1:nrow(pca_gc_out)) {
  wellID <- toString(pca_gc_out[i,1]) 
  col1 <- append(col1,get_treatment(wellID)) }

# Store that list into a new column on analyzed growth data table
pca_gc_out <- pca_gc_out %>% mutate(Treatment = col1)

# Perform PCA 
pca.res <- prcomp(pca_gc_out %>% select(k:sigma), 
                    center=TRUE, 
                    scale=TRUE)

# Change order of treatments (to make plot pretty)
pca_gc_out$Treatment <- factor(pca_gc_out$Treatment, 
                               levels = c("LB Blank", 
                                          "50 mg/mL FA103 in LB",
                                          "25 mg/mL FA103 in LB",
                                          "12.5 mg/mL FA103 in LB",
                                          "6.25 mg/mL FA103 in LB",
                                          "3.12 mg/mL FA103 in LB",
                                          "1.56 mg/mL FA103 in LB",
                                          "0.78 mg/mL FA103 in LB"))

# Plot PCA 
# On row "treatments = as.character(pca_gc_out$Treatment)", 
# might need to change into "treatments = pca_gc_out$Treatment"
# This may affect the order of items on figure legend
# Play around with the code
as_data_frame(list(PC1=pca.res$x[,1],
                   PC2=pca.res$x[,2],
                   samples = pca_gc_out$sample, 
                   treatments = as.character(pca_gc_out$Treatment))) %>% 
  ggplot(aes(x=PC1,y=PC2, label=samples, col=treatments)) + 
  scale_color_discrete(breaks = pca_gc_out$Treatment) +
  geom_text(size = 3) + 
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.background=element_blank(), axis.line=element_line(colour="black"))

# Export analyzed data to excel file
write.xlsx(pca_gc_out, "/PATH_TO_FILE.xlsx")

# PCA plot with shapes and regions (stat_ellipse)
as_data_frame(list(PC1=pca.res$x[,1],
                   PC2=pca.res$x[,2],
                   samples = pca_gc_out$sample, 
                   Treatments = as.character(pca_gc_out$Treatment))) %>% 
  ggplot(aes(x=PC1,y=PC2, shape=Treatments, fill=Treatments)) + 
  geom_point(size=3) + 
  stat_ellipse(geom="polygon", alpha=0.3) +
  scale_shape_manual(values = 0:7) + 
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.background=element_blank(), axis.line=element_line(colour="black"))

# PCA plot with geom_mark_ellipse, no points
as_data_frame(list(PC1=pca.res$x[,1],
                   PC2=pca.res$x[,2],
                   Treatments = pca_gc_out$Treatment)) %>% 
  ggplot(aes(x=PC1,y=PC2, fill=Treatments)) + 
  theme(legend.position="none") +
  geom_mark_ellipse(aes(label=Treatments), label.fontsize=13, con.cap=0, con.type="straight")+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
              panel.background=element_blank(), axis.line=element_line(colour="black"))

# PCA plot with shapes and regions (geom_mark_ellipse)
as_data_frame(list(PC1=pca.res$x[,1],
                   PC2=pca.res$x[,2],
                   Treatments = as.character(pca_gc_out$Treatment))) %>% 
  ggplot(aes(x=PC1,y=PC2, shape=Treatments, fill=Treatments)) + 
  geom_point(size=3) + 
  geom_mark_ellipse() +
  scale_shape_manual(values = 0:14) + 
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.background=element_blank(), axis.line=element_line(colour="black"))