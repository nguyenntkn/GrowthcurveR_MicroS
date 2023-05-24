# Get the right rows spanning 2-4 hours
# Change row numbers depending on time intervals
d2to4 <- d[8:16,]
gc_out2to4 <- SummarizeGrowthByPlate(d2to4)
pca_gc_out2to4 <- as_data_frame(gc_out2to4)

col1 <- c()

for (i in 1:nrow(pca_gc_out2to4)) {
  wellID <- toString(pca_gc_out2to4[i,1]) 
  col1 <- append(col1,get_treatment(wellID)) }

pca_gc_out2to4 <- pca_gc_out2to4 %>% mutate(Treatment = col1)

# Perform PCA 
pca2to4.res <- prcomp(pca_gc_out2to4 %>% select(k:sigma), 
                   center=TRUE, 
                   scale=TRUE)

# Plot PCA 
# On row "treatments = as.character(pca_gc_out$Treatment)", 
# might need to change into "treatments = pca_gc_out$Treatment"
# This may affect the order of items on figure legend
# Play around with the code
# PCA plot with shapes and regions (geom_mark_ellipse)
as_data_frame(list(PC1=pca2to4.res$x[,1],
                   PC2=pca2to4.res$x[,2],
                   Treatments = as.character(pca_gc_out2to4$Treatment))) %>% 
  ggplot(aes(x=PC1,y=PC2, shape=Treatments, fill=Treatments)) + 
  geom_point(size=3) + 
  geom_mark_ellipse() +
  scale_shape_manual(values = 0:14) + 
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.background=element_blank(), axis.line=element_line(colour="black"))

# Export analyzed data to excel file
write.xlsx(pca_gc_out2to4, "/PATH_TO_FILE.xlsx")
