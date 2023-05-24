> library('reshape2')


# Plot multiple data sets on one plot
> G1to5 %>% melt(., 
                 id.vars = "time", 
                 variable.name = "wells", 
                 value.name = "OD") %>% 
            ggplot(., aes(x = time, y = OD)) + 
            geom_point(aes(color = wells)) + 
            scale_y_log10()

#Check how to melt treatment_well1

# Make function to get treatment based on well ID
> get_treatment <- function(d) {
  treatment = long_treatment_well1 %>% filter(well==d) %>% select(X1)
  return(treatment) }

> col1 <- c()

> for (i in 1:nrow(copy_pca_gc_out)) {
  wellID <- toString(copy_pca_gc_out[i,1]) 
  col1 <- append(col1,get_treatment(wellID))
  }

> copy_pca_gc_out <- copy_pca_gc_out %>% mutate(Treatment = col1)

> copy_pca.res <- prcomp(copy_pca_gc_out %>% select(k:sigma), 
                         center=TRUE, 
                         scale=TRUE)

> as_data_frame(list(PC1=copy_pca.res$x[,1],
                     PC2=copy_pca.res$x[,2],
                     samples = copy_pca_gc_out$sample, 
                     treatments = as.character(copy_pca_gc_out$Treatment))) %>% 
  ggplot(aes(x=PC1,y=PC2, label=samples, col=treatments)) + 
  geom_text(size = 3) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              panel.background = element_blank(), axis.line = element_line(colour = "black"))

c("LB Blank", "50 mg/mL FA103 in LB","25 mg/mL FA103 in LB","12.5 mg/mL FA103 in LB","6.25 mg/mL FA103 in LB","3.12 mg/mL FA103 in LB","1.56 mg/mL FA103 in LB","0.78 mg/mL FA103 in LB")

> as_data_frame(list(PC1=pca.res$x[,1],
                     PC2=pca.res$x[,2],
                     samples = pca_gc_out$sample, 
                     treatments = as.character(pca_gc_out_LB$Treatment))) %>% 
  ggplot(aes(x=PC1,y=PC2, label=samples, color=treatments)) + 
  scale_color_discrete(breaks = pca_gc_out_LB$Treatment) +
  geom_text(size = 3) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))







