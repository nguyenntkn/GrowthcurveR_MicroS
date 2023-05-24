ggplot(d_melt, aes(x=variable,y=X,fill = Percent_inhibition))+ 
  geom_tile() + 
  scale_fill_gradient(low="white",high="red",name="",limits=c(0,100)) +
  geom_text(aes(label=sprintf("%0.2f", round(Percent_inhibition, digits=2)))) +
  xlab("Sub fractions") + ylab("") +
  coord_fixed() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.background=element_blank(), axis.ticks=element_blank())

ggplot(dlong, aes(x=V1,y=V3,fill = V2))+ 
  geom_tile() + 
  scale_fill_gradient(low="white",high="red",name="",limits=c(0,100)) +
  geom_text(aes(label=sprintf("%0.2f", round(V2, digits=2)))) +
  xlab("Sub fractions") + ylab("") +
  coord_fixed() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.background=element_blank())