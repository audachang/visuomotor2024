

xlabstr_time <- 'Blocks (24-trials)'
xlabstr_err <- 'Mini-blocks (4-trials)'

ggplot(d_cross_bk(), 
                aes(x = errblock, y = angle, 
                    color = condition,
                    group = condition)) +
  geom_point() +
  stat_smooth(method="lm", 
              formula= y ~ x, se=FALSE,
              linetype = 1)+
  stat_smooth(method="lm", 
              formula= y ~ log(x), se=FALSE,
              linetype = 2, color='black')+
  ggtitle(sid) +
  ylab("angular error (deg)") +
  xlab(xlabstr_err) +
  theme(legend.position="bottom")+
  scale_x_continuous(breaks=seq(1,73, 12))

ggsave(paste('figures/errors/', sid, '_error.png', sep=''),
       width = 1920, height= 1080, units = 'px', dpi =300 )
