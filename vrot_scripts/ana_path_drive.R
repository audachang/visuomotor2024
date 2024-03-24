# for loop use in ana_path.R

info1 <- getfns(droot1, sid)
info2 <- getfns(droot2, sid)




# tally all data
d_all <- tibble()
trial <- 1
d_all <- stack_data(info1$CONfns, d_all, 'CON', trial)

trial <- max(d_all$trial) + 1
d_all <- stack_data(info1$ADPfns, d_all, 'ADP', trial)

trial <- max(d_all$trial) + 1
d_all <- stack_data(info2$ADPfns, d_all, 'ADP', trial)

trial <- max(d_all$trial) + 1
d_all <- stack_data(info2$DADfns, d_all, 'DAD', trial)

temp <- tibble(trial = unique(d_all$trial)) %>%
  mutate(errblock = ntile(trial, 72),
         timeblock = ntile(trial, 12))

d_all <- d_all %>% left_join(temp, by = 'trial')



temp <- d_all %>% select(trial, inflag) %>%
  group_by(trial) %>%
  mutate(sample_id = row_number()) %>%
  group_by(trial, inflag) %>%
  summarise(min_seg = min(sample_id))

temp_count <- temp %>%
  count(trial) %>%
  filter(n  > 1) 

temp_w <- temp %>% 
  filter(trial %in% temp_count$trial) %>%
  pivot_wider(id_cols = trial, names_from =inflag, 
              names_prefix = "inflag_min_",
              values_from = min_seg)

# get the lag/ahead vectors of trace (xr, yr)    
d_all3 <- d_all %>%
  filter(trial %in% temp_count$trial) %>% 
  group_by(trial) %>%
  mutate(sample_id = row_number()) %>%
  left_join(temp_w, by = "trial") %>%
  filter(sample_id <= inflag_min_0) %>%
  mutate(xrl = lag(xr), yrl = lag(yr),
         xrf = lead(xr), yrf = lead(yr)) 

# compute angular difference between samples of the trace
d_all4 <- d_all3 %>% 
  rowwise() %>%
  mutate(diffangl = 
           comp_angle(
             c(xr,yr),
             c(xrl,yrl)),
         diffangf = 
           comp_angle(
             c(xr,yr),
             c(xrf,yrf)),
         distvl = dist(rbind(c(xr, xrl), c(yr, yrl))),
         distvf = dist(rbind(c(xr, xrf), c(yr, yrf)))
  ) %>%
  mutate(
    absdiffangl = abs(diffangl),
    absdiffangf = abs(diffangf) )

d_all4_summ <- d_all4 %>%
  select(trial, absdiffangl, absdiffangf)%>%
  group_by(trial) %>%
  summarise(
    max_absdiffang.l = max(absdiffangl, na.rm =T),
    max_absdiffang.f = max(absdiffangf, na.rm =T)
  )


#xrl, yrl lag vector
#xrf, yrf lead vector
d_all4_velocity.l <- d_all4 %>%
  group_by(trial) %>%
  slice(which.max(distvl)) %>%
  rowwise() %>%
  mutate(
    max_peak.l.end_angdiff =
      abs(comp_angle(
        c(xr,yr),
        c(xrl,yrl)))
    
  )


d_all4_velocity.f <- d_all4 %>%
  group_by(trial) %>%
  slice(which.max(distvf)) %>%
  rowwise() %>%
  mutate(
    max_peak.f.end_angdiff = 
      abs(comp_angle(
        c(xr,yr),
        c(xrf,yrf)))
  )

#using lag vector of peak vol to compute angular difference
good_curv_by_peak.l.vol <- d_all4_velocity.l %>%
  select(trial, max_peak.l.end_angdiff) %>%
  filter(max_peak.l.end_angdiff <=20)

#using lead vector of peak vol to compute angular difference
good_curv_by_peak.f.vol <- d_all4_velocity.f %>%
  select(trial, max_peak.f.end_angdiff) %>%
  filter(max_peak.f.end_angdiff <=20)


# using angular difference between every sample and the cross vector 
good_curv_sample2end <- d_all4_summ %>%
  filter(max_absdiffang.l <= 90) %>%
  select(trial)


d_all2 <- d_all4 %>%
  dplyr::filter(inflag == 1) %>%
  mutate(tarpos2 = factor(tarpos)) 
  #filter(trial %in% good_curv_sample2end$trial) 



f1 <- ggplot(d_all2, 
             aes(x=xr, y=yr, 
                 color = condition,
                 group = trial)) +
  geom_line(alpha = 0.3) +
  geom_point()+
  geom_point(data = tarpos_cords, 
             aes(x = x, y=y),
             color = 'black',
             size = 5)+
  scale_colour_discrete() 




# extract the crossing cooridnates and timing for each trial
d_summ <- d_all2 %>%
  select(trial, errblock, timeblock,tarpos, xrc, yrc, condition, t1, t3, t4) %>%
  group_by(trial, condition, errblock,timeblock) %>%
  summarize(tarpos = mean(tarpos, na.rm = T),
            xrc = mean(xrc, na.rm = T),
            yrc = mean(yrc, na.rm = T),
            t1 = mean(t1, na.rm =T), 
            t3 = mean(t3, na.rm=T),
            t4 = mean(t4, na.rm = T)) 


# transform target position code (tarpos) to coordnates (xrt, yrt)

d_summ2 <- d_summ %>%
  mutate(xrt = tarpos_cords$x[tarpos+1],
         yrt = tarpos_cords$y[tarpos+1])

# compute signed angle between target position and cross point
d_cross <- d_summ2 %>%
  rowwise() %>%
  mutate(angle = comp_angle(c(xrc,yrc),c(xrt,yrt))) %>%
  ungroup() %>%
  mutate(
         rt = (t3-t1)*1000,
         mt = (t4-t3)*1000)  %>%
  filter(mt <= 1500 & mt >= 150)

d_times <- d_summ2 %>%
  rowwise() %>%
  mutate(angle = comp_angle(c(xrc,yrc),c(xrt,yrt))) %>%
  ungroup() %>%
  mutate(
         rt = (t3-t1)*1000,
         mt = (t4-t3)*1000) %>%
  filter( mt <= 1500 & mt >= 150)

temp <- d_cross %>% group_by(condition) %>% 
  summarise(m = min(errblock), M = max(errblock))

print(temp)

d_cross_bk <- d_cross %>%
  group_by(errblock, condition) %>%
  summarise(angle = median(angle)) %>%
  mutate(sid = sid)


saveRDS(d_cross_bk, file = sprintf('processed/%s_error.RDS', sid))

d_times_bk <- d_times %>%
  group_by(timeblock, condition) %>%
  summarise(
    RT = median(rt),
    MT = median(mt)) %>%
  mutate(sid = sid)
saveRDS(d_times_bk, file = sprintf('processed/%s_times.RDS', sid))






xlabstr_time <- 'Blocks (24-trials)'
xlabstr_err <- 'Mini-blocks (4-trials)'

f_err <- ggplot(d_cross_bk, 
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

f_err
ggsave(paste('figures/errors/', 
       #      input$participant, '_error.png', sep=''),
       sid, '_error.png', sep=''),

              width = 1920, height= 1080, units = 'px', dpi =300 )



f_rt <- ggplot(d_times_bk, 
               aes(x = timeblock, y = RT, 
                   color = condition)) +
  geom_point()+ 
  ggtitle(sid) +
  ylab("RT(ms)") +
  xlab(xlabstr_time)

f_rt
ggsave(paste('figures/times/', sid, '_RT.png', sep=''),
       width = 1280, height=1024, units='px' )


f_mt <- ggplot(d_times_bk, 
               aes(x = timeblock, y = MT, 
                   color = condition)) +
  geom_point()+ 
  ggtitle(sid) +
  ylab("MT(ms)") +
  xlab(xlabstr_time)
f_mt
ggsave(paste('figures/times/', sid, '_MT.png', sep=''),
       width = 1280, height=1024, units='px' )


