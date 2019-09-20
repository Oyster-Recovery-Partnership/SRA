# make plot for PEW presentation
# SRA
# how much shell is used in public fishery, aquaculture, and restoration

# load packages
library(dplyr)
library(ggplot2)
library(scales)

# directory
dir.out = "G:/presentations/PEW_Sept2019/plots/"

# since data is short just build matrix
dat = as.data.frame(cbind(c(2012:2018), 
                          c(5000, 5000, 10000, 10000, 20000, 20000, 20000),
                          c(50000, 125000, 110000, 210000, 300000, 190000, 125000),
                          c(48960, 63360, 55680, 75520, 114240, 111040, 59520)))
names(dat) = c("year","aquaculture","public","restoration")

t_dat = tidyr::gather(dat) %>% mutate(year = rep(c(2012:2018),4))
t_dat = t_dat[8:28,]

# plots
p = ggplot()+
  geom_histogram(data = filter(t_dat, key %in% "aquaculture"), aes(x = year, y = value, fill = key), stat="identity") + 
  scale_fill_manual(values = c("grey")) + 
  theme_bw() +
  theme(text = element_text(size = 20)) + 
  labs(x = "Year", y = "Bushels") + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7))+ 
  scale_y_continuous(labels = scales::comma, limits = c(0, 434240))
p
ggsave(paste(dir.out, "aquaculture.png", sep=""),p)

p = ggplot()+
  geom_histogram(data = filter(t_dat, !key %in% "restoration"), aes(x = year, y = value, fill = key), stat="identity",
                 position = position_stack(reverse = TRUE)) + 
  scale_fill_manual(values = c("grey","green2")) + 
  theme_bw() +
  theme(text = element_text(size = 20)) + 
  labs(x = "Year", y = "Bushels") + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7))+ 
  scale_y_continuous(labels = scales::comma, limits = c(0, 434240))
p
ggsave(paste(dir.out, "aquaculture_and_public.png", sep=""),p)

p = ggplot()+geom_histogram(data = t_dat, aes(x = year, y = value, fill = key), stat="identity",
                            position = position_stack(reverse = TRUE)) + 
  scale_fill_manual(values = c("grey","green2","blue3")) + 
  theme_bw() +
  theme(text = element_text(size = 20)) + 
  labs(x = "Year", y = "Bushels") + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) + 
  scale_y_continuous(labels = scales::comma)
p
ggsave(paste(dir.out, "all.png", sep=""),p)
