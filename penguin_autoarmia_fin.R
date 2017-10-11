###
### Random Walk of the Penguins (8th place solution)
### halstead.jack@gmail.com
###

### init
rm(list = ls())
set.seed(1)
library(forecast)

### read
format = read.csv('D:\\Dropbox\\DrivenData\\penguin\\submission_format.csv')
nest = read.csv('D:\\Dropbox\\DrivenData\\penguin\\training_set_nest_counts.csv')

### transpose 
t.nest = reshape(nest, 
                 idvar = c('site_id', 'common_name'), 
                 direction = 'long', varying = names(nest)[3:57], v.names = 'n', 
                 timevar = 'year', times = substr(names(nest)[3:57], 2, 5))

### create temp index
t.nest$site_name = do.call(paste, c(t.nest[c('site_id', 'common_name')], sep = '-'))
format$site_name = do.call(paste, c(format[c('site_id', 'common_name')], sep = '-'))
t.nest = t.nest[order(t.nest$site_name), ]
rownames(t.nest) = NULL

### model forecast::auto.arima
u.site_name = unique(t.nest$site_name)
for (i in u.site_name) {
  print(i)
  temp = ts(subset(t.nest, t.nest$site_name ==  i & t.nest$year >= 1980)$n)
  if (sum(!is.na(temp))==0) {
    format[format$site_name == i, 3:6] = rep(0, 4)
  }
  else {
    temp.fill = na.locf(na.locf(temp, na.rm = F), fromLast = T)
    fit.aa = auto.arima(temp.fill)
    aa.fc = forecast(fit.aa, h = 4)
    format[format$site_name == i, 3:6] = round(aa.fc$mean, 1)
  }
}

### submit
format[, 3:6][format[, 3:6] < 0] = 0
format$site_name = NULL
colnames(format) = c('site_id','common_name','2014','2015','2016','2017')
write.csv(format, 'D:\\...\\auto_arima_fin.csv', row.names = F)
