library(EMD)
library(tidyverse)
library(reshape2)
library(forecast)
library(neuralnet)

# Data processing

EUA_euro = read_csv('EUA-ckz20.csv') %>%
  rename(date = Time,
         price_euro = Last)
n = nrow(EUA_euro)-1
EUA_euro <- EUA_euro[n:1,c(1,5)]
EUA_euro$date <- as.Date(EUA_euro$date, format = '%m/%d/%y')

euro_usd = read_csv('euro-dollar.csv') %>%
  rename(rate = value)
EUA_usd = merge(EUA_euro,euro_usd)
EUA_usd$price_usd = EUA_usd$price_euro*EUA_usd$rate

EUA = EUA_usd[,c(1,4)]
write.csv(EUA,'EUA.csv')
n = nrow(EUA)

ggplot(EUA, aes(date, price_usd)) +
  geom_line()

# EMD 

try <- emd(EUA$price_usd, EUA$date, boundary="none")
imfs = as.data.frame(try$im)
imfs = imfs %>% add_column(residue = try$residue,
                           date = EUA$date, price = EUA$price_usd) %>%
  rename(imf1 = V1,
         imf2 = V2,
         imf3 = V3,
         imf4 = V4,
         imf5 = V5)

imfs_adj = melt(subset(imfs,select=c(date,imf1,imf2,imf3,imf4,imf5,residue,price)),id.var="date")
ggplot(imfs_adj, aes(x = date, y = value)) +
  geom_line(aes(color = variable), show.legend = F) +
  facet_grid(variable ~ ., scales = "free_y")

# Fine-to-coarse reconstruction

zeros = rep(0,n)
s1_avg = imfs$imf1
t.test(zeros,s1_avg)
s2_avg = (imfs$imf1+imfs$imf2)/2
t.test(zeros,s2_avg)
s3_avg = (imfs$imf1+imfs$imf2+imfs$imf3)/3
t.test(zeros,s3_avg)
s4_avg = (imfs$imf1+imfs$imf2+imfs$imf3+imfs$imf4)/4
t.test(zeros,s4_avg)
s5_avg = (imfs$imf1+imfs$imf2+imfs$imf3+imfs$imf4+imfs$imf5)/5
t.test(zeros,s5_avg)

imfs$high_f = imfs$imf1 + imfs$imf2 + imfs$imf3
imfs$low_f = imfs$imf4 + imfs$imf5
imfs$trend = imfs$residue
ggplot(imfs, aes(x = date)) +
  geom_line(aes(y = high_f), linetype = 'dotted') +
  geom_line(aes(y = low_f), linetype = 'longdash') +
  geom_line(aes(y = trend), linetype = 'dashed') +
  geom_line(aes(y = price))

imfs_adj2 = melt(subset(imfs,select=c(date,high_f,low_f,trend,price)),id.var="date")
ggplot(imfs_adj2, aes(x = date, y = value)) +
  geom_line(aes(color = variable), show.legend = F) +
  facet_grid(variable ~ ., scales = "free_y")

# ARIMA prerdiction for low frequency layer

order = imfs$low_f %>% auto.arima(stationary=F, 
                                seasonal = T, 
                                test = c("kpss", "adf", "pp"),  
                                ic = c("aicc", "aic", "bic"), 
                                allowdrift = T, 
                                allowmean = T, 
                                approximation = T) %>%
  arimaorder()
fit = Arima(imfs$low_f, order = order, include.mean = T)
low_f_prediction = forecast(fit,h=100)
plot(low_f_prediction)
low_f_future = low_f_prediction$mean

# BP neural network prediction for trend layer
