setwd('/home/konstantin/documents/projects/datajour_hack/imdb')
dev.off()
rm(list = ls())

imdb <- read.csv('imdb_data/imdb_rus_ratings.csv')
imdb <- imdb[imdb$n_rates > 10 & imdb$year >= 2004 & imdb$year <= 2016, ]
imdb_mean <- aggregate(rating~year, imdb, mean)
head(imdb_mean)
fk <- read.csv('fond_kino_150.tsv')
fk_mean <- aggregate(imdb_rating~year, fk, mean)
head(fk_mean)

cairo_pdf('IMDb_score.pdf', 7, 5)
par(mar=c(2,5,1,1))
plot(NULL, ylim=c(1, 10), xlim=c(2004, 2016), axes=F, ylab='IMDb score', xlab='')
abline(v=2004:2016, h=seq(1,10,1), col='gray80')
abline(v=2010, col='gray80', lwd=3)
lines(rating~year, imdb_mean, lwd=2)
points(imdb_rating~year, fk, pch=19, col='red')
lines(imdb_rating~year, fk_mean, lwd=2, lty=2, col='red')
Axis(side=1, labels=2004:2016, at=2004:2016, lwd=2)
Axis(side=2, labels=1:10, at=1:10, lwd=2)
legend('topleft', c('Средний рейтинг всех российских фильмов', 'Средний рейтинг фильмов с поддержкой ФК', 'Рейтинг фильмов с поддержкой ФК'),
       lty=c(1,2,NA),
       lwd=c(2,2,NA),
       pch=c(NA,NA,19),
       col=c('black','red','red'))
dev.off()

fk_clean <- fk[!is.na(fk$bt_usd_2010), ]
fk_clean <- fk_clean[!is.na(fk_clean$return_usd_2010), ]
fk_clean$col <- 'firebrick'
fk_clean$col[fk_clean$return_usd_2010 > fk_clean$bt_usd_2010] <- 'royalblue'

cairo_pdf('Profit.pdf', 7, 6)
par(mar=c(5,5,1,1))
plot(NULL, ylim=c(2, 8), xlim=c(2, 8), axes=F, xlab='Budget $, log10', ylab='Return $, log10')
abline(v=2:8, h=2:8, col='gray80')
lines(2:8, 2:8, lwd=2, col='gray50')
points(log10(return_usd_2010)~log10(bt_usd_2010), fk_clean,
       col=fk_clean$col, cex=fk_clean$kp_rating/4, lwd=2)
Axis(side=1, labels=2:8, at=2:8, lwd=2)
Axis(side=2, labels=2:8, at=2:8, lwd=2)
legend('topleft', c('Прибыльный', 'Убыточный'),
       lwd=2, pch=1, lty=NA, pt.cex=1.5, bty='n',
       col=c('royalblue','firebrick'))
legend('left', legend=seq(2,10,2),
       title='Рейтинг на\n"Кинопоиске"',
       lwd=2, pch=1, lty=NA, pt.cex=seq(2,10,2)/4, bty='n',
       col='gray30')
dev.off()
