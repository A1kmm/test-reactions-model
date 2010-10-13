df <- read.csv('out.csv')
png(filename="hydrogenmodel.png", width = 480, height = 480, antialias="subpixel")
plot(df$Time, df$Amount.of.O2.in.Reactor.Vessel, type='l', col='red', ylim=c(0,1), xlab='Time (s)', ylab='Concentration (mol/L)',
     main='Concentration vs time for hydrogen and oxygen reacting at 1300K')
lines(df$Time, df$Amount.of.H2.in.Reactor.Vessel, col='green')
lines(df$Time, df$Amount.of.Water.in.Reactor.Vessel, col='blue')
lines(df$Time, df$Amount.of.O..in.Reactor.Vessel, col='purple')
legend(800, 1, c("O2", "H2", "H2O", "O."), col=c('red', 'green', 'blue', 'purple'), lwd=1)
dev.off()
