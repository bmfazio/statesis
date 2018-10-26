w <- 800*1.2*1.2
h <- 500*1.2
tc <- 2.6
png("D:/gitrepos/statesis/latex/img/oprobit.png", width = w, height = h)
###
par(mar = c(0, 0, 0, 0))

xx <- seq(-2.5, 2.5, len = 101)
yd <- dnorm(xx, 0.42, 0.5)
yc <- pnorm(xx, 0.42, 0.5)

plot(xx, yd, type = "l", axes = FALSE, ylim = c(0,1), xlim = c(-1.5, 2),
     xlab = "", ylab = "", col = "gray", lwd = 2)
lines(xx, yc, type = "l", lwd = 2)

usr <- par("usr")

arrows(usr[1], 0, usr[2], 0, length = 0.1, angle = 25)
arrows(-1.5, usr[3], -1.5, usr[4], length = 0.1, angle = 0)
text(-1.55, usr[4], "1", adj = c(1, 1), cex = tc)

segments(-1.5, 1, 2, 1, lty = 2, lwd = 0)

segments(0, 0, 0, yc[xx==0], lty = 2, lwd = 0)
text(0.15, 0.03, expression(c[0]), adj = c(1, 0), cex = tc)
segments(1, 0, 1, yc[xx==1], lty = 2, lwd = 0)
text(1.15, 0.03, expression(c[1]), adj = c(1, 0), cex = tc)
arrows(0, yc[xx==0], -1.5, yc[xx==0], length=0.1)
arrows(1, yc[xx==1], -1.5, yc[xx==1], length=0.1)

text(-1.35, yc[xx==0]*0.42, expression(p^0), adj = c(1, 0), cex = tc)
text(-1.35, (yc[xx==1]-yc[xx==0])*0.45+yc[xx==0], expression(p^1), adj = c(1, 0), cex = tc)
text(-1.35, (1-yc[xx==1])*0.32+yc[xx==1], expression(p^2), adj = c(1, 0), cex = tc)
###
dev.off()