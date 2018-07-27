library(ggplot2)
library(statmod)
library(tikzDevice)
library(png)
library(grid)
plot(dis, type="l", main="Wiener process", xlab="Time", ylab="Degradation signal S")

Rmax = 100
mu = 1.2
sigma = 1.1
N = 1001
tm = 0
Tmax = 100
Sinit = 40
df = data.frame(
  dis = rnorm(N, mu/10, sigma/sqrt(10)),
  x = seq(0, Tmax, Tmax/(N-1)),
  ymax = rep(Rmax, N)
)
df$y = cumsum(df$dis) + Sinit
df$y[df$x > tm]
df$y[df$x >= tm] = df$y[df$x >= tm] - min(df$y[df$x >= tm])
df$mean = Sinit + mu*df$x
df$var = sigma^2*df$x
df$sigma = sqrt(df$var)
df$min = df$mean - 2*df$sigma
df$min[df$x >= tm] = df$min[df$x >= tm] - min(df$min[df$x >= tm])
df$max = df$mean + 2*df$sigma
df$max[df$x >= tm] = df$max[df$x >= tm] - min(df$max[df$x >= tm])
df$mean[df$x >= tm] = df$mean[df$x >= tm] - min(df$mean[df$x >= tm])
df$shademax = rep(Rmax, N)
df$shademax[df$max < Rmax] = df$max[df$max < Rmax]
Tfail = df$x[min(which(df$y > Rmax))]
Tmean = df$x[min(which(df$mean > Rmax))]
df$invgauss = dinvgauss(df$x, mean=Tmean, shape=Rmax^2/(sigma)^2)*250 + Rmax
dfmax = data.frame(
  x = df$x[df$max < Rmax],
  y = df$max[df$max < Rmax]
)
dfmean = data.frame(
  x = df$x[df$mean < Rmax],
  y = df$mean[df$mean < Rmax]
)
dfsig = data.frame(
  x = df$x[df$y < Rmax],
  y = df$y[df$y < Rmax]
)
dfpdf = data.frame(
  x = df$x[df$invgauss >= Rmax + 0.05],
  y = df$invgauss[df$invgauss >= Rmax + 0.05]
)
#tikz('H:/STN/STN-Scheduler/results/deg-sig2.tex',width=4,height=3)
ggplot(df, aes(x, y)) +
  theme(text = element_text(color=rgb(0, 52/255, 92/255))) +
  theme(panel.background = element_rect(fill = rgb(225/255, 229/255, 235/255))) +
  theme(axis.text = element_text(color=rgb(77/255, 106/255, 141/255))) +
  geom_ribbon(aes(ymin=df$min, ymax=df$shademax), fill=rgb(173/255, 190/255, 203/255)) +
  geom_ribbon(data=dfpdf, aes(ymin=Rmax, ymax=y), fill=rgb(205/255, 231/255, 205/255)) +
  geom_line(aes(x, ymax), linetype=2, color="red") +
  labs(y="$s^{meas}(t)$", x="time t") +
  geom_text(aes(x = 25, y = Rmax+5), label = "$s^{max}$", color = "red") +
  geom_text(aes(x = Tfail+5, y = 10), label = "$t^{fail}$", color = "red") +
  geom_text(aes(x = Tfail+3, y = 119), label = "$P(S(t)>s^{max})$", color=rgb(2/255, 128/255, 2/255)) +
  geom_segment(aes(x=Tfail, y=0, xend=Tfail, yend=Rmax), linetype=2, color=rgb(254/255, 3/255, 4/255)) +
  geom_line(data=dfsig, linetype=1, color=rgb(0, 52/255, 92/255)) +
  geom_line(data=dfmean, color=rgb(77/255, 106/255, 141/255)) +
  geom_line(aes(x, min), linetype=2, color=rgb(77/255, 106/255, 141/255)) +
  geom_line(data=dfmax, linetype=2, color=rgb(77/255, 106/255, 141/255)) +
  scale_x_continuous(limits = c(0,100)) +
  scale_y_continuous(limits = c(-10,120)) +
  # geom_vline(xintercept=Tmean, color="red") +
  geom_line(data=dfpdf, color=rgb(2/255, 128/255, 2/255))
dev.off()
ggsave(file="example-wiener.pdf", width=10, height=6)


img <- readPNG("maintenance.png")
g <- rasterGrob(img, interpolate=TRUE)
img2 <- readPNG("car-breakdown.png")
g2 <- rasterGrob(img2, interpolate=TRUE)

tikz('H:/STN/STN-Scheduler/results/deg-sig.tex',width=4,height=3)
ggplot(df, aes(x, y)) +
  theme(text = element_text(color=rgb(0, 52/255, 92/255))) +
  theme(panel.background = element_rect(fill = rgb(225/255, 229/255, 235/255))) +
  theme(axis.text = element_text(color=rgb(77/255, 106/255, 141/255))) +
  geom_line(aes(x, ymax), linetype=2, color="red") +
  labs(y="degradation signal $s^{meas}(t)$", x="time t") +
  geom_text(aes(x = 25, y = Rmax+5), label = "$s^{max}$", color="red") +
  geom_text(aes(x = 5, y = Sinit-5), label = "$s^{init}$") +
  geom_text(aes(x = 59, y = 5), label = "$s^{0}$") +
  annotation_custom(g, xmin=70, xmax=85, ymin = Sinit, ymax=Sinit+30) +
  annotation_custom(g2, xmin=180, xmax=195, ymin = 95, ymax=125) +
  #geom_text(aes(x = Tfail+5, y = 10, label = "T[failure]"), parse = TRUE, color = "red", size=6) +
  #geom_segment(aes(x=Tfail, y=0, xend=Tfail, yend=Rmax), linetype=2, color=rgb(254/255, 3/255, 4/255)) +
  geom_line(data=dfsig, linetype=1, color=rgb(0, 52/255, 92/255)) +
  scale_x_continuous(limits = c(0,200)) +
  scale_y_continuous(limits = c(-10,120))
dev.off()
ggsave(file="example-wiener-simple.pdf", width=10, height=6)




mu = 5
sd = 1.5
df = data.frame(
  x = seq(0,10,0.1)
)
df$y = dnorm(df$x, mean=mu, sd=sd)
alpha = 0.02
lower = qnorm(alpha, mean=mu, sd=sd)
upper = qnorm(1 - alpha, mean=mu, sd=sd)
dflower = data.frame(
  y = df$y[df$x <= lower]
)
dfupper = data.frame(
  y = df$y[df$x >= upper]
)
dfmiddle = data.frame(
  y = df$y[df$x >= lower & df$x <= upper],
  x = df$x[df$x >= lower & df$x <= upper]
)
lab = c(expression(paste("P(D","%in%")), expression(paste("P(D","%in%")))
dflab = data.frame(
  lab = lab
)
tikz('H:/STN/STN-Scheduler/results/alpha.tex',width=2,height=2)
ggplot(df) +
  theme(text = element_text(size=8, color=rgb(0, 52/255, 92/255))) +
  theme(panel.background = element_rect(fill = rgb(225/255, 229/255, 235/255))) +
  theme(axis.text = element_text(color=rgb(77/255, 106/255, 141/255))) +
  labs(y="$\\mathrm{pdf}(D_{j,k})$", x="$D_{j,k}$") +
  geom_ribbon(data=dfmiddle, aes(x=x, ymin=0, ymax=y), fill=rgb(173/255, 190/255, 203/255)) +
  geom_line(aes(x,y), color=rgb(0, 52/255, 92/255))+
  geom_segment(aes(x=lower, y=0, xend=lower, yend=dnorm(lower, mean=5, sd=1.5)), color=rgb(0, 52/255, 92/255), linetype=2) +
  geom_segment(aes(x=upper, y=0, xend=upper, yend=dnorm(upper, mean=5, sd=1.5)), color=rgb(0, 52/255, 92/255), linetype=2) +
  geom_text(aes(x = upper, y = -0.015), label = "$\\bar{d}_{j,k}(1+\\epsilon)$", color = rgb(0, 52/255, 92/255), size=2) +
  geom_text(aes(x = lower, y = -0.015), label = "$\\bar{d}_{j,k}(1-\\epsilon)$", color = rgb(0, 52/255, 92/255), size=2) +
  geom_text(aes(x = mu, y = 0.02), label = "$P(D_{j,k} \\in \\mathcal{U}) = 1 - 2\\alpha$", color = rgb(0, 52/255, 92/255), size=2)
dev.off()



Rmax = 100
mu = 1.2
sigma = 1.2
N = 1001
Tmax = 100
dt = Tmax/N
mu = rep(0.8, N)
mu[round(N/3):round(N*2/3)] = rep(1.7, length(mu[round(N/3):round(N*2/3)]))
sigma = rep(1.2, N)
sigma[round(N/3):round(N*2/3)] = rep(1.7, length(sigma[round(N/3):round(N*2/3)]))
df = data.frame(
  dis = rnorm(N, mu/10, sigma/sqrt(10)),
  x = seq(0, Tmax, Tmax/(N-1)),
  ymax = rep(Rmax, N)
)
df$y = cumsum(df$dis)
df$mean = cumsum(mu*dt)
df$var = cumsum(sigma^2*dt)
df$sigma = sqrt(df$var)
df$min = df$mean - 2*df$sigma
df$max = df$mean + 2*df$sigma
df$shademax = rep(Rmax, N)
df$shademax[df$max < Rmax] = df$max[df$max < Rmax]
Tfail = df$x[min(which(df$y > Rmax))]
Tmean = df$x[min(which(df$mean > Rmax))]
Tstart = df$x[min(which((abs(mu[2:1001] - mu[1:1000])) > 0))]
Tend = df$x[max(which((abs(mu[2:1001] - mu[1:1000])) > 0))]
df$invgauss = dinvgauss(df$x, mean=Tmean, shape=Rmax^2/(sigma)^2)*250 + Rmax
dfmax = data.frame(
  x = df$x[df$max < Rmax],
  y = df$max[df$max < Rmax]
)
dfmean = data.frame(
  x = df$x[df$mean < Rmax],
  y = df$mean[df$mean < Rmax]
)
dfsig = data.frame(
  x = df$x[df$y < Rmax],
  y = df$y[df$y < Rmax]
)
dfpdf = data.frame(
  x = df$x[df$invgauss >= Rmax + 0.05],
  y = df$invgauss[df$invgauss >= Rmax + 0.05]
)

ggplot(df, aes(x, y)) +
  theme(text = element_text(size=16, color=rgb(0, 52/255, 92/255))) +
  theme(panel.background = element_rect(fill = rgb(225/255, 229/255, 235/255))) +
  theme(axis.text = element_text(color=rgb(77/255, 106/255, 141/255))) +
  geom_ribbon(aes(ymin=df$min, ymax=df$shademax), fill=rgb(173/255, 190/255, 203/255)) +
  # geom_ribbon(data=dfpdf, aes(ymin=Rmax, ymax=y), fill=rgb(205/255, 231/255, 205/255)) +
  geom_line(aes(x, ymax), linetype=2, color="red") +
  labs(y="Degradation Signal S", x="Time") +
  geom_text(aes(x = 25, y = Rmax+5, label = paste("S[max]")), parse = TRUE, color = "red", size=6) +
  geom_text(aes(x = Tfail+5, y = 10, label = "T[failure]"), parse = TRUE, color = "red", size=6) +
  # geom_text(aes(x = Tfail+3, y = Rmax+4, label = "P(S>S[max])"), parse = TRUE, color=rgb(2/255, 128/255, 2/255), size=5) +
  geom_segment(aes(x=Tfail, y=0, xend=Tfail, yend=Rmax), linetype=2, color=rgb(254/255, 3/255, 4/255)) +
  geom_segment(aes(x=Tstart, y=0, xend=Tstart, yend=Rmax), linetype=2) +
  geom_segment(aes(x=Tend, y=0, xend=Tend, yend=Rmax), linetype=2) + 
  geom_text(aes(x = Tstart/2, y = -5, label = paste("Mode 1")), size=6) +
  geom_text(aes(x = (Tstart+Tend)/2, y = -5, label = paste("Mode 2")), size=6) +
  geom_text(aes(x = (Tend+Tmax)/2, y = -5, label = paste("Mode 1")), size=6) +
  geom_line(data=dfsig, linetype=1, color=rgb(0, 52/255, 92/255)) +
  geom_line(data=dfmean, color=rgb(77/255, 106/255, 141/255)) +
  geom_line(aes(x, min), linetype=2, color=rgb(77/255, 106/255, 141/255)) +
  geom_line(data=dfmax, linetype=2, color=rgb(77/255, 106/255, 141/255)) +
  scale_x_continuous(limits = c(0,100)) +
  scale_y_continuous(limits = c(-10,120))
  # geom_vline(xintercept=Tmean, color="red") +
  # geom_line(data=dfpdf, color=rgb(2/255, 128/255, 2/255))
ggsave(file="example-wiener-om.pdf", width=10, height=6)


x = 0
l = 50
k = seq(0,100)
x = exp(-l)*l^k/factorial(k)
sum(x)
plot(x)
