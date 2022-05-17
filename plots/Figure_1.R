library(ggplot2)

d = data.frame(x = seq(-3,3,by=0.1))
a = 0.8
b = 0
c = -0.1
d$y = a + b*d$x + c*d$x*d$x


g1 = ggplot(d, aes(x,y)) + 
  geom_line(col="darkblue",size=1.3,alpha=0.3) + 
  theme_bw() + 
  ylab("Estimated effect") + 
  xlab("Scaled temperature") +
  geom_point(data = data.frame(x=c(-2.83,0,2.83,0,0),y=c(0,0,0,0.7,0.8)), col="darkblue",size=3) + 
  geom_text(data = data.frame(x=-3,y=0.03), label="(a)") + 
  geom_text(data = data.frame(x=2.83-0.17,y=0.03), label="(b)") +
  geom_text(data = data.frame(x=-0.17,y=0.03), label="(c)") + 
  geom_text(data = data.frame(x=-0.17,y=0.73), label="(e)") + 
  geom_text(data = data.frame(x=-0.17,y=0.83), label="(d)") + 
  geom_line(data = data.frame(x = c(-2.83,2.83), y = c(0,0)), col="darkblue",linetype="dashed") +
  geom_line(data = data.frame(x = c(0,1), y = c(0.7,0.7)), col="darkblue",linetype="dashed") +
  geom_line(data = data.frame(x = c(0,0), y = c(0,0.8)), col="darkblue",linetype="dashed")

ggsave(plot = g1, filename="plots/Figure_1.png")

