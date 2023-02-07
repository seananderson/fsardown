##
## simulated outputs from a population model
##
## this simulated dataset will have all possible types of values for the proposed 4-panel template

## name of the 4 panels
categories <- c("Catch","SSB","Fishing","Recruitment")
years <- 1950:2022

set.seed(1234)
ts1 <- 2000 + (arima.sim(list(order = c(1,0,0), ar = 0.9), n = length(years))*400)
ts2 <- rep(1500, length(years))

sim.df <-
  rbind(
    data.frame(
      row.number=1,
      col.number=1,
      panel.category=rep(categories[1], length(years)*2),
      year=rep(years, 2),
      ts.name=rep(c("Catch-MT","TAC-MT"), each=length(years)),
      ts.value=c(ts1,ts2)
    ),
    data.frame(
      row.number=1,
      col.number=2,
      panel.category=rep(categories[2], length(years)*4),
      year=rep(years, 4),
      ts.name=rep(c("SSB-MT","SSBlow-MT","SSBhigh-MT","SSBlim-MT"), each=length(years)),
      ts.value=c(ts1,ts1+500, ts1-500, ts2)
    ),
    data.frame(
      row.number=2,
      col.number=1,
      panel.category=rep(categories[3], length(years)*4),
      year=rep(years, 4),
      ts.name=rep(c("F-1/yr","Flow-1/yr","Fhigh-1/yr","Flim-1/yr"), each=length(years)),
      ts.value=c(ts1/2000,(ts1+500)/2000, (ts1-500)/2000, ts2/2000)
    ),
    data.frame(
      row.number=2,
      col.number=2,
      panel.category=rep(categories[4], length(years)*3),
      year=rep(years, 3),
      ts.name=rep(c("R-E06","Rlow-E06","Rhigh-E06"), each=length(years)),
      ts.value=c(ts1/0.001,(ts1/0.001)+8E5, (ts1/0.001)-8E5)/1E6
    )
  )



library(ggplot2)

g <- ggplot(data=sim.df, aes(x=year, y=ts.value)) +
  facet_grid(rows=vars(row.number), cols=vars(col.number), scales="free") +
  geom_line()
g

four.panel.fct <- function(in.df){## in base R
mm <- matrix(c(rep(0,5),0,1,0,2,0,rep(0,5),0,3,0,4,0,rep(0,5)), nc=5, byrow = TRUE)
ll <- layout(mm, widths=c(0.06,0.43,0.06,0.43,0.02), heights=c(c(0.02,0.45,0.04,0.45,0.04))) # layout.show(ll)
par(mar=c(2,2,0,0))
# top-left panel
idx <- which(in.df$panel.category=="Catch")
yl <- c(0,max(in.df[idx,"ts.value"])*1.1)
idx <- which(in.df$panel.category=="Catch" & in.df$ts.name=="Catch-MT")
plot(ts.value~year, data=in.df[idx,], type='l', lwd=2, axes=FALSE, xlab="", ylab="", ylim=yl)
idx <- which(in.df$panel.category=="Catch" & in.df$ts.name=="TAC-MT")
points(ts.value~year, data=in.df[idx,], pch=19)

legend("top", c("TAC","Catch"), lty=c(0,1), pch=c(19,-1))
axis(side=1, padj=-0.5)
axis(side=2, las=1, hadj=0.9)
mtext(side=1, "Year", line=2)
mtext(side=2, "Catch (MT)", line=3)
box()

# top-right panel
idx <- which(in.df$panel.category=="SSB")
yl <- c(0,max(in.df[idx,"ts.value"])*1.1)
idx <- which(in.df$panel.category=="SSB" & in.df$ts.name=="SSB-MT")
plot(ts.value~year, data=in.df[idx,], type='l', lwd=2, axes=FALSE, xlab="", ylab="", ylim=yl)
idx <- which(in.df$panel.category=="SSB" & in.df$ts.name=="SSBlow-MT")
lines(ts.value~year, data=in.df[idx,], type='l', lty=2)
idx <- which(in.df$panel.category=="SSB" & in.df$ts.name=="SSBhigh-MT")
lines(ts.value~year, data=in.df[idx,], type='l', lty=2)
idx <- which(in.df$panel.category=="SSB" & in.df$ts.name=="SSBlim-MT")
lines(ts.value~year, data=in.df[idx,], type='l', lty=3, lwd=2)
text(years[floor(length(years)/2)], max(in.df[idx,"ts.value"])*1.1, "B=Blim")
axis(side=1, padj=-0.5)
axis(side=2, las=1, hadj=0.9)
mtext(side=1, "Year", line=2)
mtext(side=2, "SSB (MT)", line=3)
box()
legend("top", "SSB", bty="n")

## bottom-left panel
idx <- which(in.df$panel.category=="Fishing")
yl <- c(0,max(in.df[idx,"ts.value"])*1.1)
idx <- which(in.df$panel.category=="Fishing" & in.df$ts.name=="F-1/yr")
plot(ts.value~year, data=in.df[idx,], type='l', lwd=2, axes=FALSE, xlab="", ylab="", ylim=yl)
idx <- which(in.df$panel.category=="Fishing" & in.df$ts.name=="Flow-1/yr")
lines(ts.value~year, data=in.df[idx,], type='l', lty=2)
idx <- which(in.df$panel.category=="Fishing" & in.df$ts.name=="Fhigh-1/yr")
lines(ts.value~year, data=in.df[idx,], type='l', lty=2)
idx <- which(in.df$panel.category=="Fishing" & in.df$ts.name=="Flim-1/yr")
lines(ts.value~year, data=in.df[idx,], type='l', lty=3, lwd=2)
text(years[floor(length(years)/2)], max(in.df[idx,"ts.value"])*1.1, "F=Flim")
axis(side=1, padj=-0.5)
axis(side=2, las=1, hadj=0.9)
mtext(side=1, "Year", line=2)
mtext(side=2, "Fishing mortality (per year)", line=3)

box()
legend("top", "Fishing mortality", bty="n")

## bottom-right panel
idx <- which(in.df$panel.category=="Recruitment")
yl <- c(0,max(in.df[idx,"ts.value"])*1.1)
idx <- which(in.df$panel.category=="Recruitment" & in.df$ts.name=="R-E06")
plot(ts.value~year, data=in.df[idx,], type='l', lwd=2, axes=FALSE, xlab="", ylab="", ylim=yl)
idx <- which(in.df$panel.category=="Recruitment" & in.df$ts.name=="Rlow-E06")
lines(ts.value~year, data=in.df[idx,], type='l', lty=2)
idx <- which(in.df$panel.category=="Recruitment" & in.df$ts.name=="Rhigh-E06")
lines(ts.value~year, data=in.df[idx,], type='l', lty=2)
axis(side=1, padj=-0.5)
mtext(side=1, "Year", line=2)
axis(side=2, las=1, hadj=0.9)
mtext(side=1, "Year", line=2)
mtext(side=2, "Recruits (E06)", line=2)
box()
legend("top", "Recruitment", bty="n")

}

tiff.fn <- "CSAS-template-4-panel-figure-for-SAR.tiff"

tiff(tiff.fn, width=2000, height=2000, res=300, compression="lzw")
four.panel.fct(sim.df)
dev.off()

