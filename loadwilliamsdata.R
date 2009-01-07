# Load data from Williams and Thomas 2007
williamsdata<-read.csv("williamsdata.csv")
# Put it into a reasonable format (convert radial to perpendicular)
williamsdata$distance<-abs(williamsdata$Radial.distance*sin(williamsdata$Angle*pi/180))

# Harbour porpoise
harbourporpoise<-data.frame(object=c(1:length(williamsdata$distance[williamsdata$Species=="hp"|williamsdata$Species=="HP"])),distance=williamsdata$distance[williamsdata$Species=="hp"|williamsdata$Species=="HP"],detected=c(rep(1,length(williamsdata$distance[williamsdata$Species=="hp"|williamsdata$Species=="HP"]))))
# truncated
harbourporpoise.trunc<-data.frame(object=c(1:length(harbourporpoise$distance[harbourporpoise$distance <= 500])),distance=harbourporpoise$distance[harbourporpoise$distance <= 500],detected=c(rep(1,length(harbourporpoise$distance[harbourporpoise$distance <= 500]))))

# Humpback whale
humpback<-data.frame(object=c(1:length(williamsdata$distance[williamsdata$Species=="h"|williamsdata$Species=="H"])),distance=williamsdata$distance[williamsdata$Species=="h"|williamsdata$Species=="H"],detected=c(rep(1,length(williamsdata$distance[williamsdata$Species=="h"|williamsdata$Species=="H"]))))
# truncated
humpback.trunc<-data.frame(object=c(1:length(humpback$distance[humpback$distance <= 2000])),distance=humpback$distance[humpback$distance <= 2000],detected=c(rep(1,length(humpback$distance[humpback$distance <= 2000]))))

# Dall's porpoise
dallsporpoise<-data.frame(object=c(1:length(williamsdata$distance[williamsdata$Species=="dp"|williamsdata$Species=="DP"])),distance=williamsdata$distance[williamsdata$Species=="dp"|williamsdata$Species=="DP"],detected=c(rep(1,length(williamsdata$distance[williamsdata$Species=="dp"|williamsdata$Species=="DP"]))))
# truncated
dallsporpoise.trunc<-data.frame(object=c(1:length(dallsporpoise$distance[dallsporpoise$distance <= 700])),distance=dallsporpoise$distance[dallsporpoise$distance <= 700],detected=c(rep(1,length(dallsporpoise$distance[dallsporpoise$distance <= 700]))))

