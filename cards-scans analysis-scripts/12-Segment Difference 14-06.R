##
#Check difference between the segements created
#Simplest way is to run independent test for each variable of interest
##

#Euros
aov1 <- aov(euros ~ c6, data = scans1)

dev.new(width=10, height=10)

plot(TukeyHSD(aov1), las = 1, col="brown")
mtext("Pairs of segments", side = 2, line = 3)


#Number of Units
dev.new(width=10, height=10)

aov2 <- aov(units ~ c6, data = scans1)

plot(TukeyHSD(aov2), las = 1)
mtext("Pairs of segments", side = 2, line = 3)

#Number of Categories 
dev.new(width=10, height=10)

aov3 <- aov(num_of_cat ~ c6, data = scans1)

plot(TukeyHSD(aov3), las = 1)
mtext("Pairs of segments", side = 2, line = 3)

#Age
dev.new(width=10, height=10)

aov4 <- aov(age ~ c6, data = scans1)

plot(TukeyHSD(aov4), las = 1)
mtext("Pairs of segments", side = 2, line = 3)

#hh_size

dev.new(width=10, height=10)

aov5 <- aov(hh_size ~ c6, data = scans1)

plot(TukeyHSD(aov5), las = 1)
mtext("Pairs of segments", side = 2, line = 3)

rm(aov1,aov2, aov3, aov4, aov5)

#day of the week

dev.new(width=10, height=10)

aov6 <- aov(day1 ~ c6, data = scans1)

plot(TukeyHSD(aov6), las = 1)
mtext("Pairs of segments", side = 2, line = 3)

rm(aov1,aov2, aov3, aov4, aov5, aov6)


