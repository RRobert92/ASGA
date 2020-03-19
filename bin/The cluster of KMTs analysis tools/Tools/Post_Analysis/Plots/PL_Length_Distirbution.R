LD_0_1.5 <- data.frame(LD[with(LD, plus_dist_to_kinetochore_core <= 1.5 & plus_dist_to_kinetochore_core > 0),][,1],
                       LD[with(LD, plus_dist_to_kinetochore_core <= 1.5 & plus_dist_to_kinetochore_core > 0),][,2])

P <- ggplot(LD_0_1.5, aes(KMTs.length)) + geom_density(colour = "red", size = 1) + ylim(0, 1) + xlim(0, 10) + theme_classic() +
  ggtitle(paste(Data_label, "Length Distribution")) +
  xlab("KMTs length (um)") + 
  ylab("Normalized no. of KMTs")

LD_1.5_3 <- data.frame(LD[with(LD, plus_dist_to_kinetochore_core <= 3 & plus_dist_to_kinetochore_core > 1.5),][,1],
                       LD[with(LD, plus_dist_to_kinetochore_core <= 3 & plus_dist_to_kinetochore_core > 1.5),][,2])
P <- P + geom_density(data = LD_1.5_3, aes(KMTs.length), colour = "blue", size = 1) + theme_classic()

LD_3_4.5 <- data.frame(LD[with(LD, plus_dist_to_kinetochore_core <= 10 & plus_dist_to_kinetochore_core > 3),][,1],
                       LD[with(LD, plus_dist_to_kinetochore_core <= 10 & plus_dist_to_kinetochore_core > 3),][,2])
P <- P + geom_density(data = LD_3_4.5, aes(KMTs.length), colour = "green", size = 1) + theme_classic()

print(P)

print(ggplot(LD, aes(`KMTs length`)) +
        geom_density(colour = "black", size = 1) + 
        ylim(0, 1) + xlim(0, 10) + theme_classic() + 
        ggtitle(paste(Data_label, "Length Distribution")) +
        xlab("KMTs length (um)") + 
        ylab("Normalized no. of KMTs"))
