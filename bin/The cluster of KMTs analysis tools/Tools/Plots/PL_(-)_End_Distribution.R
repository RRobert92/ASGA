print(ggplot(Minus_end_position, aes(Relative_minus_position,minus_dist_to_pole)) + 
        geom_density_2d(color = "red") + 
        geom_point(alpha = 0.25) +
        ylim(0, 3) + xlim(-0.3, 0.5) +
        theme_classic() +        ggtitle(paste(Data_label, "Length Distribution")) +
        xlab("(-) end relative position") + 
        ylab("(-) end distance to the pole"))

