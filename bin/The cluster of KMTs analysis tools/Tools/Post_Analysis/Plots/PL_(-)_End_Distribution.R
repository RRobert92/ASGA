print(ggplot(Minus_end_pos, aes(Relative_minus_position, `Minus end dist.`)) + 
        geom_density_2d(color = "red") + 
        geom_point(alpha = 0.25) +
        ylim(0, 3) + xlim(-0.3, 0.5) +
        theme_classic() +        ggtitle(paste(Data_label, "Minus end distribution")) +
        xlab("(-) end relative position") + 
        ylab("(-) end distance to the pole"))

