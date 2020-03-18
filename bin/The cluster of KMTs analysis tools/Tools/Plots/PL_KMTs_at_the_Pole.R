print(ggplot(KMTs_at_the_Pole1, aes(Data_label, `No. of KMTs`)) + 
        geom_violin(trim = FALSE, draw_quantiles = c(0.5), fill = "red") + 
        geom_jitter(width = 0.25, size = 2) + 
        theme_classic())