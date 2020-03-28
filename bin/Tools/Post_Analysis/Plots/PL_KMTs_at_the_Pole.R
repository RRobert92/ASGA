print(ggplot(KMTs_at_P, aes(Data_label, `No. of KMTs`)) + 
        geom_violin(trim = FALSE, draw_quantiles = c(0.5), fill = "red") + 
        geom_jitter(width = 0.25, size = 2) + 
        theme_classic() +
        ggtitle("No of KMTs at the pole") +
        xlab(""))

write.xlsx(KMTs_at_P, paste("Output/", Data_label, "_KMTs_at_P.xlsx", sep = ""))
