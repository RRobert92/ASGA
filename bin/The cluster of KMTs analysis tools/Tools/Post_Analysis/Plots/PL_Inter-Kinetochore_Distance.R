print(
  ggplot(IKD,aes(Data_label, `Inter-kinetochore distance`)) + 
    geom_violin(trim = FALSE, draw_quantiles = c(0.5), fill = "red") + 
    theme_classic() +
    xlab("") +
    ggtitle("AVG. Inter - Kinetochore distance")
)
