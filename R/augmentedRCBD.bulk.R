# Example data
blk <- c(rep(1,7),rep(2,6),rep(3,7))
trt <- c(1, 2, 3, 14, 7, 11, 12, 1, 2, 3, 4, 5, 9, 13, 2, 3, 4, 8, 6, 10)
y1 <- c(92, 79, 87, 81, 96, 89, 82, 79, 81, 81, 91, 79, 78, 83, 77, 78, 78,
        70, 75, 74)
y2 <- c(258, 224, 238, 278, 347, 300, 289, 260, 220, 237, 227, 281, 311, 250,
        240, 268, 287, 226, 395, 450)
df <- data.frame(blk, trt, y1, y2)


augmentedRCBD.bulk <- function(df, block, treatment, traits, checks = NULL,
                               method.comp = c("lsd","tukey"),
                               alpha=0.05, genetics = TRUE){


}
