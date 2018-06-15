aug.bulk <- out



if (!is(aug, "augmentedRCBD.bulk")) {
  stop('"aug" is not of class "augmentedRCBD.bulk"')
}

augreport <- read_docx("./inst/template.docx")

augreport <- body_add_par(augreport, value = "augmentedRCBD", style = "Title")
augreport <- body_add_toc(augreport, level = 2)

# Details
augreport <- body_add_par(augreport, value = "Details", style = "heading 1")

Details <- t(data.frame(`Number of blocks` = aug.bulk$Details$`Number of blocks`,
                        `Number of treatments` = aug.bulk$Details$`Number of treatments`,
             `Number of check treatments` = aug.bulk$Details$`Number of check treatments`,
             `Number of test treatments` = aug.bulk$Details$`Number of test treatments`,
             `Check treatments` =  paste(aug.bulk$Details$`Check treatments`, collapse = ", "),
             `Number of Traits` = aug.bulk$Details$`Number of Traits`,
             `Traits` = paste(aug.bulk$Details$Traits, collapse = ", ")))
Details <- data.frame(Details)
Details <- cbind(gsub("\\.", " ", rownames(Details)), Details)
colnames(Details) <- c("Item", "Details")

Details <- regulartable(data = data.frame(Details))
Details <- autofit(Details)
augreport <- body_add_flextable(augreport, Details)

# ANOVA, TA
augreport <- body_add_par(augreport, value = "ANOVA, Treatment Adjusted", style = "heading 1")
anovata <- aug.bulk$`ANOVA, Treatment Adjusted`
anovata$Df <- as.character(anovata$Df)
anovata <- autofit(flextable(anovata))
anovata <- bold(anovata, part = "header")
augreport <- body_add_flextable(augreport, anovata)

# ANOVA, BA
augreport <- body_add_par(augreport, value = "ANOVA, Block Adjusted", style = "heading 1")
anovaba <- aug.bulk$`ANOVA, Block Adjusted`
anovaba$Df <- as.character(anovaba$Df)
anovaba <- autofit(flextable(anovaba))
anovaba <- bold(anovaba, part = "header")
augreport <- body_add_flextable(augreport, anovaba)

# Std. error
augreport <- body_add_par(augreport, value = "Standard Errors", style = "heading 1")
SE <- aug.bulk$`Std. Errors`
SE <- autofit(flextable(SE))
SE <- bold(SE, part = "header")
augreport <- body_add_flextable(augreport, SE)

# CD
augreport <- body_add_par(augreport,
                          value = paste("Critical Difference (", aug.bulk$alpha*100, "%)", sep = ""),
                          style = "heading 1")
CD <- aug.bulk$CD
CD <- autofit(flextable(CD))
CD <- bold(SE, part = "header")
augreport <- body_add_flextable(augreport, CD)

# CV
augreport <- body_add_par(augreport, value = "Coefficient of Variance", style = "heading 1")
CV <- aug.bulk$CV
CV <- autofit(flextable(CV))
CV <- bold(CV, part = "header")
augreport <- body_add_flextable(augreport, CV)

# Overall adj. mean
augreport <- body_add_par(augreport, value = "Overall Adjusted Mean", style = "heading 1")
oadjmean <- aug.bulk$`Overall adjusted mean`
oadjmean <- autofit(flextable(oadjmean))
oadjmean <- bold(oadjmean, part = "header")
augreport <- body_add_flextable(augreport, oadjmean)

# Descriptive statistics
augreport <- body_add_par(augreport, value = "Descriptive Statistics", style = "heading 1")
descout <- aug.bulk$`Descriptive statistics`
descout <- autofit(flextable(descout))
descout <- bold(descout, part = "header")
augreport <- body_add_flextable(augreport, descout)

# Frequency distribution
augreport <- body_add_par(augreport, value = "Frequency Distribution", style = "heading 1")

# for(i in 1:aug.bulk$Details)
#trqit name
src <- tempfile(fileext = ".png")
png(filename = src, width = 6, height = 4, units = 'in', res = 300)
plot(aug.bulk$`Frequency distribution`$y1)
dev.off()
augreport <- body_add_img(augreport, src = src, width = 6, height = 4)



print(augreport, target = "test.docx")
