# Example data
blk <- c(rep(1,7),rep(2,6),rep(3,7))
# trt <- c(1, 2, 3, 4, 7, 11, 12, 1, 2, 3, 4, 5, 9, 1, 2, 3, 4, 8, 6, 10)
trt <- c(1, 2, 3, 14, 7, 11, 12, 1, 2, 3, 4, 5, 9, 13, 2, 3, 4, 8, 6, 10)
y1 <- c(92, 79, 87, 81, 96, 89, 82, 79, 81, 81, 91, 79, 78, 83, 77, 78, 78,
        70, 75, 74)
y2 <- c(258, 224, 238, 278, 347, 300, 289, 260, 220, 237, 227, 281, 311, 250,
        240, 268, 287, 226, 395, 450)
dataf <- data.frame(blk, trt, y1, y2)

block = "blk"
treatment = "trt"
traits = c("y1", "y2")
data = dataf
checks = NULL
method.comp = "lsd"
alpha=0.05
describe = TRUE
freqdist = TRUE
gva = TRUE
#
# augmentedRCBD.bulk(data = dataf, block = "blk", treatment = "trt", traits =  c("y1", "y2"),
#                    checks = NULL, method.comp = "lsd", alpha=0.05,
#                    describe = TRUE, freqdist = TRUE, gva = TRUE)

augmentedRCBD.bulk <- function(data, block, treatment, traits, checks = NULL,
                               alpha=0.05, describe = TRUE,
                               freqdist = TRUE, gva = TRUE,
                               round.digits) {

  # Check if data.frame
  if (!is.data.frame(data)) {
    stop('"data" should be a data frame object')
  }
  # check if block column present in data
  if (!(block %in% colnames(data))) {
    stop(paste('Column ', block,
               ' specified as the block column is not present in "data"',
               sep = ""))
  }
  # check if treatment column present in data
  if (!(treatment %in% colnames(data))) {
    stop(paste('Column ', treatment,
               ' specified as the treatment column is not present in "data"',
               sep = ""))
  }
  # check if trait columns present in data
  if (FALSE %in% (traits %in% colnames(data))) {
    stop(paste('The following column(s) specified as trait columns not present in "data":\n',
               paste(traits[!(traits %in% colnames(data))], collapse = ", "),
               sep = ""))
  }
  # check for missing values
  missvcols <- unlist(lapply(data, function(x) TRUE %in% is.na(x)))
  if (TRUE %in% missvcols) {
    stop(paste('The following column(s) in "data" have missing values:\n',
               paste(names(missvcols[missvcols]), collapse = ", ")))
  }
  # check if trait columns are of type numeric/integer
  inttraitcols <- unlist(lapply(data[,traits],
                                function(x) FALSE %in% (is.vector(x, mode = "integer") | is.vector(x, mode = "numeric"))))
  if (TRUE %in% inttraitcols) {
    stop(paste('The following trait column(s) in "data" are not of type numeric:\n',
               paste(names(inttraitcols[inttraitcols]), collapse = ", ")))
  }
  # alpha
  if (!(0 < alpha && alpha < 1)) {
    stop('"alpha" should be between 0 and 1 (0 < alpha <1)')
  }
  # method.comp
  method.comp <- match.arg(method.comp, c("lsd","tukey"), several.ok = FALSE)

  # convert to factor
  data[, block] <- as.factor(data[, block])
  data[, treatment] <- as.factor(data[, treatment])

  if (!missing(checks) && !is.null(checks)) {
  #if (!is.null(checks)) {
    checks <- as.character("checks")
    # checks are present in treatment levels
    if (FALSE %in% c(checks %in% levels(data[, treatment]))) {
      miss <- paste(checks[!(checks %in% levels(data[, treatment]))], collapse = ", ")
      stop(paste("Following check(s) are not present in treatment levels:\n",
                    miss))
    }
  }


  output <- vector("list", length(traits))
  names(output) <- traits

  for (i in 1:length(traits)) {

    output[[i]] <- augmentedRCBD(block = data[, block],
                                 treatment = data[, treatment],
                                 y = data[, traits[i]], checks = checks,
                                 method.comp = method.comp, alpha = alpha,
                                 group = FALSE, console = FALSE)

  }

  # Details
  Details <- output[[1]]$Details

  # ANOVA table

  anovata <- lapply(output, function(x) x$`ANOVA, Treatment Adjusted`)
  anovaba <- lapply(output, function(x) x$`ANOVA, Block Adjusted`)

  anovata <- lapply(anovata, function(x) data.frame(x[[1]]))
  anovaba <- lapply(anovaba, function(x) data.frame(x[[1]]))

  anovata <- Map(cbind, anovata, Trait = names(anovata))
  anovaba <- Map(cbind, anovaba, Trait = names(anovaba))

  anovata <- lapply(anovata, function(x) cbind(Source = rownames(x), x))
  anovaba <- lapply(anovaba, function(x) cbind(Source = rownames(x), x))

  anovata <- lapply(anovata, function(x) mutate_if(x, is.factor, as.character))
  anovaba <- lapply(anovaba, function(x) mutate_if(x, is.factor, as.character))

  anovata <- bind_rows(anovata)
  anovaba <- bind_rows(anovaba)

  anovata$sig <- ifelse(anovata$Pr..F. < 0.01, "**",
                        ifelse(anovata$Pr..F. < 0.05, "*", ""))
  anovaba$sig <- ifelse(anovaba$Pr..F. < 0.01, "**",
                        ifelse(anovaba$Pr..F. < 0.05, "*", ""))

  anovatable <- bind_rows(anovata, anovaba)
  anovatable$Source <- trimws(anovatable$Source)

  dcast(anovatable, Source ~ Trait, value.var = "Mean.Sq")


  # Adjusted means

  # CV

  # overall adj mean

  # SE

  # CD

  # Descriptive statistics

  # GVA

  # Freq Dist


}
