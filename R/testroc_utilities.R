


print.sensSpecTable <- function(Title, TP, FP, TN, FN) {
   res <- paste0(
      "<style type='text/css'>
  .tg  {border-collapse:collapse;border-spacing:0;border-width:1px;border-style:solid;border-color:black;}
  .tg td{font-family:Arial, sans-serif;font-size:14px;padding:10px 5px;border-style:solid;border-width:0px;overflow:hidden;word-break:normal;}
  .tg th{font-family:Arial, sans-serif;font-size:14px;font-weight:normal;padding:10px 5px;border-style:solid;border-width:0px;overflow:hidden;word-break:normal;}
  .tg .tg-s6z2{text-align:center}
  .tg .tg-uys7{border-color:inherit;text-align:center}
  .tg .tg-h0x1{text-align:center}
  </style>
  <table class='tg'>
    <tr>
      <th class='tg-0lax' colspan='4'>",
      Title,
      "</th>
    </tr>
    <tr>
      <td class='tg-s6z2'></td>
      <td class='tg-uys7' colspan='3'>DECISION BASED ON MEASURE</td>
    </tr>
    <tr>
      <td class='tg-h0x1' rowspan='3'>CRITERION</td>
      <td class='tg-h0x1'></td>
      <td class='tg-h0x1'>Negative</td>
      <td class='tg-h0x1'>Positive</td>
    </tr>
    <tr>
      <td class='tg-s6z2'>Negative</td>
      <td class='tg-s6z2'>",
      TN,
      " (TN)</td>
      <td class='tg-s6z2'>",
      FP,
      " (FP)</td>
    </tr>
    <tr>
      <td class='tg-h0x1'>Positive</td>
      <td class='tg-h0x1'>",
      FN,
      " (FN)</td>
      <td class='tg-h0x1'>",
      TP,
      " (TP)</td>
    </tr>
    <tr>
      <td class='tg-tf2e'></td>
      <td class='tg-tf2e'></td>
      <td class='tg-tf2e'></td>
      <td class='tg-tf2e'></td>
    </tr>
</table>"
   )
   return(res)
}

formatter <- function(x) {
   resToReturn = numeric()
   for (i in 1:length(x)) {
      number = round(x[i], 2)
      if (length(as.character(number)) <= 3) {
         resToReturn[[i]] <- jmvcore::format("{}.00%", number)
      }
      resToReturn[[i]] <- jmvcore::format("{}%", number)
   }
   resToReturn
}

deLong.test <-
   function(data,
            classVar,
            pos_class,
            ref = NULL,
            conf.level = 0.95) {
      # if (length(classVar) != dim(data)[1]) {
      #    stop("\n The number of rows in data must match the length of classVar\n")}
      
      id.pos <- classVar == pos_class
      
      if (sum(id.pos) < 1) {
         stop("\n wrong level specified.\n")
      }
      if (dim(data)[2] < 2) {
         stop("\n data must contain at least two columns.\n")
      }
      if (dim(data)[1] < 2) {
         stop("\n data must contain at least two dependent variables for DeLong's test.\n")
      }
      
      nn <- sum(!id.pos)
      np <- sum(id.pos)
      nauc <- ncol(data)
      
      if (is.null(ref)) {
         L <- matrix(0, nrow = nauc * (nauc - 1) / 2, ncol = nauc)
         newa <- 0
         for (i in 1:(nauc - 1)) {
            newl <- nauc - i
            L[(newa + 1):(newa + newl), i] <- rep(1, newl)
            L[(newa + 1):(newa + newl), ((i + 1):(i + newl))] <-
               diag(-1, nrow = newl, ncol = newl)
            newa <- newa + newl
         }
      }
      else {
         # test for superiority of one method against all others)
         if (ref > nauc)
            stop(
               paste(
                  "Reference ref must be one of the markers (1...",
                  nauc,
                  " in this case)",
                  sep = ""
               )
            )
         L <- matrix(1, ncol = nauc, nrow = nauc - 1)
         L[, -ref] <- diag(-1, nrow = nauc - 1, ncol = nauc - 1)
      }
      
      markern <- as.matrix(data[!id.pos,])
      markerp <- as.matrix(data[id.pos,])
      
      ###
      ### compute wilcox statistic
      ###
      
      WK.STAT <- function(data, y) {
         r <- rank(c(data, y))
         n.data <- length(data)
         n.y <- length(y)
         STATISTIC <-
            sum(r[seq_along(data)]) - n.data * (n.data + 1) / 2
         STATISTIC
      }
      
      auc <- vector("numeric", length = nauc)
      for (r in 1:nauc) {
         auc[r] <- WK.STAT(markerp[, r], markern[, r])
      }
      auc <- auc / (nn * np)
      
      ###
      ### if AUCs smaller than 0.5: 1-auc
      ###
      if (any(auc < 0.5)) {
         data[, auc < 0.5] <- -data[, auc < 0.5]
         auc[auc < 0.5] <- 1 - auc[auc < 0.5]
         markern <- as.matrix(data[!id.pos,])
         markerp <- as.matrix(data[id.pos,])
      }
      
      V10 <- matrix(0, nrow = np, ncol = nauc)
      V01 <- matrix(0, nrow = nn, ncol = nauc)
      
      tmn <- t(markern)
      tmp <- t(markerp)
      for (i in 1:np) {
         V10[i,] <- rowSums(tmn < tmp[, i]) + 0.5 * rowSums(tmn == tmp[, i])
      }
      for (i in 1:nn) {
         V01[i,] <- rowSums(tmp > tmn[, i]) + 0.5 * rowSums(tmp == tmn[, i])
      }
      V10 <- V10 / nn
      V01 <- V01 / np
      
      W10 <- cov(V10)
      W01 <- cov(V01)
      
      ###
      ### estimated covariance matrix
      ###
      
      S <- W10 / np + W01 / nn
      
      ###
      ### compute variances of AUCs and test for AUC > 0.5
      ###
      
      ### Hanley, McNeil (1982)
      q1 <- auc / (2 - auc)
      q2 <- 2 * auc ^ 2 / (1 + auc)
      
      ### Haney, McNeil (1982) / Bamber (1975)
      aucvar <-
         (auc * (1 - auc) + (np - 1) * (q1 - auc ^ 2) + (nn - 1) * (q2 - auc ^ 2)) / (np *
                                                                                         nn)
      zhalf <- (auc - 0.5) / sqrt(aucvar)
      phalf <- 1 - pnorm(zhalf)
      zdelong <- (auc - 0.5) / sqrt(diag(S))
      pdelong <- 1 - pnorm(zdelong)
      
      
      ### global p-value
      ###
      aucdiff <- L %*% auc
      # Original function used rms::matinv() for all solves/inversions.
      # That function was jenky and didn't work
      z <- t(aucdiff) %*% MASS::ginv(L %*% S %*% t(L)) %*% aucdiff
      p <-
         pchisq(z, df = qr(L %*% S %*% t(L))$rank, lower.tail = FALSE)
      
      if (is.null(ref)) {
         cor.auc <- matrix(ncol = 1, nrow = nauc * (nauc - 1) / 2)
         ci <- matrix(ncol = 2, nrow = nauc * (nauc - 1) / 2)
         ctr <- 1
         rows <-
            vector("character", length = (nauc * (nauc - 1) / 2))
         pairp <- matrix(nrow = nauc * (nauc - 1) / 2, ncol = 1)
         quantil <- qnorm(1 - (1 - conf.level) / 2)
         for (i in 1:(nauc - 1)) {
            for (j in (i + 1):nauc) {
               cor.auc[ctr] <- S[i, j] / sqrt(S[i, i] * S[j, j])
               LSL <-
                  t(c(1, -1)) %*% S[c(j, i), c(j, i)] %*% c(1, -1)
               # Original function used rms::matinv() for all solves/inversions.
               # That function was jenky and didn't work
               tmpz <-
                  (aucdiff[ctr]) %*% MASS::ginv(LSL) %*% aucdiff[ctr]
               pairp[ctr] <- 1 - pchisq(tmpz, df = qr(LSL)$rank)
               ci[ctr,] <-
                  c(aucdiff[ctr] - quantil * sqrt(LSL),
                    aucdiff[ctr] + quantil * sqrt(LSL))
               rows[ctr] <- paste(i, j, sep = " vs. ")
               ctr <- ctr + 1
            }
         }
      } else {
         cor.auc <- matrix(ncol = 1, nrow = nauc - 1)
         ci <- matrix(ncol = 2, nrow = nauc - 1)
         rows <- vector("character", length = nauc - 1)
         pairp <- matrix(nrow = nauc - 1, ncol = 1)
         comp <- (1:nauc)[-ref]
         for (i in 1:(nauc - 1)) {
            cor.auc[i] <-
               S[ref, comp[i]] / sqrt(S[ref, ref] * S[comp[i], comp[i]])
            LSL <-
               t(c(1, -1)) %*% S[c(ref, comp[i]), c(ref, comp[i])] %*% c(1, -1)
            # Original function used rms::matinv() for all solves/inversions.
            # That function was jenky and didn't work
            tmpz <- aucdiff[i] %*% MASS::ginv(LSL) %*% aucdiff[i]
            pairp[i] <- 1 - pchisq(tmpz, df = qr(LSL)$rank)
            ci[i,] <-
               c(aucdiff[i] - quantil * sqrt(LSL),
                 aucdiff[i] + quantil * sqrt(LSL))
            rows[i] <- paste(ref, comp[i], sep = " vs. ")
         }
      }
      
      newres <- as.data.frame(cbind(aucdiff, ci, pairp, cor.auc))
      names(newres) <-
         c("AUC Difference",
           "CI(lower)",
           "CI(upper)",
           "P.Value",
           "Correlation")
      rownames(newres) <- rows
      row.names(ci) <-
         row.names(cor.auc) <-
         row.names(aucdiff) <- row.names(pairp) <- rows
      colnames(ci) <-
         c(paste0(100 * conf.level, "% CI (lower)"),
           paste0(100 * conf.level, "% CI (upper)"))
      names(auc) <- 1:nauc
      auc <-
         as.data.frame(cbind(auc, sqrt(aucvar), phalf, sqrt(diag(S)), pdelong))
      colnames(auc) <-
         c("AUC",
           "SD(Hanley)",
           "P(H0: AUC=0.5)",
           "SD(DeLong)",
           "P(H0: AUC=0.5)")
      
      ERG <-
         list(
            AUC = auc,
            difference = newres,
            covariance = S,
            global.z = z,
            global.p = p
         )
      class(ERG) <- "DeLong"
      ERG
   }

print.DeLong <-
   function(x, digits = max(3, getOption("digits") - 3), ...) {
      cat("Estimated AUC's:\n")
      print(format(round(x$AUC, digits = digits, ...), nsmall = digits, ...))
      cat("\n Pairwise comparisons:\n")
      print(format(round(x$difference, digits = digits, ...), nsmall = digits, ...))
      cat(paste(
         "\n Overall test:\n p-value =",
         format.pval(x$global.p, digits = digits),
         "\n"
      ))
   }
