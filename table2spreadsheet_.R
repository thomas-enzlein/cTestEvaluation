setExcelBackgroundColor <- function(wb, df, colors, levels, sheetName) {
  stopifnot(length(colors) == length(levels))
  
  len <- length(df$Kat.) + 1
  
  for(i in seq_along(levels)) {
    st <- openxlsx::createStyle(fontColour = "black", bgFill = colors[i])
    openxlsx::conditionalFormatting(wb, 
                          sheet = sheetName,
                          cols = 7, # kat col 
                          rows = 1:len, 
                          type = "contains",
                          rule = levels[i], 
                          style = st
    )
  }
  
  return(wb)
}

# angepasste export::table2spreadsheet funktion, original see https://github.com/tomwenseleers/export, credit to Tom Wenseleers
table2spreadsheet_ = function(x = NULL, file = "Rtable", type = c("XLS"), append = FALSE, sheetName="new sheet",
                              digits = 1, digitspvals = 2, trim.pval = 1E-16, add.rownames = FALSE, ...) {
  
  obj=x
  if (is.null(obj)) {
    outp = .Last.value # capture previously shown output or use passed object
  } else {
    outp = obj
  }
  if (is.null(outp)) stop("no R stats object available to export")
  supobjects = unique(c(as.character(gsub("xtable.", "", methods(xtable::xtable))), 
                        as.character(gsub("tidy.", "", methods(broom::tidy))),
                        "xtabs"))
  if (length(intersect(class(outp), supobjects)) == 0) stop(paste0(class(outp), " is currently not supported by table2office"))
  
  
  type=toupper(type)
  type=match.arg(type,c("XLS"))
  
  ext <-  ".xlsx"
  
  file = sub("^(.*)[.].*", "\\1", file)  # remove extension if given
  file = paste0(file, ext)  # add extension
  
  # deal with specific classes of objects 
  if (inherits(outp, "summary.merMod")) {
    outp <- data.frame(coef(summary(outp)), check.names = F)
  } else if(inherits(outp, "Matrix")) {
    outp <- as.data.frame(as.matrix(x))
  } else if (inherits(outp, c("xtabs", "ftable"))) {
    outp <- ftable(outp)
  } 
  
  # Depending on the data class, call xtable or tidy
  if (length(intersect(class(outp), as.character(gsub("xtable.", "", methods(xtable::xtable))))) >= 1) {
    tab <- export:::xtable2(x=outp, ndigits = digits, ndigitspvals = digitspvals, trim.pval=trim.pval)
  } else if (length(intersect(class(outp), as.character(gsub("tidy.", "", methods(tidy))))) >= 1) {
    tab <- export:::tidy2(x=outp, ndigits = digits, ndigitspvals = digitspvals, trim.pval=trim.pval)
  } else { # should not occur
    tab <- export:::data.frame2(x=outp, ndigits = digits, ndigitspvals = digitspvals, trim.pval=trim.pval)
  }
  
  
  if (append & file.exists(file)){
    doc <- openxlsx::loadWorkbook(file=file)
  } else {
    doc <- openxlsx::createWorkbook()
  }
  openxlsx::addWorksheet(doc, sheetName = sheetName)
  openxlsx::writeData(doc, sheet = sheetName, x = tab, colNames = TRUE, rowNames = add.rownames, 
            headerStyle = openxlsx::createStyle(textDecoration="bold"), withFilter = FALSE)
  sheetStyle <- openxlsx::createStyle(...)
  openxlsx::addStyle(doc, sheet = sheetName, style = sheetStyle, 
           rows = rep(2:(nrow(tab)+1), ncol(tab)), 
           cols = rep((1+add.rownames):(ncol(tab)+add.rownames), each=nrow(tab)))
  
  doc <- setExcelBackgroundColor(wb = doc, df = x, sheetName = sheetName, colors = cols, levels = lvls)
  
  openxlsx::saveWorkbook(doc, file, overwrite = TRUE) 
  
  message(paste0("Exported table as ",file))
  return(as.data.frame(tab))
}
