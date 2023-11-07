setDocBackgroundColor <- function(tab, df, colors, levels) {
  
  stopifnot(length(colors) == length(levels))
  for (i in seq_along(levels)) {
    lvl <- levels[i]
    kat <- pull(df, "Kat.")
    idx <- which(kat == lvl)
    tab <- flextable::bg(x = tab, 
                         j = 7, # Kat. column
                         i = idx, 
                         bg = colors[i])
  }
  return(tab)
}

# angepasste export::table2doc funktion, original see https://github.com/tomwenseleers/export, credit to Tom Wenseleers
table2doc_ = function(x = NULL, file = "Rtable", append = FALSE, digits = 2, 
                      digitspvals = NULL, trim.pval = 1E-16, width = NULL, height = NULL, offx = 1, offy = 1, 
                      font = ifelse(Sys.info()["sysname"]=="Windows","Arial","Helvetica")[[1]], pointsize = 12, 
                      add.rownames = FALSE) {
  
  if(is.null(digitspvals)) digitspvals <- digits
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
  
  ext <-  ".docx"
  
  file = sub("^(.*)[.].*", "\\1", file)  # remove extension if given
  file = paste0(file, ext)  # add extension
  
  if (append & file.exists(file)) { 
    doc = officer::read_docx(path = file) 
    doc = officer::body_add_break(doc, pos = "after")
  } else { 
    doc = officer::read_docx(path = "template.docx") 
  }
  pagesize <- (doc$sect_dim$page - doc$sect_dim$margins[c(3,2)])/1440 # 1440 is a factor to convert to inches
  
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
    tab <- export:::xtable2(x=outp, ndigits = digits, ndigitspvals = digitspvals, trim.pval = trim.pval)
  } else if (length(intersect(class(outp), as.character(gsub("tidy.", "", methods(broom::tidy))))) >= 1) {
    tab <- export:::tidy2(x=outp, ndigits = digits, ndigitspvals = digitspvals, trim.pval = trim.pval)
  } else { # should not occur
    tab <- export:::data.frame2(x=outp, ndigits = digits, ndigitspvals = digitspvals, trim.pval = trim.pval)
  }
  
  nc <- ncol(tab)
  nr <- nrow(tab)
  tblaspectr = nc / nr * 2  # guess table aspect ratio
  pageaspectr = pagesize["width"]/pagesize["height"]
  if (pageaspectr > tblaspectr) {
    xf = tblaspectr/pageaspectr
    yf = 1
  } else {
    xf = 1
    yf = pageaspectr/tblaspectr
  }
  w = pagesize["width"] * xf
  h = pagesize["height"] * yf
  # if width and height is given override other scaling params
  if (!is.null(width)) w = width  
  if (!is.null(height)) h = height
  
  # Avoid bug in flextable: when one of the colnames = x, flextable returns an empty table
  x.col <- which(colnames(tab) == "x")
  if(length(x.col)>0) colnames(tab)[x.col]<- "x "
  
  cell.height <- min(h, pagesize["height"] - offy)/(nr+1)
  cell.width <- min(w, pagesize["width"] - offx)/(nc+1)
  
  if(inherits(tab,"xtable")){
    tab <- flextable::as_flextable(tab, include.rownames = add.rownames, rowname_col = ".")
    tab <- flextable::width(tab, width=c(1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 4))
    tab <- flextable::height(tab, height=cell.height)
  } else {
    if(add.rownames) x <- cbind(" " = rownames(x), x)
    tab <- flextable::flextable(tab, cheight = cell.height, cwidth = cell.width)
  }
  
  # Format the digits 
  col.pval <- grep("\\QPr(\\E|\\Qp-value\\E|\\Qp value\\E|\\Qpadj\\E|^p$|^padj$|p[.]value", tab$col_keys, value = TRUE)
  col.df <- grep("^df$", tab$col_keys, value = TRUE, ignore.case = TRUE) 
  col.other <- tab$col_keys[! tab$col_keys %in% c(col.pval, col.df)]
  tab <- flextable::colformat_double(x = tab, j = col.other, digits = digits)
  tab <- flextable::colformat_int(x = tab, j = col.df)
  tab <- flextable::colformat_double(x = tab, j = col.pval)
  tab <- flextable::bold(tab, part = "header") # bold header
  tab <- flextable::fontsize(tab, part = "all", size = pointsize) 
  tab <- flextable::font(tab, part = "all", fontname = font)
  tab <- setDocBackgroundColor(tab = tab, df = x, colors = cols, levels = lvls)
  
  doc <- flextable::body_add_flextable(doc, value = tab)
  
  
  print(doc, target = file)
  message(paste0("Exported table as ",file))
  return(tab)
}
