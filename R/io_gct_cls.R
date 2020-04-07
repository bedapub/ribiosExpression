##------------------------------##
## export and import gct/cls files
##------------------------------##

## Export ExpressionSet into gct/cls files
#' @exportMethod writeGct
setGeneric("writeGct", function(obj, file, feat.name, feat.desc) standardGeneric("writeGct"))

#' @export 
setMethod("writeGct",
          c("matrix", "ANY", "ANY", "ANY"),
          function(obj, file, feat.name, feat.desc) {
            if(missing(file)) file <- stdout()
            write_gct(obj, file=file, feat.name=feat.name, feat.desc=feat.desc)
          })
getDfCol <- function(df, name) {
  nameInF <- ncol(df)>=1 & length(name)==1 && (name %in% colnames(df) || name %in% 1:ncol(df))
  if(nameInF) return(df[, name])
  return(name)
}
#' @export 
setMethod("writeGct",
          c("ExpressionSet", "ANY", "ANY", "ANY"),
          function(obj, file, feat.name, feat.desc) {
            fd <- fData(obj)
            if(!missing(feat.name))
              feat.name <- getDfCol(fd, feat.name)
            if(!missing(feat.desc))
              feat.desc <- getDfCol(fd, feat.desc)
            writeGct(exprs(obj), file=file, feat.name=feat.name, feat.desc)
          })

#' Write CLS (class file) from Eset
#' @param eset An ExpressionSet object
#' @param file A file, by default the standard output
#' @param sample.group.col Column name in the sample annotation (\code{pData}) to be used as groups
#' @export 
writeCls <- function(eset, file=stdout(), sample.group.col="group") {
  if(missing(sample.group.col)) {
    stop("Sample groups must be given by the sample.group.col\n")
  }
  cs <- dfFactor(pData(eset), sample.group.col)
  nc <- nlevels(cs)
  str1 <- paste(ncol(eset), nc, 1L, sep=" ")
  str2 <- paste("#", paste(levels(cs), collapse=" "), sep=" ")
  str3 <- paste(as.integer(cs)-1L, collapse=" ")
  writeLines(c(str1, str2, str3),
             con=file)
}


#' Export ExpressionSet as Gct/Cls files
#' 
#' Gct/Cls file formats are required by the Gene Set Enrichment Analysis (GSEA)
#' tool. Functions \code{writeGct} and \code{writeCls} exports file of two
#' formats respectively, and \code{writeGctCls} calls the two function
#' internally to write two files.
#' 
#' The \code{feat.name} option specifies what identifiers should be used for
#' features (probesets). When the value is missing, \code{featureNames} is
#' called to provide feature identifiers.
#' 
#' In contrast, the \code{sample.group.col} cannot be missing: since cls files
#' encode groups (classes) of samples, and if \code{sample.group.col} was
#' missing, it is usually impossible to get class information from
#' \code{sampleNames}.
#' 
#' Internally \code{writeCls} calls \code{\link{dfFactor}} function to
#' determine factor of samples. Therefore \code{sample.group.col} is to a
#' certain degree generic: it can be a character string or integer index of the
#' \code{pData(eset)} data matrix, or a factor vector of the same length as
#' \code{ncol(eset)}.
#' 
#' @aliases writeCls writeGct writeGctCls eset2gct eset2cls
#' @param eset An object of the \code{eSet} class, for example an
#' \code{ExpressionSet} object
#' @param obj A matrix or \code{ExpressionSet} object, which shall be written
#' in GCT format
#' @param file Name of the Gct/Cls file. If left missing, the file is printed
#' on the standard output.
#' @param file.base For writeGctCls, the base name of the two files: the suffix
#' (.gct and .cls) will be appended
#' @param feat.desc Integer or character, indicating which column of the
#' featureData should be used as feature names; if missing, results of the
#' \code{featureNames} function will be used as identifiers in the Gct file.
#' See details.
#' @param feat.name Integer or character, indicating which column of the
#' featureData should be used as feature descriptions. If the value is missing,
#' the Description column of the Gct file will be left blank
#' @param sample.group.col Integer, character or a factor vector of the same
#' length as the sample number, indicating classes (groups) of samples. See
#' details.
#' @param write.add.fData.file Logical, whether additional featureData should
#' be written into a file named \code{${file.base}.add.fData.txt}
#' @param write.add.pData.file Logical, whether additional phenoData should be
#' written into a file named \code{${file.base}.add.pData.txt}
#' @return Functions are used for their side effects.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso See \code{\link{dfFactor}} for possible values of the
#' \code{sample.group.col} option.
#' 
#' See \code{\link{readGctCls}} for importing functions.
#' @references
#' \url{http://www.broadinstitute.org/gsea/doc/GSEAUserGuideTEXT.htm}
#' @examples
#' 
#' data(sample.ExpressionSet)
#' writeGct(sample.ExpressionSet[1:5, 1:4], file=stdout())
#' writeCls(sample.ExpressionSet, file=stdout(), sample.group.col="type")
#' 
#' tmpfile <- tempfile()
#' writeGctCls(sample.ExpressionSet, file.base=tmpfile, sample.group.col="type")
#' readLines(paste(tmpfile, ".cls",sep=""))
#' unlink(c(paste(tmpfile, ".cls", sep=""), paste(tmpfile, ".gct", sep="")))
#' 
#' @export writeGctCls
writeGctCls <- function(eset,
                        file.base,
                        feat.name,
                        sample.group.col,
                        write.add.fData.file=TRUE,
                        write.add.pData.file=TRUE) {
  gct.file <- paste(file.base, ".gct", sep="")
  cls.file <- paste(file.base, ".cls", sep="")
  writeGct(eset, file=gct.file, feat.name=feat.name)
  writeCls(eset, file=cls.file, sample.group.col=sample.group.col)
  if(write.add.fData.file) {
    add.fData.file <- paste(file.base, ".add.fData.txt", sep="")
    write.table(fData(eset), file=add.fData.file, sep="\t", col.names=TRUE)
  }
  if(write.add.pData.file) {
    add.pData.file <- paste(file.base, ".add.pData.txt", sep="")
    write.table(pData(eset), file=add.pData.file, sep="\t", col.names=TRUE)
  }
}

## Import ExpressionSet into gct/cls files
## the C version, about 5x faster than the R implementation for the ALL dataset
readGct <- function(gct.file) {
  mat <- read_gct_matrix(gct.file, keep.desc=TRUE)
  desc <- attr(mat, "desc")
  attr(mat, "desc") <- NULL
  
  fnames <- rownames(mat)
  if(any(duplicated(fnames))) {
    warning("Duplicated feature names detected, they will be made unique.\n")
    fnames <- make.unique(fnames)
    rownames(mat) <- fnames
  }
  res <- new("ExpressionSet",
             exprs=mat,
             featureData=new("AnnotatedDataFrame",
               data.frame(desc=desc, row.names=fnames)),
             phenoData=new("AnnotatedDataFrame",
               data.frame(row.names=colnames(mat))))
  res
}

#' @rdname readGct
#' @export
readCls <- read_cls



#' Read ExpressionSet from Gct/Cls files
#' 
#' As complementary functions to \code{writeGctCls}, \code{readGctCls} reads a
#' pair of gct and cls files (with same base names) into an
#' \code{ExpressionSet} object.
#' 
#' The \code{readGctCls} function calls internally the \code{readGct} and
#' \code{readCls} functions to read in two formats respeectively.
#' \code{readGct} returns a barely annotated \code{ExpressionSet} object, and
#' \code{readCls} returns a vector of levels encoding sample groups.
#' 
#' Since gct/cls contains only one property of features and samples each
#' (Description in the gct file as well as sample groups/levels in the cls
#' file), \code{readGctCls} allows users to provide additional fData/pData
#' files. They should be tab-delimited files, with first column machting
#' exactly the names of features or samples. They must be within the path
#' specified by the \code{path} option, namely in the same directory of gls/cls
#' files.sample
#' 
#' See example below.
#' 
#' @aliases readGct readCls readGctCls
#' @param file.base The full file name of gct/cls files without suffixe, if not
#' in the current diretory, must contain the path (dirname) as well . For
#' instance if it is set as \code{~/my/dir/input}, then the function seeks the
#' file \code{~/my/dir/input.gct} and \code{~/my/dir/input.cls}.
#' @param gct.file The name of the gct file (only valid when file.base is
#' missing).
#' @param cls.file The name of the cls file (only valid when file.base is
#' missing).
#' @param add.fData.file Optional, file of additional feature data, see
#' details.
#' @param add.pData.file Optional, file of additional phenotype (sample) data,
#' see details.
#' @return An \code{ExpressionSet} object. The \code{Description} column in the
#' gct file is encoded in the \code{desc} column in the featureData of the
#' resulting object. The sample groups in the cls file is encoded in the
#' \code{cls} column in the phenoData.
#' @note The \code{readGct} function is a wrapper of the
#' \link[ribiosIO]{read_gct_matrix} function in the \code{ribiosIO} package,
#' which makes up the GCT matrix into an \code{ExpressionSet} object.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{\link{writeGctCls}}. See
#' \code{\link[ribiosIO]{read_gct_matrix}} for underlying C code to import GCT
#' files.
#' @examples
#' 
#' idir <- system.file("extdata", package="ribiosExpression")
#' 
#' sample.eset <- readGctCls(file.base=file.path(idir, "test"))
#' 
#' ext.eset <- readGctCls(file.base=file.path(idir, "test"),
#' add.fData.file=file.path(idir, "test.add.fData.txt"),
#' add.pData.file=file.path(idir, "test.add.pData.txt"))
#' 
#' stopifnot(identical(exprs(sample.eset), exprs(ext.eset)))
#' 
#' ## try to compare pData(sample.eset) with pData(ext.eset), and similarly
#' ## fData(sample.eset) with fData(ext.eset)
#' 
#' @export readGctCls
readGctCls <- function(file.base,
                       gct.file,
                       cls.file,
                       add.fData.file,
                       add.pData.file) {
  has.file.base <- !missing(file.base)
  has.gct.cls <- !missing(gct.file) && !missing(cls.file)

  if(has.file.base && !has.gct.cls) {
    gct.file <- paste(file.base, ".gct", sep="")
    cls.file <- paste(file.base, ".cls", sep="")
  } else if (has.file.base && has.gct.cls) {
    stop("Either 'file.base' or both 'gct.file' and 'cls.file' should be provided, not all at one time\n")
  } else if (!has.file.base && !has.gct.cls) {
    stop("Provide either 'file.base', or both 'gct.file' and 'cls.file' as parameters\n")
  }
  
  stopifnot(file.exists(gct.file) && file.exists(cls.file))

  eset <- readGct(gct.file)
  cls <- readCls(cls.file)
  pData(eset)$cls <- cls

  if(!missing(add.fData.file)) {
    add.fdata <- readFKtable(add.fData.file, featureNames(eset))
    fData(eset) <- cbind(fData(eset), add.fdata)
  }
  
  if(!missing(add.pData.file)) {
    add.pdata <- readFKtable(add.pData.file, sampleNames(eset))
    pData(eset) <- cbind(pData(eset), add.pdata)
  }

  return(eset)
}
