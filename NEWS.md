# Changes in ribiosExpression

## Changes in version 1.3.5

### Bioconductor preparation
- Added `biocViews` to DESCRIPTION.
- Added BiocStyle vignette.
- Updated R dependency to >= 4.4.0.
- Added `@return` tags to all exported functions.
- Fixed typos in documentation.
- Replaced `class(x) %in%` with `is.numeric()`.
- Converted NEWS to NEWS.md format.

## Changes in version 1.0-39

- `parseDesignContrast` adds new parameter `expSampleNames` to allow sample
  name consistency check with input matrix.

## Changes in version 1.0-34

- `DesignContrast` prints error message if validity is violated.
- `parseDesignContrastFile` fixes a mistake of assigning group levels.
- `parseDesignContrastFile` now accepts the `dispLevelStr` option.

## Changes in version 1.0-33

- `parseDesignContrastStr` fixes two errors.

## Changes in version 1.0-32

- Add `DesignContrast` object.

## Changes in version 1.0-29

- Add new functionality in keepMaxMeanProbes.Rscript: able to read from .CHIP
  file.
- keepMaxMeanProbes.Rscript now does not depend on the S4 class ExpressionSet
  anymore, therefore much faster.

## Changes in version 1.0-28

- Debug keepMaxMeanProbes.Rscript: `extname` returns NA if not found, therefore
  an additional check is added.

## Changes in version 1.0-27

- Function extension in `ChipFetcher2ExpressionSet`: now non-Affymetrix
  probesets are supported as well.

## Changes in version 1.0-26

- The main body of `readCls` has been moved to `ribiosIO::read_cls`. `readCls`
  becomes a synonym for compatibility.

## Changes in version 1.0-25

- Improvement in `readCls`: error messages provide more informative details for
  mal-formatted files.

## Changes in version 1.0-24

- Bug fix in `writeEset`: expression matrix is written without quotes.
- New script: keepMaxMeanProbes.Rscript filters duplicated genes by taking the
  one with maximum average expression.

## Changes in version 1.0-23

- Bug fix in `readGct`: if input feature names have duplicates, the exprs
  matrix row names are changed.

## Changes in version 1.0-22

- Move exprsMat2gct.Rscript to the ribiosIO package.
- `writeGct` has been refactored for matrix in ribiosIO, and re-implemented as
  an S4-method for matrix and ExpressionSet.

## Changes in version 1.0-21

- Add exprsMat2gct.Rscript to convert between expression matrix files and GCT
  files.

## Changes in version 1.0-20

- `ChipFetcher2ExpressionSet`: add option orthologue.
- `keepMaxStatProbe` uses `function(x) mean(x, na.rm=TRUE)` as default
  function.

## Changes in version 1.0-19

- limma2gsea.Rscript is updated.

## Changes in version 1.0-18

- Refactor GSEA-related functions that are not dependent on the Biobase data
  structure to the ribiosGSEA package.

## Changes in version 1.0-17

- limma2gsea.Rscript guarantees no replicated gene symbols after orthologue
  mapping.

## Changes in version 1.0-16

- Add limma2gsea.Rscript to inst/Rscript.

## Changes in version 1.0-15

- The ribiosAnnotation package has been removed from the dependency list.
- Add comp_diff.Rscript to inst/Rscript.

## Changes in version 1.0-14

- The deprecation message of `remainHighestVarProbe` and `keepHighestVarProbe`
  have been updated.

## Changes in version 1.0-13

- The `rowscale` method warns if the input ExpressionSet does not have
  'lockedEnvironment' as storageMode.

## Changes in version 1.0-12

- Low-level functions for computational drug repositioning refactored into
  ribiosReposition.

## Changes in version 1.0-11

- Add feature in `summarizeProbesets`: option `keep.featureNames`.
- Rename and rewrite `sprintGmt` to `formatGmt` as S4 method.

## Changes in version 1.0-10

- Bugfix in `readEset` for numeric column names.
- Add `annotate` and `reannotate` methods.
- Add documentation for the `ribios.ExpressionSet` object.

## Changes in version 1.0-9

- Add `kendallW` function and `kendallWmat` S4-method.

## Changes in version 1.0-8

- PCA-related data structures and functions are refactored into the ribiosPCA
  package.

## Changes in version 1.0-7

- Add `summarizeProbesets` to summarize probesets.
- NAMESPACE are now explicitly stated.

## Changes in version 1.0-6

- Add dependency on the ribiosAnnotation package.
- Add `writeEset` and `readEset` functions.
- `readFKdata` was renamed as `readFKtable`.

## Changes in version 1.0-5

- Add git version control.

## Changes in version 1.0-4

- Move data.frame and matrix functions to the ribiosUtils package.

## Changes in version 1.0-3

- Refactor `read_gct` into ribiosIO for efficiency.

## Changes in version 1.0-2

- Change `keepHighestVarProbe` to more generalized `keepMaxStatProbe`.

## Changes in version 1.0-1

- Remove the path option from the `readGctCls` function.
