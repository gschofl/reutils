This is a resubmission of reutils 0.2.0. New changes:

- Corrected the Description field in DESCRIPTION.
- Fixed broken URL im the documentation.

## Test environments
* local ubuntu 14.04 install (R-devel, R 3.1.3)
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There were 2 NOTEs:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Gerhard Schöfl <gschofl@yahoo.de>'

New maintainer:
  Gerhard Schöfl <gschofl@yahoo.de>
Old maintainer(s):
  'Gerhard SchÃ¶fl' <gschofl@yahoo.de>

The umlaut in the last name is now rendered correctly after setting the
encoding field to UTF-8.

* Possibly mis-spelled words in DESCRIPTION:
  EUtils (3:25, 9:50, 9:68)
  Entrez (9:20, 10:45)
  GenBank (8:61)
  NCBI (3:20, 8:30, 10:40)
  PubMed (8:53)

These are not misspelled.

## Downstream dependencies
There are currently no downstream dependencies for this package.
