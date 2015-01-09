reutils 0.2.0
=============

* New parameters 'version' and 'retmode' in `einfo()`.
* New parameters 'sort' and 'retmode' in `esearch()`.
* New parameter 'retmode' in `esummary()`.
* Support for JSON as alternative output format for EInfo, ESearch and ESummary.

reutils 0.1.2
=============

*  Add global option `reutils.test.remote` to control if unit tests that require online access to NCBI EUtilities are run or not.

reutils 0.1.1
=============

* Add global option `reutils.show.headlines` to control the number of head lines displayed in show,efetch method (currently only if `rettype = "text"`.

* new return type `content(x, as = "textConnection")`.
