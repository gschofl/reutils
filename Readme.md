


# reutils #

`reutils` provides support for interacting with NCBI databases such as PubMed,
Genbank, or GEO via the Entrez Programming Utilities
([EUtils](http://www.ncbi.nlm.nih.gov/books/NBK25501/)).

Please check the relevant
[usage guidelines](http://www.ncbi.nlm.nih.gov/books/NBK25497/#chapter2.Usage_Guidelines_and_Requiremen)
when using these services. Note that Entrez server requests are subject to frequency limits.

## Getting Started ##


### Install ###

reutils is still under development. You can install it from `github` using the `devtools` package.



```r
install_github("reutils", "gschofl")
```





### Important functions ###

With nine E-Utilities, NCBI provides a programmatical interface to the Entrez query and database system for searching and retrieving requested data

Each of these tools corresponds to an `R` function in the reutils package described below.

#### `esearch` ####

`esearch`: search and retrieve a list of primary UIDs or the NCBI History
Server information (queryKey and webEnv). The objects returned by `esearch`
can be passed on directly to `epost`, `esummary`, `elink`, or `efetch`.


#### `efetch` ####

`efetch` retrieves data records from NCBI in a specified retrieval type
and retrieval mode as given in this
[table](http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1). Data are returned as XML or text documents.

#### `esummary` ####

`esummary` returns Entrez database summaries (DocSums) from a list of primary UIDs (Provided as a character vector or as an `esearch` object)

#### `elink` ####

`elink` returns a list of UIDs (and relevancy scores) from a target database
that are related to a set of UIDs provided by the user.

#### `einfo` ####

`einfo` provides field names, term counts, last update, and available updates
for each database.

#### `epost` ####

`epost` uploads primary UIDs to the users's Web Environment on the Entrez
history server for subsequent use with `esummary`, `elink`, or `efetch`.


## Examples ##

### `esearch`: Searching the Entrez databases ###

Let's search PubMed for articles with Chlamydia psittaci in the title that have been published in 2013 and retrieve a list of PubMed IDs (PMIDs).


```r
pmid <- esearch("Chlamydia psittaci[titl] and 2013[pdat]", "pubmed")
pmid

 ##  Object of class 'esearch' 
 ##  List of UIDs from the 'pubmed' database.
 ##  [1] "23699067" "23691148" "23654298" "23532978" "23405306" "23391180"
 ##  [7] "23265868" "23227890" "23098816"
```




Alternatively we can collect the PMIDs on the history server.


```r
pmid2 <- esearch("Chlamydia psittaci[titl] and 2013[pdat]", "pubmed", usehistory = TRUE)
pmid2

 ##  Object of class 'esearch' 
 ##  Web Environment for the 'pubmed' database.
 ##  Number of UIDs stored on the History server: 9
 ##  Query Key: 1
 ##  WebEnv: NCID_1_122533193_130.14.22.215_9001_1380555216_1795268571
```




We can also use `esearch` to search GenBank. Here we do a search for polymorphic membrane
proteins (PMPs) in Chlamydiaceae.


```r
cpaf <- esearch("Chlamydiaceae[orgn] and PMP[gene]", "nucleotide")
cpaf

 ##  Object of class 'esearch' 
 ##  List of UIDs from the 'nucleotide' database.
 ##  [1] "544687983" "532821218" "520923452" "519865230" "519794601" "392376213"
 ##  [7] "410857988" "410810883" "313847556"
```




Some accessors for `esearch` objects


```r
getUrl(cpaf)

 ##  [1] "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?term=Chlamydiaceae%5Borgn%5D%20AND%20PMP%5Bgene%5D&db=nucleotide&retstart=0&retmax=100&rettype=uilist&email=gschofl%40yahoo.de&tool=reutils"
```






```r
getError(cpaf)

 ##  No errors
```






```r
database(cpaf)

 ##  [1] "nucleotide"
```




Extract a vector of GIs:


```r
uid(cpaf)

 ##  [1] "544687983" "532821218" "520923452" "519865230" "519794601" "392376213"
 ##  [7] "410857988" "410810883" "313847556"
```




Get query key and web environment:


```r
querykey(pmid2)

 ##  [1] 1
```






```r
webenv(pmid2)

 ##  [1] "NCID_1_122533193_130.14.22.215_9001_1380555216_1795268571"
```




Extract the content of an EUtil request as XML.


```r
content(cpaf, "xml")

 ##  <?xml version="1.0"?>
 ##  <!DOCTYPE eSearchResult PUBLIC "-//NLM//DTD eSearchResult, 11 May 2002//EN" "http://www.ncbi.nlm.nih.gov/entrez/query/DTD/eSearch_020511.dtd">
 ##  <eSearchResult>
 ##    <Count>9</Count>
 ##    <RetMax>9</RetMax>
 ##    <RetStart>0</RetStart>
 ##    <IdList>
 ##      <Id>544687983</Id>
 ##      <Id>532821218</Id>
 ##      <Id>520923452</Id>
 ##      <Id>519865230</Id>
 ##      <Id>519794601</Id>
....
```




Or extract parts of the XML data using the reference class method `#xmlValue()` and
an XPath expression:


```r
cpaf$xmlValue("//Id")

 ##  [1] "544687983" "532821218" "520923452" "519865230" "519794601" "392376213"
 ##  [7] "410857988" "410810883" "313847556"
```




### `esummary`: Retrieving summaries from primary IDs ###

`esummary` retrieves document summaries (_docsum_s) from a list of primary IDs.
Let's find out what the first entry for PMP is about:


```r
esum <- esummary(cpaf[1])
esum

 ##  Object of class 'esummary' 
 ##  <?xml version="1.0" encoding="UTF-8"?>
 ##  <!DOCTYPE eSummaryResult PUBLIC "-//NLM//DTD eSummaryResult//EN" "http://www.ncbi.nlm.nih.gov/entrez/query/DTD/eSummaryDTD/eSummary_nucleotide.dtd">
 ##  <eSummaryResult>
 ##    <DocumentSummarySet status="OK">
 ##      <DocumentSummary uid="544687983">
 ##        <Caption>NZ_APJW01000002</Caption>
 ##        <Title>Chlamydiaceae bacterium 10-1398/6 ibis.contig.1, whole genome shotgun sequence</Title>
 ##        <Extra>gi|544687983|ref|NZ_APJW01000002.1||gnl|WGS:NZ_APJW01|ibis.contig.1</Extra>
 ##        <Gi>544687983</Gi>
 ##        <CreateDate>2013/09/20</CreateDate>
 ##        <UpdateDate>2013/09/20</UpdateDate>
 ##        <Flags>544</Flags>
 ##        <TaxId>1046581</TaxId>
 ##        <Slen>334575</Slen>
 ##        <Biomol>genomic</Biomol>
....
```




We can also parse _docsum_s into a `data.frame`


```r
esum <- esummary(cpaf[1:4])
content(esum, "parsed")

 ##           Id         Caption
 ##  1 544687983 NZ_APJW01000002
 ##  2 532821218    APJW01000002
 ##  3 520923452     NZ_KE360863
 ##  4 519865230        KE360863
 ##                                                                                                    Title
 ##  1                        Chlamydiaceae bacterium 10-1398/6 ibis.contig.1, whole genome shotgun sequence
 ##  2                        Chlamydiaceae bacterium 10-1398/6 ibis.contig.1, whole genome shotgun sequence
 ##  3 Chlamydia psittaci 84-8471/1 genomic scaffold CP_84_8471_1.contig.1261, whole genome shotgun sequence
 ##  4 Chlamydia psittaci 84-8471/1 genomic scaffold CP_84_8471_1.contig.1261, whole genome shotgun sequence
 ##                                                                         Extra
 ##  1        gi|544687983|ref|NZ_APJW01000002.1||gnl|WGS:NZ_APJW01|ibis.contig.1
....
```





### `efetch`: Downloading full records from Entrez ###

First we search the protein database for sequences of the **c**hlamydial **p**rotease
**a**ctivity **f**actor, [CPAF](http://dx.doi.org/10.1016/j.tim.2009.07.007)


```r
cpaf <- esearch("Chlamydia[orgn] and CPAF", "protein")
cpaf

 ##  Object of class 'esearch' 
 ##  List of UIDs from the 'protein' database.
 ##   [1] "339626260" "220702405" "220702404" "220702403" "220702402"
 ##   [6] "220702401" "220702400" "220702395" "220702394" "339460927"
```




Let's fetch the FASTA record for the first protein. To do that, we have to
set `rettype = "fasta"` and `retmode = "text"`. 


```r
cpaff <- efetch(cpaf[1], rettype = "fasta", retmode = "text")
cpaff

 ##  Object of class 'efetch' 
 ##  >gi|339626260|ref|YP_004717739.1| general secretion pathway protein E [Chlamydia trachomatis L2c]
 ##  MDGNKGTMQDLLDRLPYSFLKKNYLLPVEDLGDKIVFARHLKKTPLEALDEVRLITQKPLSLVSKEEAEI
 ##  IHGLQKLYSDKDGKASEMLQSMQEAVVPESESDTTELLENQENSAPVVRLLNLILKEAIEERASDIHFDP
 ##  VEDLLRIRYRIDGVLHDRHAPPNHLRAALITRIKVLTKLDIAEHRLPQDGRIKLQLGGQEIDMRVSTVPV
 ##  IHGERVVLRILDKRNVILDIRGLCMPPKMETSFRKAIGVPEGILLVTGPTGSGKTTTLYSVIQHLSGPFT
 ##  NIMTIEDPPEYKLPGVAQIAVKPKIGLTFSKGLRHLLRQDPDVLMVGEIRDQETAEIAIQAALTGHLVVS
 ##  TLHTNDAVSAIPRLLDMGVEPYLLSATMIGAVAQRLVRRICTHCKEFCVADVQEQALLRALGKDPFAPLC
 ##  KGRGCSQCFRSGYKGRQGIYEFVDVTTTLRSEIALGRPYHILRGVAEREGYCPLLEHGVELALAGETTLS
 ##  EVLRVAKRSE
 ##  
 ##  EFetch query using the 'protein' database.
....
```




Now we can write the sequence to a fasta file by first extracting the data from the
`efetch` object using `content()`:


```r
write(content(cpaff), file = "~/cpaf.fna")
```




Alternatively we can fetch the FASTA records as _TSeqSet_ XML records
and extract the sequence from the XML file.


```r
cpafx <- efetch(cpaf, rettype = "fasta", retmode = "xml")
cpafx

 ##  Object of class 'efetch' 
 ##  <?xml version="1.0"?>
 ##  <!DOCTYPE TSeqSet PUBLIC "-//NCBI//NCBI TSeq/EN" "http://www.ncbi.nlm.nih.gov/dtd/NCBI_TSeq.dtd">
 ##  <TSeqSet>
 ##    <TSeq>
 ##      <TSeq_seqtype value="protein"/>
 ##      <TSeq_gi>339626260</TSeq_gi>
 ##      <TSeq_accver>YP_004717739.1</TSeq_accver>
 ##      <TSeq_sid>gnl|REF_ideu|CTL2C_511</TSeq_sid>
 ##      <TSeq_taxid>887712</TSeq_taxid>
 ##      <TSeq_orgname>Chlamydia trachomatis L2c</TSeq_orgname>
 ##      <TSeq_defline>general secretion pathway protein E [Chlamydia trachomatis L2c]</TSeq_defline>
 ##      <TSeq_length>500</TSeq_length>
 ##      <TSeq_sequence>MDGNKGTMQDLLDRLPYSFLKKNYLLPVEDLGDKIVFARHLKKTPLEALDEVRLITQKPLSLVSKEEAEIIHGLQKLYSDKDGKASEMLQSMQEAVVPESESDTTELLENQENSAPVVRLLNLILKEAIEERASDIHFDPVEDLLRIRYRIDGVLHDRHAPPNHLRAALITRIKVLTKLDIAEHRLPQDGRIKLQLGGQEIDMRVSTVPVIHGERVVLRILDKRNVILDIRGLCMPPKMETSFRKAIGVPEGILLVTGPTGSGKTTTLYSVIQHLSGPFTNIMTIEDPPEYKLPGVAQIAVKPKIGLTFSKGLRHLLRQDPDVLMVGEIRDQETAEIAIQAALTGHLVVSTLHTNDAVSAIPRLLDMGVEPYLLSATMIGAVAQRLVRRICTHCKEFCVADVQEQALLRALGKDPFAPLCKGRGCSQCFRSGYKGRQGIYEFVDVTTTLRSEIALGRPYHILRGVAEREGYCPLLEHGVELALAGETTLSEVLRVAKRSE</TSeq_sequence>
 ##    </TSeq>
 ##    <TSeq>
 ##      <TSeq_seqtype value="protein"/>
 ##      <TSeq_gi>220702405</TSeq_gi>
....
```






```r
aa <- cpafx$xmlValue("//TSeq_sequence")
aa

 ##   [1] "MDGNKGTMQDLLDRLPYSFLKKNYLLPVEDLGDKIVFARHLKKTPLEALDEVRLITQKPLSLVSKEEAEIIHGLQKLYSDKDGKASEMLQSMQEAVVPESESDTTELLENQENSAPVVRLLNLILKEAIEERASDIHFDPVEDLLRIRYRIDGVLHDRHAPPNHLRAALITRIKVLTKLDIAEHRLPQDGRIKLQLGGQEIDMRVSTVPVIHGERVVLRILDKRNVILDIRGLCMPPKMETSFRKAIGVPEGILLVTGPTGSGKTTTLYSVIQHLSGPFTNIMTIEDPPEYKLPGVAQIAVKPKIGLTFSKGLRHLLRQDPDVLMVGEIRDQETAEIAIQAALTGHLVVSTLHTNDAVSAIPRLLDMGVEPYLLSATMIGAVAQRLVRRICTHCKEFCVADVQEQALLRALGKDPFAPLCKGRGCSQCFRSGYKGRQGIYEFVDVTTTLRSEIALGRPYHILRGVAEREGYCPLLEHGVELALAGETTLSEVLRVAKRSE"                                                                                   
 ##   [2] "SLVCKNALQDLSFLEHLLQVKYAPKTWKEQYLGWDLVQSSVSAQQKLRTQENPSTSFCQQVLADFIGGLNDFHAGVTFFAIESAYLPYTVQKSSDGRFYFVDIMTFSSEIRVGDELLEVDGAPVQDVLATLYGSNHKGTAAEESAALRTLFSRMASLGHKVPSGRTTLKIRRPFGTTREVRVKWRYVPEGVGDLATIAPSIRAPQLQKSMRSFFPKKDDAFHRSSSLFYSPMVPHFWAELRNHYATSGLKSGYNIGSTDGFLPVIGPVIWESEGLFRAYISSVTDGDGKSHKVGFLRIPTYSWQDMEDFDPSGPPPWEEFAKIIQVFSSNTEALIIDQTNNPGGSVLYLYALLSMLTDRPLELPKHRMILTQDEVVDALDWLTLLENVDTNVESRLALGDNMEGYTVDLQVAEYLKSFGRQVLNCWSKGDIELSTPIPLFGFEKIHPHPRVQYSKPICVLINEQDFACADFFPVVLKDNDRALIVGTRTAGAGGFVFNVQFPNRTGIKTCSLTGSLAVREHGAFIENIGVEPHIDLPFTANDIRYKGYSEYLDKVKKLVCQLINNDGTIILAEDGSFHHHHHH"
 ##   [3] "SLVCKNALQDLSFLEHLLQVKYAPKTWKEQYLGWDLVQSSVSAQQKLRTQENPSTSFCQQVLADFIGGLNDFHAGVTFFAIESAYLPYTVQKSSDGRFYFVDIMTFSSEIRVGDELLEVDGAPVQDVLATLYGSNHKGTAAEESAALRTLFSRMASLGHKVPSGRTTLKIRRPFGTTREVRVKWRYVPEGVGDLATIAPSIRAPQLQKSMRSFFPKKDDAFHRSSSLFYSPMVPHFWAELRNHYATSGLKSGYNIGSTDGFLPVIGPVIWESEGLFRAYISSVTDGDGKSHKVGFLRIPTYSWQDMEDFDPSGPPPWEEFAKIIQVFSSNTEALIIDQTNNPGGSVLYLYALLSMLTDRPLELPKHRMILTQDEVVDALDWLTLLENVDTNVESRLALGDNMEGYTVDLQVAEYLKSFGRQVLNCWSKGDIELSTPIPLFGFEKIHPHPRVQYSKPICVLINEQDFACADFFPVVLKDNDRALIVGTRTAGAGGFVFNVQFPNRTGIKTCSLTGSLAVREHGAFIENIGVEPHIDLPFTANDIRYKGYSEYLDKVKKLVCQLINNDGTIILAEDGSFHHHHHH"
 ##   [4] "SLVCKNALQDLSFLEHLLQVKYAPKTWKEQYLGWDLVQSSVSAQQKLRTQENPSTSFCQQVLADFIGGLNDFHAGVTFFAIESAYLPYTVQKSSDGRFYFVDIMTFSSEIRVGDELLEVDGAPVQDVLATLYGSNHKGTAAEESAALRTLFSRMASLGHKVPSGRTTLKIRRPFGTTREVRVKWRYVPEGVGDLATIAPSIRAPQLQKSMRSFFPKKDDAFHRSSSLFYSPMVPHFWAELRNHYATSGLKSGYNIGSTDGFLPVIGPVIWESEGLFRAYISSVTDGDGKSHKVGFLRIPTYSWQDMEDFDPSGPPPWEEFAKIIQVFSSNTEALIIDQTNNPGGSVLYLYALLSMLTDRPLELPKHRMILTQDEVVDALDWLTLLENVDTNVESRLALGDNMEGYTVDLQVAEYLKSFGRQVLNCWSKGDIELSTPIPLFGFEKIHPHPRVQYSKPICVLINEQDFSCADFFPVVLKDNDRALIVGTRTAGAGGFVFNVQFPNRTGIKTCSLTGSLAVREHGAFIENIGVEPHIDLPFTANDIRYKGYSEYLDKVKKLVCQLINNDGTIILAEDGSFHHHHHH"
 ##   [5] "SLVCKNALQDLSFLEHLLQVKYAPKTWKEQYLGWDLVQSSVSAQQKLRTQENPSTSFCQQVLADFIGGLNDFHAGVTFFAIESAYLPYTVQKSSDGRFYFVDIMTFSSEIRVGDELLEVDGAPVQDVLATLYGSNHKGTAAEESAALRTLFSRMASLGHKVPSGRTTLKIRRPFGTTREVRVKWRYVPEGVGDLATIAPSIRAPQLQKSMRSFFPKKDDAFHRSSSLFYSPMVPHFWAELRNHYATSGLKSGYNIGSTDGFLPVIGPVIWESEGLFRAYISSVTDGDGKSHKVGFLRIPTYSWQDMEDFDPSGPPPWEEFAKIIQVFSSNTEALIIDQTNNPGGSVLYLYALLSMLTDRPLELPKHRMILTQDEVVDALDWLTLLENVDTNVESRLALGDNMEGYTVDLQVAEYLKSFGRQVLNCWSKGDIELSTPIPLFGFEKIHPHPRVQYSKPICVLINEQDFSCADFFPVVLKDNDRALIVGTRTAGAGGFVFNVQFPNRTGIKTCSLTGSLAVREHGAFIENIGVEPHIDLPFTANDIRYKGYSEYLDKVKKLVCQLINNDGTIILAEDGSFHHHHHH"
 ##   [6] "SLVCKNALQDLSFLEHLLQVKYAPKTWKEQYLGWDLVQSSVSAQQKLRTQENPSTSFCQQVLADFIGGLNDFHAGVTFFAIESAYLPYTVQKSSDGRFYFVDIMTFSSEIRVGDELLEVDGAPVQDVLATLYGSNHKGTAAEESAALRTLFSRMASLGHKVPSGRTTLKIRRPFGTTREVRVKWRYVPEGVGDLATIAPSIRAPQLQKSMRSFFPKKDDAFHRSSSLFYSPMVPHFWAELRNHYATSGLKSGYNIGSTDGFLPVIGPVIWESEGLFRAYISSVTDGDGKSHKVGFLRIPTYSWQDMEDFDPSGPPPWEEFAKIIQVFSSNTEALIIDQTNNPGGSVLYLYALLSMLTDRPLELPKHRMILTQDEVVDALDWLTLLENVDTNVESRLALGDNMEGYTVDLQVAEYLKSFGRQVLNCWSKGDIELSTPIPLFGFEKIHPHPRVQYSKPICVLINEQDFSCADFFPVVLKDNDRALIVGTRTAGAGGFVFNVQFPNRTGIKTCSLTGSLAVREHGAFIENIGVEPHIDLPFTANDIRYKGYSEYLDKVKKLVCQLINNDGTIILAEDGSFHHHHHH"
....

defline <- cpafx$xmlValue("//TSeq_defline")
defline

 ##   [1] "general secretion pathway protein E [Chlamydia trachomatis L2c]"
 ##   [2] "Chain B, Crystal Structure Of Cpaf S499a Mutant"                
 ##   [3] "Chain A, Crystal Structure Of Cpaf S499a Mutant"                
 ##   [4] "Chain B, Structure Of Mature Cpaf Complexed With Lactacystin"   
 ##   [5] "Chain A, Structure Of Mature Cpaf Complexed With Lactacystin"   
 ##   [6] "Chain B, Crystal Structure Of Mature Cpaf"                      
....
```




### `einfo`: Information about the Entrez databases ###

You can use `einfo` to obtain a list of all database names accessible through the Entrez utilities:


```r
einfo()

 ##  Object of class 'einfo' 
 ##  List of Entrez databases
 ##   [1] "pubmed"          "protein"         "nuccore"        
 ##   [4] "nucleotide"      "nucgss"          "nucest"         
 ##   [7] "structure"       "genome"          "assembly"       
 ##  [10] "genomeprj"       "bioproject"      "biosample"      
....
```




For each of these databases, we can use `einfo` again to obtain more information:


```r
einfo("taxonomy")

 ##  Object of class 'einfo' 
 ##  Overview over the Entrez database 'Taxonomy'.
 ##    dbName: taxonomy
 ##    MenuName: Taxonomy
 ##    Description: Taxonomy db
 ##    DbBuild: Build130918-1325.1
 ##    Count: 1159149
 ##    LastUpdate: 2013-09-18 16:03:00
 ##    Fields: Name; FullName; Description; TermCount; IsDate; IsNumerical; S...
 ##    Links: Name; Menu; Description; DbTo
....
```




