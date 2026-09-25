


Update Instructions:

To update the acreage data, open fsa_acreage_urls.xlsx and add a row
corresponding to any new file releases on FSA's crop acreage data webpage:
https://www.fsa.usda.gov/tools/informational/freedom-information-act-foia/electronic-reading-room/frequently-requested/crop-acreage-data

After adding the new row, save and close fsa_acreage_urls.xlsx

Run fsaCropAcreageCode.R from the package root with rfsa loaded. It downloads
every listed release to Raw_Data and saves each cleaned release separately in
Cleaned_Data. Existing cached releases are retained. The packaged datasets
fsaCropAcreage, fsaCropAcreageCC, and fsaCoveredCommodityShares use only the
latest release for each crop year.

The manifest's year/month/day/date fields identify the release, not the crop
year. January releases refer to the preceding crop year. For historical source
labels that specify only a month, date uses the first of that month as a cache
identifier; it is not a verified publication day.

Crop year 2012 includes August, September, October, November, revised December
2012, and final January 2013 releases. The September, October, and January
workbooks spell Failed Acres as Failded Acres; the importer corrects that header.
Literal NULL crop types are normalized to NA before extracting types from crop
names. The August workbook has an entirely blank Crop Codes column, which
remains missing.
The September ZIP on the current FSA site is truncated. Its manifest entry uses
the intact archive on www.old.fsa.usda.gov. Keep that complete URL unchanged.

Crop year 2013 also includes every published release: August through December
2013 and final January 2014. The September source is USDA's revised workbook.
The five in-season releases were absent from the original manifest, which began
in January 2014; they are now available as individual cleaned RDS files.
The November 2013 source leaves Planted and Failed Acres blank in 344 rows
(3 Wyoming, 1 Marianas, 340 Puerto Rico). These source NAs are preserved;
Planted Acres and Failed Acres are complete and can be added when that total
is needed. Prevented Acres is complete in all five added releases.

Some later monthly gaps reflect actual publication gaps: FSA did not post an
October 2021 report and did not release October or November 2025 reports because
of the government shutdown. Keep those observations missing in monthly figures;
do not substitute zero acreage or a different release month.

FSA used the Not Planted reporting category in 2011 and 2012 only. The 2012
values are preserved as reported. Comparisons of total planted acreage and
covered-commodity shares with 2013 onward must account for this reporting break.


For questions, contact Dylan Turner (dylan.turner@ndsu.edu)
