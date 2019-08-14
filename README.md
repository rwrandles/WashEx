# This repository contains code and data for the Washington State Legislative Explorer Project #

## *Setup*

`R` packages:

  * httr
  * XML
  * dplyr
  * tidyr
  * DBI
  * RSQLite
  * lubridate

## *Data availability*

The current version as of 8/11/2019 contains complete data for all legislation, sponsors, committees, and legislative activity in both the 2015-16 and 2017-18 congresses. 

## *Data Architecture*

The current version as of 8/11/2019 contains a SQLite database named `WashEx-db.sqlite`, which contains all of the data scraped and processed from the Washington State Legislature's [legislative services API](http://wslwebservices.leg.wa.gov) that is implemented in the project. Below is a description of the contents and architecture of the database.

## *Database tables*

* Legislation: contains summary information on each piece of legislation
* Sponsors: contains identifying and demographic information on individual legislators
* Committees: contains information on legislative commitees and their active status
* Actions: contains information on legislative actions carried out on bills throughout their lifetime
* Sessions: contains information on start and end dates for legislative sessions in each congress

### *Data*

Below is a description of the variables contained in each table of the SQLite database

**Legislation**

* BillId: ID of the bill, such as HB 1000 or SSB 5025
* BillNumber: contains the bill number
* Biennium: biennium for the congress in which the bill was active
* LegalTitle: legal title as it appears on the bill at the time of introduction
* IntroducedDate: date of bill introduction, in the form YYYY-MM-DD
* Sponsor: name of the primary sponsor of the bill or substitute bill
* PrimeSponsorID: unique identifier linked to the [same field](#sponsID) in the Sponsors table, unless in the event of a gubernatorial appointment in which case the field will be marked "GOV_YY" with YY representing the year of the biennium
* LongDescription: description of the bill's contents in relation to the topic and intent of the bill
* billUrl: link to a PDF of the bill located on the Washington State Legislature's website

**Sponsors**

* repId<a name="sponsID">: unique identifier of the form XXX_YY, where XXX may contain up to five digits and is unique to the legislator across all terms and YY represents the two digits representing the year of the congress for that particular term
* cong: two-year congress number, representing the *n*th congress since 1889
* FirstName: legislator's first name
* LastName: legislator's last name
* type: type of legislator, where "rep" indicates a representative and "sen" indicates a senator
* District: legislator's home district
* Party: legislator's party affiliation
* Gender: legislator's gender
* Biennium: biennium for the congress in which the legislator was active

**Committees**

* CommitteeId<a name="commID">: unique alphanumeric identifier to locate the particular committee within and between congresses, of the form XY where X is a letter denoting the biennium and Y is a number representing the committee within the congress
* Biennium: biennium for the congress in which the commmittee is represented in the API
* Acronym: committee acronym
* Agency: either "House" or "Senate"
* Name: name of committee
* LongName: combines Agency and Name fields
* Active: in some cases, the API returns committees that did not exist historically, or were not full standing committees on their own. For this reason, any committee with the Active field marked FALSE is not displayed in the visualization

**Actions**

* BillId: ID of the bill, such as HB 1000 or SSB 5025
* Biennium: biennium for the congress in which the legislative action took place
* ActionDate: date on which the legislative action took place
* SessionAct: alphanumeric ID referencing the [Sessions](#sessDates) table. Takes one of three values - "S1" denotes the first regular session, "S2" denotes the second regular session, and "SS" denotes that the action took place during a special session
* locId: alphanumeric identifier linked to the [CommitteeId](#commID) field in the Committees table. Used to locate where the action took place

   *Note: there are several locId's that are not paired to a commmittee but are found in the table:*

   * HRL: House Rules
   * SRL: Senate Rules
   * HFL: House Floor
   * SFL: Senate Floor
   * CNF: Conference
   * SPK: Speaker of the House
   * PRE: President of the Senate
   * DSK: Governor's Desk
   * PVT: Governor Partial Veto
   * VET: Governor Veto
   * SGN: Governor Signed
   * OVR: Veto Override
   * LAW: Became Law

* HistoryLine: text from the API detailing legislative action

**Sessions<a name="SessDates">**

* biennium: biennium for the congress listed
* cong: two-year congress number, representing the *n*th congress since 1889
* StartDate_S1: beginning date of the first regular session of the congress, in the form YYYY-MM-DD
* EndDate_S1: end date of the first regular session of the congress, in the form YYYY-MM-DD
* StartDate_S2: beginning date of the second regular session of the congress, in the form YYYY-MM-DD
* EndDate_S2: end date of the second regular session of the congress, in the form YYYY-MM-DD
