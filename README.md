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
  * zoo
  * snakecase

## *Data availability*

The current version as of 8/30/2019 contains complete data for all legislation, sponsors, committees, and legislative activity in both the 2015-16 and 2017-18 congresses. 

## *Data Architecture*

The current version as of 8/30/2019 contains an SQLite database named `WashEx-db.sqlite`, which contains all of the data scraped and processed from the Washington State Legislature's [legislative services API](http://wslwebservices.leg.wa.gov) that is implemented in the project. Below is a description of the contents and architecture of the database.

## *Database tables*

* Legislation: contains summary information on each piece of legislation
* Sponsors: contains identifying and demographic information on individual legislators
* Committees: contains information on legislative commitees and their active status
* Actions: contains information on legislative actions carried out on bills throughout their lifetime
* Sessions: contains information on start and end dates for legislative sessions in each congress

### *Data*

Below is a description of the variables contained in each table of the SQLite database

**Legislation**

* billId: ID of the bill, such as HB 1000 or SSB 5025
* billType: contains the bill prefix, indicating the type of legislation based on the final characters:

  * B: Bill
  * JM: Joint Motion
  * JR: Joint Resolution
  * CR: Concurrent Resolution
  * R: Resolution
  * GA: Gubernatorial Appointment
  * I: Initiative

* billNumber: contains the bill number
* biennium: biennium for the congress in which the bill was active
* legalTitle: legal title as it appears on the bill at the time of introduction
* introducedDate: date of bill introduction, in the form YYYY-MM-DD
* Sponsor: name of the primary sponsor of the bill or substitute bill
* primeSponsorID: unique identifier linked to the [same field](#sponsID) in the Sponsors table, unless in the event of a gubernatorial appointment in which case the field will be marked "GOV_YY" with YY representing the year of the biennium
* majSpons: boolean flag for whether the bill's primary sponsor was a member of the majority party
* longDescription: description of the bill's contents in relation to the topic and intent of the bill
* billUrl: link to a PDF of the bill located on the Washington State Legislature's website
* firstRef: long name of the committee of original referral
* isChair: boolean flag for whether the primary sponsor was the chair of the committee of original referral
* isVice: boolean flag for whether the primary sponsor was the vice-chair of the committee of original referral
* isMem: boolean flag for whether the primary sponsor was a member of the committee of original referral

**Sponsors**

* repId<a name="sponsID">: unique identifier of the form XXX_YY, where XXX may contain up to five digits and is unique to the legislator across all terms and YY represents the two digits representing the year of the congress for that particular term
* leg: number representing the *n*th legislature since 1889
* firstName: legislator's first name
* lastName: legislator's last name
* type: type of legislator, where "rep" indicates a representative and "sen" indicates a senator
* district: legislator's home district
* party: legislator's party affiliation
* gender: legislator's gender
* biennium: biennium for the congress in which the legislator was active
* termStart: the ymd-date indicating the date that begins the legislator's term; for legislators whose term begins at the beginning of the legislature, the date is XXXX-01-01
* termEnd: the ymd-date indicating the date that ends the legislator's term; for legislators whose term lasts through the entire legislature, the date is XXXX-12-31

**Committees**

* commId<a name="commID">: unique alphanumeric identifier to locate the particular committee within and between congresses, of the form XY where X is a letter denoting the biennium and Y is a number representing the committee within the congress
* biennium: biennium for the congress in which the commmittee is represented in the API
* acronym: committee acronym
* agency: either "House" or "Senate"
* name: name of committee
* longName: combines Agency and Name fields
* active: in some cases, the API returns committees that did not exist historically, or were not full standing committees on their own. For this reason, any committee with the Active field marked FALSE is not displayed in the visualization

**Actions**

* billId: ID of the bill, such as HB 1000 or SSB 5025
* biennium: biennium for the congress in which the legislative action took place
* actionDate: date on which the legislative action took place
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

* historyLine: text from the API detailing legislative action
* sessionAct: alphanumeric ID referencing the [Sessions](#sessTitle) table. Takes on a value linked to a particular session during the biennium. Sessions marked "R" indicate regular sessions, while "S" indicates a special session. *For example, a session marked "S3" indicates the third special session of that biennium*

**Sessions**

* leg: number representing the *n*th legislature since 1889
* biennium: biennium for the legislature listed
* sessTitle<a name="sessTitle">: alphanumeric code for the particular session. Sessions marked "R" indicate regular sessions, while "S" indicates special sessions. *For example, a session marked "S3" indicates the third special session of that biennium*
* dateStart: start date of the session in ymd-date form
* dateEnd: end date of the session in ymd-date form