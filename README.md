# This repository contains code and data for the Washington State Legislative Explorer Project #

## *Setup*

`R` packages:

  * lubridate
  * RSQLite
  * ini
  * snakecase
  * dplyr
  * tidyr
  * zoo
  * dbplyr
  * RMySQL
  * mailR

## *Data availability*

The current version as of 1/23/2020 contains complete data for all legislation, sponsors, committees, sessions, and legislative activity since the 2007-08 term.

## *Files*

The project consists of three `R` scripts, one of which contains two variations depending on the output destination. It relies on two helper scripts titled `WashEx-source-functions.R` and `WashEx-buildHandler.R`. The two variations of the main script are `source-forDatabases.R` for builds that are offloading to a MySQL server such as Amazon AWS, and `source-forLocal.R` for data stored locally. Each of the files are described in brief below:

  * `WashEx-source-functions.R` contains the functions necessary for scraping and processing data from the API

  * `WashEx-buildHandler.R` contains code that handles errors during runtime. It is responsible for logging and transmitting error messages, as well as ceasing the script in the event of an error.

  * `source-forDatabases.R`/`source-forLocal.R` is the main script that downloads data from the API, parses the information, and assembles it into the properly formatted databases. Currently, this file will take the existing database and update its information with any new information occuring within the current year. 

## *Data Architecture*

The current version as of 1/23/2020 contains an SQLite database named `WashEx-db.sqlite`, which contains all of the data scraped and processed from the Washington State Legislature's [legislative services API](http://wslwebservices.leg.wa.gov) that is implemented in the project. Below is a description of the contents and architecture of the database.

## *Accessory Files*

There are six accessory data files included in the project. These files are included in the `Source Data` folder and are listed below

* `comm_changefile.csv:` contains a list of committees that need to be manually swapped in order to maintain consistency within a biennium (i.e. committees that are renamed during the term can be merged into one)
* `commLeaders.csv:` contains information for each legislator and whether they are the chair or vice-chair of a particular committee
* `legMajorities.csv:` contiains the majority party for each year in both the House and Senate
* `sessDates.csv:` contains start and end dates for all regular and special sessions in each biennium
* `sponsor_gender.csv:` contains a binary variable for the gender of each legislator
* `WAgov.csv:` contains general demographic information for the governor for each year

## *Database Tables*

* Legislation: contains summary information on each piece of legislation
* Sponsors: contains identifying and demographic information on individual legislators
* Committees: contains information on legislative commitees and their active status
* Actions: contains information on legislative actions carried out on bills throughout their lifetime
* Sessions: contains information on start and end dates for legislative sessions in each congress

### *Data*

Below is a description of the variables contained in each table of the SQLite database

**Legislation**

* billId: ID of the bill, such as HB 1000 or SSB 5025
* billType: contains the bill's type, based on the prefix. The following types are included:

   * Appointment
   * Bill
   * Concurrent Resolution
   * Initiative
   * Joint Memorial
   * Joint Resolution
   * Resolution

* billNumber: contains the bill number
* biennium: biennium for the congress in which the bill was active
* legalTitle: legal title as it appears on the bill at the time of introduction
* introducedDate: date of bill introduction, in the form YYYY-MM-DD
* primeSponsorID: unique identifier linked to the [same field](#sponsID) in the Sponsors table, unless in the event of a gubernatorial appointment or initiative in which case the field will be marked "GOV_YY" or "PPL_YY", respectively, with YY representing the year of the biennium
* majSpons: boolean flag indicating whether the bill's primary sponsor was a member of the majority party
* isMinor: boolean flag indicating whether the bill is consider a "minor bill". Minor bills are defined as those whose legal title contains any of the following phrases:

   * Acknowledging
   * Asking
   * Calling
   * Celebrating
   * Commending
   * Congratulating
   * Declaring
   * Designating
   * Honoring
   * Naming
   * Objecting to
   * Observing
   * Proclaiming
   * Recognizing
   * Renaming
   * Requesting
   * Technical correction
   * Thanking
   * Urging

* longDescription: description of the bill's contents in relation to the topic and intent of the bill
* billUrl: link to a PDF of the bill located on the Washington State Legislature's website
* firstRef: long name of the committee of original referral
* isChair: boolean flag indicating whether the primary sponsor was the chair of the committee of original referral
* isVice: boolean flag indicating whether the primary sponsor was the vice-chair of the committee of original referral
* isMem: boolean flag indicating whether the primary sponsor was a member of the committee of original referral
* passedHouse: boolean flag indicating whether the bill was passed by the House
* passedSenate: boolean flag indicating whether the bill was passed by the Senate
* becameLaw: boolean flag indicating whether the bill became law
* bounces: total number of legislative steps taken by the bill

**Sponsors**

* repId<a name="sponsID">: unique identifier of the form XXX_YY, where XXX may contain up to five digits and is unique to the legislator across all terms and YY represents the two digits representing the year of the congress for that particular term
* id: identifier of the form XXX, where XXX may contain up to five digits and is unique to the legislator across all terms
* leg: number representing the *n*th legislature since 1889
* firstName: legislator's first name
* lastName: legislator's last name
* type: type of legislator, where "rep" indicates a representative and "sen" indicates a senator
* district: legislator's home district
* party: legislator's party affiliation
* gender: legislator's gender
* biennium: biennium for the congress in which the legislator was active
* isEastDistrict: boolean flag indicating whether the legislator belongs to a district east of the cascades (Districts 3,4,6,7,8,9,12,13,14,15,16)
* isSNDistrict: boolean flag indicating whether the legislator belongs to a district that falls under the "Space Needle Effect" (is visible from the top of the Space Needle - Districts 1,5,11,24,30,31,32,33,34,36,37,39,40,41,43,45,46,47,48)
* numTerms: represents the nth term served in a given chamber
* class: represents whether the legislator belongs to a class "A" or class "B" district (only relevant for Senators). Washington Senators are elected to four-year terms and are elected in staggered elections. Class "A" elections are held in general election years and class "B" elections are held in midterm election years

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
   * CON: Confirmed
   * FIL: Filed with Secretary of State
   * ADP: Adopted
   * LAW: Became Law

* historyLine: text from the API detailing legislative action
* sessionAct: alphanumeric ID referencing the [Sessions](#sessTitle) table. Takes on a value linked to a particular session during the biennium. Sessions marked "R" indicate regular sessions, while "S" indicates a special session. *For example, a session marked "S3" indicates the third special session of that biennium*

**Sessions**

* leg: number representing the *n*th legislature since 1889
* biennium: biennium for the legislature listed
* sessTitle<a name="sessTitle">: alphanumeric code for the particular session. Sessions marked "R" indicate regular sessions, while "S" indicates special sessions. *For example, a session marked "S3" indicates the third special session of that biennium*
* dateStart: start date of the session in ymd-date form
* dateEnd: end date of the session in ymd-date form
