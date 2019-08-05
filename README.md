# WashEx

This is a summary of the information contained in each table that the code generates.

**Legislation:** This contains a bill identifier (“BillId”), the biennium of that bill (“Biennium”), the introduction date (“IntroducedDate”), the bill’s sponsor (“Sponsor”), and the URL to the bill's page on the Washington State Legislature's website ("billUrl).

**Sponsors:** This contains the first and last name of legislature members for each biennium, as well as their identifier (“Id”), whether they were a senator or representative (“type”), their district, the congress they served in, and their party.

**Committees:** This contains the committee name (i.e., “Finance”), its location within the legislature (i.e. House or Senate) (“Agency”), the committee’s acronym, the committee’s biennium, and a committee ID that is unique to a specific committee in a specific biennium (“CommitteeId”).

**Actions:** The column “HistoryLine” contains each action taken for a given bill (“BillID”). This table records the day the action happened (“ActionDate”), the biennium it happened in, and where the action sent the bill ("locId" -- this is directly paired with "CommitteeId" from the Committee table).

*Additional locId's that are not paired to a committee but are found in the table:*

- HFL: House Floor
- SFL: Senate Floor
- CNF: Conference
- SPK: Speaker of the House
- PRE: President of the Senate
- DSK: Governor's Desk
- PVT: Governor Partial Veto
- VET: Governor Veto
- SGN: Governor Singed
- LAW: Became Law
