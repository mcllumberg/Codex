* ZHR_BIRTHDAYS_REPORT
* ABAP OO report using LDB PNPCE
* Lists employees with birthdays in a selected period
* and emails the list based on org assignment
* (old syntax, suitable for older releases)

REPORT zhr_birthdays_report
       LINE-SIZE 132
       NO STANDARD PAGE HEADING.

* Use logical database PNPCE
TABLES: pernr.
NODES:  pernr.
INFOTYPES: 0001, 0002.

PARAMETERS: p_begda TYPE sy-datum DEFAULT sy-datum,
            p_endda TYPE sy-datum DEFAULT sy-datum.

DATA: go_bday TYPE REF TO lcl_birthday.

START-OF-SELECTION.
  CREATE OBJECT go_bday
    EXPORTING
      i_pbegda = p_begda
      i_pendda = p_endda.

GET pernr.
  go_bday->process_pernr( ).

END-OF-SELECTION.
  go_bday->display_alv( ).
  go_bday->send_email( ).
