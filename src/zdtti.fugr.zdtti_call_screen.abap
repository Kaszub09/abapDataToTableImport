FUNCTION zdtti_call_screen.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(SCREEN_NR) LIKE  SY-DYNNR
*"     REFERENCE(COL1) TYPE  I DEFAULT 0
*"     REFERENCE(LINE1) TYPE  I DEFAULT 0
*"     REFERENCE(COL2) TYPE  I DEFAULT 0
*"     REFERENCE(LINE2) TYPE  I DEFAULT 0
*"----------------------------------------------------------------------
  CALL SCREEN screen_nr STARTING AT col1 line1 ENDING AT col2 line2.


ENDFUNCTION.
