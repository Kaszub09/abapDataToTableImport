*"* use this source file for any macro definitions you need
*"* in the implementation part of the class
DEFINE ref_to_tab_fs.
  FIELD-SYMBOLS &2 TYPE table.
  ASSIGN &1->* TO &2.
end-of-DEFINITION.
