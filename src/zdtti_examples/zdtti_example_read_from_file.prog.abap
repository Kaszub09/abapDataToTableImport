*&---------------------------------------------------------------------*
*& Report zdtti_example_read_from_file
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdtti_example_read_from_file.

"Sample target structure

TYPES:
  BEGIN OF t_target,
    date_field     TYPE d,
    some_name      TYPE fieldname,
    int_field      TYPE i,
    packed_field   TYPE p LENGTH 10 DECIMALS 2,
    another_column TYPE string,
  END OF t_target,
  tt_target TYPE STANDARD TABLE OF t_target WITH EMPTY KEY.
DATA:
    target_tab TYPE tt_target.

DATA(target) = zcl_dtti_target_factory=>create_from_ref( REF #( target_tab ) ).
"Set in code or create (or use existing) data elements if needed
target->set_field_description( field = 'ANOTHER_COLUMN' description = |Another column| ).
target->set_field_description( field = 'INT_FIELD' description = |Some int column| ).

"Data source from file
DATA(source) = zcl_dtti_source_factory=>create_from_file( ).
IF source IS NOT BOUND.
  MESSAGE TEXT-001 TYPE 'E'.
ENDIF.

"Get mapping
DATA(dtti) = NEW zcl_data_to_table_import( ).
DATA(result) = dtti->run_mapping( source = source target = target ).

"Display mapping result
DATA(alv) = NEW zcl_ea_salv_table(  ).
alv->set_data( target->get_target_table( ) ).
alv->display_data( ).
