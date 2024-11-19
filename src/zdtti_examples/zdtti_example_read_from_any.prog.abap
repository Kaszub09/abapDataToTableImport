*&---------------------------------------------------------------------*
*& Report zdtti_example_read_from_file
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdtti_example_read_from_any.

"Sample target structure
DATA tadir_tab TYPE STANDARD TABLE OF tadir.
DATA(target) = zcl_dtti_target_factory=>create_from_ref( REF #( tadir_tab ) ).

"Data source from user
DATA(source) = zcl_dtti_source_factory=>create_from_user_input( ).
IF source IS NOT BOUND.
  MESSAGE TEXT-001 TYPE 'E'.
ENDIF.

"Get mapping
DATA(dtti) = NEW zcl_data_to_table_import( ).
DATA(result) = dtti->run_mapping( source = source target = target ).

"Display mapping result
DATA(alv) = NEW zcl_ea_salv_table(  ).
alv->set_data( REF #( tadir_tab ) ).
alv->display_data( ).
