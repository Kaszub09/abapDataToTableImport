*&---------------------------------------------------------------------*
*& Report zdtti_example_read_from_file
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdtti_example_read_to_many_ce.

"Sample target structure with many conversion exits
TYPES:
  BEGIN OF t_target,
    spras TYPE spras,
    meins TYPE meins,
    matnr TYPE matnr,
    parvw TYPE parvw,
    kunnr TYPE kunnr,
    ajahr TYPE ajahr,
  END OF t_target,
  tt_target TYPE STANDARD TABLE OF t_target WITH EMPTY KEY.

DATA target_table TYPE tt_target.
DATA(target) = zcl_dtti_target_factory=>create_from_ref( REF #( target_table ) ).

"Get mapping
DATA(dtti) = NEW zcl_data_to_table_import( ).
DATA(result) = dtti->run_mapping( source = zcl_dtti_source_factory=>create_from_user_input( ) target = target ).

"Display mapping result
DATA(alv) = NEW zcl_ea_salv_table(  ).
alv->set_data( target->get_target_table( ) ).
alv->display_data( ).
