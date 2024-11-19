*&---------------------------------------------------------------------*
*& Report zdtti_example_read_from_file
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdtti_example_read_from_table.

"Sample target structure
SELECT FROM sflight FIELDS carrid, connid, price, seatsmax, fldate INTO TABLE @DATA(sflight_tab).

DATA(target) = zcl_dtti_target_factory=>create_from_info( VALUE #(
    ( field = 'OBJ_NAME' field_description = |Overwrite default description| type = CAST #( cl_abap_datadescr=>describe_by_name( 'SOBJ_NAME' ) ) is_required = abap_true )
    ( field = 'SRCSYSTEM' type = CAST #( cl_abap_datadescr=>describe_by_name( 'SRCSYSTEM' ) ) )
    ( field = 'CREATED_ON' type = CAST #( cl_abap_datadescr=>describe_by_name( 'CREATIONDT' ) ) ) ) ).

"Get mapping
DATA(dtti) = NEW zcl_data_to_table_import( ).
DATA(result) = dtti->run_mapping( source = zcl_dtti_source_factory=>create_from_ref( REF #( sflight_tab ) ) target = target ).

"Display mapping result
DATA(alv) = NEW zcl_ea_salv_table(  ).
alv->set_data( target->get_target_table( ) ).
alv->display_data( ).
