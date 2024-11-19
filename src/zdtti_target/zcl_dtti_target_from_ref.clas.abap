CLASS zcl_dtti_target_from_ref DEFINITION PUBLIC INHERITING FROM zcl_dtti_target_base CREATE PRIVATE GLOBAL FRIENDS zcl_dtti_target_factory.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING target_table TYPE REF TO data.
  PRIVATE SECTION.
    METHODS:
      get_target_info IMPORTING struct_descr TYPE REF TO cl_abap_structdescr RETURNING VALUE(info) TYPE zif_dtti_target=>tt_target.
ENDCLASS.

CLASS zcl_dtti_target_from_ref IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    me->target_table = target_table.

    DATA(table_descr) = CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data_ref( target_table ) ).
    DATA(struct_descr) = CAST cl_abap_structdescr( table_descr->get_table_line_type( ) ).
    APPEND LINES OF get_target_info( struct_descr ) TO me->table_info.

    "We can read key fields if table is from DDIC
    IF struct_descr->is_ddic_type( ).
      LOOP AT struct_descr->get_ddic_field_list( ) REFERENCE INTO DATA(ddic_field) WHERE keyflag = abap_true.
        table_info[ KEY field field = ddic_field->fieldname ]-is_key = abap_true.
        table_info[ KEY field field = ddic_field->fieldname ]-is_required = abap_true.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD get_target_info.
    LOOP AT struct_descr->get_components( ) REFERENCE INTO DATA(comp).
      IF comp->as_include = abap_true.
        APPEND LINES OF get_target_info( CAST #( comp->type ) ) TO info.
        CONTINUE.
      ENDIF.
      IF comp->type->is_ddic_type( ) AND comp->type IS INSTANCE OF cl_abap_elemdescr.
        DATA(de_descr) = CAST cl_abap_elemdescr( comp->type ).
        APPEND VALUE #( field = comp->name type = comp->type is_ddic = abap_true field_description = de_descr->get_ddic_field( )-scrtext_l ) TO info.
      ELSE.
        APPEND VALUE #( field = comp->name type = comp->type is_ddic = abap_false field_description = comp->name ) TO info.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
