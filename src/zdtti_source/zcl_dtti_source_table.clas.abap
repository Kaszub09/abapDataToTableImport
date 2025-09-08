CLASS zcl_dtti_source_table DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_dtti_source_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_dtti_source.
    METHODS:
      constructor IMPORTING source_table TYPE REF TO data is_first_row_headers TYPE abap_bool DEFAULT abap_false.
  PRIVATE SECTION.
    DATA:
      source_table TYPE REF TO data.
ENDCLASS.

CLASS zcl_dtti_source_table IMPLEMENTATION.
  METHOD constructor.
    me->source_table = source_table.

    DATA(table_descr) = CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data_ref( source_table ) ).
    DATA(struct_descr) = CAST cl_abap_structdescr( table_descr->get_table_line_type( ) ).
    DATA(source_components) = struct_descr->get_components( ).

    IF is_first_row_headers = abap_true.
      "Get field descriptions from first row
      FIELD-SYMBOLS <table> TYPE table.
      ASSIGN source_table->* TO <table>.
      ASSIGN <table>[ 1 ] TO FIELD-SYMBOL(<first_row>).

      LOOP AT source_components REFERENCE INTO DATA(comp).
        ASSIGN COMPONENT comp->name OF STRUCTURE <first_row> TO FIELD-SYMBOL(<description>).
        APPEND VALUE #( field = comp->name description = <description> is_ddic = abap_false ) TO zif_dtti_source~source_field_info.
      ENDLOOP.

      DELETE <table> INDEX 1.

    ELSE.
      "Try getting field description from DDIC, otherwise same as name
      LOOP AT source_components REFERENCE INTO comp.
        IF comp->type->is_ddic_type( ) AND comp->type IS INSTANCE OF cl_abap_elemdescr.
          DATA(ddic_field) = CAST cl_abap_elemdescr( comp->type )->get_ddic_field( ).
          APPEND VALUE #( field = comp->name description = ddic_field-scrtext_l is_ddic = abap_true ) TO zif_dtti_source~source_field_info.
        ELSE.
          APPEND VALUE #( field = comp->name description = comp->name is_ddic = abap_false ) TO zif_dtti_source~source_field_info.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD zif_dtti_source~get_source_table.
    source_table = me->source_table.
  ENDMETHOD.

ENDCLASS.
