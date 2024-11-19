CLASS zcl_dtti_target_base DEFINITION PUBLIC CREATE PRIVATE
  GLOBAL FRIENDS zcl_dtti_target_factory zcl_dtti_target_from_ref.

  PUBLIC SECTION.
    INTERFACES:
      zif_dtti_target.

  PROTECTED SECTION.
    DATA:
      target_table TYPE REF TO data,
      table_info   TYPE zif_dtti_target~tt_target.

  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_dtti_target_base IMPLEMENTATION.
  METHOD zif_dtti_target~get_target_table.
    IF me->target_table IS NOT BOUND.
      zif_dtti_target~refresh_table_structure( ).
    ENDIF.
    target_table = me->target_table.
  ENDMETHOD.

  METHOD zif_dtti_target~get_target_table_info.
    info = table_info.
  ENDMETHOD.

  METHOD zif_dtti_target~set_target_table_info.
    table_info = info.
  ENDMETHOD.

  METHOD zif_dtti_target~refresh_table_structure.
    DATA(struct_descr) = cl_abap_structdescr=>get( VALUE #( FOR field IN table_info ( name = field-field type = field-type ) ) ).
    DATA(table_descr) = cl_abap_tabledescr=>get( p_line_type = struct_descr p_key_kind = cl_abap_tabledescr=>keydefkind_empty ).

    CREATE DATA target_table TYPE HANDLE table_descr.
  ENDMETHOD.
  METHOD zif_dtti_target~set_field_info.
    IF line_exists( table_info[ KEY field field = field_info-field ] ).
      DELETE table_info WHERE field = field_info-field.
    ENDIF.
    INSERT field_info INTO TABLE table_info.
  ENDMETHOD.

  METHOD zif_dtti_target~set_field_is_hidden.
    table_info[ KEY field field = field ]-is_hidden = is_hidden.
  ENDMETHOD.

  METHOD zif_dtti_target~set_field_is_key.
    table_info[ KEY field field = field ]-is_key = is_key.
  ENDMETHOD.

  METHOD zif_dtti_target~set_field_is_required.
    table_info[ KEY field field = field ]-is_required = is_required.
  ENDMETHOD.

  METHOD zif_dtti_target~set_field_description.
    table_info[ KEY field field = field ]-field_description = description.
  ENDMETHOD.

ENDCLASS.
