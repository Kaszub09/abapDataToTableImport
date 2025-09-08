CLASS zcl_dtti_target_base DEFINITION PUBLIC CREATE PRIVATE
  GLOBAL FRIENDS zcl_dtti_target_factory zcl_dtti_target_from_ref.

  PUBLIC SECTION.
    INTERFACES:
      zif_dtti_target.

  PROTECTED SECTION.
    DATA:
      refresh_needed TYPE abap_bool,
      target_table   TYPE REF TO data,
      table_info     TYPE zif_dtti_target~tt_target.

  PRIVATE SECTION.
    METHODS:
      get_struct_descr IMPORTING prefix TYPE fieldname RETURNING VALUE(struct_descr) TYPE REF TO cl_abap_structdescr.

ENDCLASS.

CLASS zcl_dtti_target_base IMPLEMENTATION.
  METHOD zif_dtti_target~get_target_table.
    IF me->target_table IS NOT BOUND OR refresh_needed = abap_true.
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
    DATA(components) = VALUE cl_abap_structdescr=>component_table( ).
    LOOP AT table_info REFERENCE INTO DATA(field_info).
      IF NOT field_info->field CP '*-*'.
        APPEND VALUE #( name = field_info->field type = field_info->type ) TO components.
        CONTINUE.
      ENDIF.

      SPLIT field_info->field AT '-' INTO DATA(struct_name) DATA(dummy).
      IF line_exists( components[ name = struct_name ] ).
        CONTINUE.
      ENDIF.

      APPEND VALUE #( name = struct_name type = get_struct_descr( CONV #( struct_name ) ) ) TO components.
    ENDLOOP.

    DATA(struct_descr) = cl_abap_structdescr=>get( components ).
    DATA(table_descr) = cl_abap_tabledescr=>get( p_line_type = struct_descr p_key_kind = cl_abap_tabledescr=>keydefkind_empty ).

    CREATE DATA target_table TYPE HANDLE table_descr.
    refresh_needed = abap_false.
  ENDMETHOD.
  METHOD zif_dtti_target~set_field_info.
    IF line_exists( table_info[ KEY field field = field_info-field ] ).
      DELETE table_info WHERE field = field_info-field.
    ENDIF.
    INSERT field_info INTO TABLE table_info.
    refresh_needed = abap_true.
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

  METHOD zif_dtti_target~set_currency_field.
    table_info[ KEY field field = field ]-currency_field = currency_field.
  ENDMETHOD.

  METHOD zif_dtti_target~remove_field.
    DELETE table_info USING KEY field WHERE field = field.
    refresh_needed = abap_true.
  ENDMETHOD.

  METHOD get_struct_descr.
    DATA(components) = VALUE cl_abap_structdescr=>component_table( ).

    LOOP AT table_info REFERENCE INTO DATA(field_info) WHERE field CP |{ prefix }-*|.
      DATA(field) = substring( val = field_info->field off = strlen( prefix ) len = strlen( field_info->field ) - strlen( prefix ) ).
      SPLIT field AT '-' INTO DATA(dummy) DATA(struct_field) DATA(field_subfield).
      IF field_subfield IS INITIAL.
        APPEND VALUE #( name = struct_field type = field_info->type ) TO components.
        CONTINUE.
      ELSEIF line_exists( components[ name = struct_field ] ).
        CONTINUE.
      ENDIF.

      APPEND VALUE #( name = struct_field type = get_struct_descr( CONV #( |{ prefix }-{ struct_field }| ) ) ) TO components.
    ENDLOOP.

    struct_descr = cl_abap_structdescr=>get( components ).
  ENDMETHOD.

ENDCLASS.
