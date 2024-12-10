CLASS zcl_dtti_source_alv DEFINITION PUBLIC INHERITING FROM zcl_ea_alv_table CREATE PRIVATE GLOBAL FRIENDS zcl_data_to_table_import.

  PUBLIC SECTION.
    INTERFACES:
      zif_dtti_change_mapping_event.

    METHODS:
      constructor IMPORTING source TYPE REF TO zif_dtti_source container TYPE REF TO cl_gui_container
                            layout_key TYPE salv_s_layout_key OPTIONAL report_id  TYPE sy-repid DEFAULT sy-cprog,
      "! <p class="shorttext synchronized" lang="en">Call after mapping to display errors</p>
      update_row_info IMPORTING mapping_result TYPE zcl_dtti_mapper=>tt_mapping_result.
    DATA:
      "! <p class="shorttext synchronized" lang="en">Source tab with columns with additional info</p>
      source_tab_ext TYPE REF TO data READ-ONLY,
      container      TYPE REF TO cl_gui_container.

    EVENTS:
        source_data_changed.
  PROTECTED SECTION.
    METHODS:
      on_button_click REDEFINITION,
      on_drag REDEFINITION,
      on_drop REDEFINITION,
      on_added_function REDEFINITION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_col,
        exception  TYPE fieldname VALUE 'EXCEPTION_____________________',
        cell_style TYPE fieldname VALUE 'CELL_STYLE____________________',
        color      TYPE fieldname VALUE 'COLOR_________________________',
        comment    TYPE fieldname VALUE 'COMMENT_______________________',
      END OF c_col,
      BEGIN OF c_functions,
        change_edit_mode TYPE syst_ucomm VALUE 'CHANGE_EDIT_MODE',
        refresh          TYPE syst_ucomm VALUE 'REFRESH',
        restore_initial  TYPE syst_ucomm VALUE 'RESTORE_INITIAL',
      END OF c_functions.

    METHODS:
      prepare_tab IMPORTING source_tab TYPE REF TO data,
      prepare_columns,
      prepare_button_row,
      change_edit_mode.

    DATA:
      is_in_edit_mode   TYPE abap_bool VALUE abap_false,
      source            TYPE REF TO zif_dtti_source,
      source_components TYPE cl_abap_structdescr=>component_table.
ENDCLASS.

CLASS zcl_dtti_source_alv IMPLEMENTATION.
  METHOD constructor.
    super->constructor( layout_key = layout_key report_id = report_id ).
    me->source = source.
    me->container = container.
    set_container( container ).

    DATA(table_descr) = CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data_ref( source->get_source_table( ) ) ).
    DATA(struct_descr) = CAST cl_abap_structdescr( table_descr->get_table_line_type( ) ).
    source_components = struct_descr->get_components( ).

    prepare_tab( source->get_source_table( ) ).
    set_data( source_tab_ext ).
    prepare_columns( ).
    set_header( header = TEXT-001 header_size = 'X' ).
    me->grid_layout-sel_mode = 'A'.
    me->grid_layout-no_rowmark = abap_true.
    display_data( ).

    is_in_edit_mode = abap_true.
    change_edit_mode( ).
  ENDMETHOD.

  METHOD prepare_tab.
    DATA(components) = source_components.

    INSERT VALUE #( name = c_col-exception type = CAST #( cl_abap_datadescr=>describe_by_name( 'CHAR1' ) ) ) INTO components INDEX 1.
    APPEND VALUE #( name = c_col-comment type = CAST #( cl_abap_datadescr=>describe_by_name( 'STRING' ) ) ) TO components.
    APPEND VALUE #( name = c_col-cell_style type = CAST #( cl_abap_datadescr=>describe_by_name( 'LVC_T_STYL' ) ) ) TO components.
    APPEND VALUE #( name = c_col-color type = CAST #( cl_abap_datadescr=>describe_by_name( 'LVC_T_SCOL' ) ) ) TO components.

    DATA(struct_ext_descr) = cl_abap_structdescr=>get( components ).
    DATA(table_ext_descr) = cl_abap_tabledescr=>get( p_line_type = struct_ext_descr p_key_kind = cl_abap_tabledescr=>keydefkind_empty ).

    CREATE DATA source_tab_ext TYPE HANDLE table_ext_descr.

    ref_to_tab_fs source_tab_ext <source_ext>.
    ref_to_tab_fs source_tab <source>.
    <source_ext> = CORRESPONDING #( <source> ).

    "Remove empty rows
    DATA(where_initial) = REDUCE #( INIT where = || FOR component IN source_components NEXT where = |{ where }AND { component-name } IS INITIAL | ).
    IF strlen( where_initial ) > 0.
      where_initial = substring( val = where_initial off = 3 ).
      DELETE <source_ext> WHERE (where_initial).
    ENDIF.
  ENDMETHOD.

  METHOD prepare_columns.
    LOOP AT source->source_field_info REFERENCE INTO DATA(source_field_info).
      IF source_field_info->is_ddic = abap_false.
        columns->set_fixed_text( column = source_field_info->field text = source_field_info->description ).
      ENDIF.
    ENDLOOP.
    columns->set_fixed_text( column = c_col-comment text = TEXT-c01 ).

    columns->set_as_color( c_col-color ).
    columns->set_as_exception( column = c_col-exception group = '2' ).
    columns->set_as_cell_style( c_col-cell_style  ).
  ENDMETHOD.

  METHOD on_button_click.
    IF es_row_no-row_id = 0. RETURN. ENDIF.
    RAISE EVENT zif_dtti_change_mapping_event~change_mapping EXPORTING mapping_info = NEW #( source_col = es_col_id-fieldname ).
  ENDMETHOD.

  METHOD prepare_button_row.
    ref_to_tab_fs source_tab_ext <source_ext>.
    INSERT INITIAL LINE INTO <source_ext> INDEX 1 ASSIGNING FIELD-SYMBOL(<row>).
    FIELD-SYMBOLS <cell_type> TYPE ANY TABLE.
    ASSIGN COMPONENT c_col-cell_style OF STRUCTURE <row> TO <cell_type>.

    LOOP AT source_components REFERENCE INTO DATA(comp).
      INSERT VALUE lvc_s_styl( fieldname = comp->name style = cl_gui_alv_grid=>mc_style_button ) INTO TABLE <cell_type>.
    ENDLOOP.
  ENDMETHOD.

  METHOD update_row_info.
    FIELD-SYMBOLS <color> TYPE lvc_t_scol.
    ref_to_tab_fs source_tab_ext <source_ext>.
    LOOP AT mapping_result REFERENCE INTO DATA(result).
      ASSIGN <source_ext>[ sy-tabix ] TO FIELD-SYMBOL(<row>).
      ASSIGN COMPONENT c_col-exception OF STRUCTURE <row> TO FIELD-SYMBOL(<exception>).
      ASSIGN COMPONENT c_col-color OF STRUCTURE <row> TO <color>.
      ASSIGN COMPONENT c_col-comment OF STRUCTURE <row> TO FIELD-SYMBOL(<comment>).

      <exception> = COND #( WHEN strlen( result->comment ) = 0 THEN 3 ELSE 1 ).
      <color> = VALUE #( FOR error_field IN result->error_fields ( fname = error_field color = VALUE #( col = 6 ) ) ).
      APPEND LINES OF VALUE lvc_t_scol( FOR ok_field IN result->ok_fields ( fname = ok_field color = VALUE #( col = 5 ) ) ) TO <color>.
      <comment> = result->comment.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_drag.
    e_dragdropobj->object = NEW zcl_dtti_new_mapping_info( source_col = e_column-fieldname ).
  ENDMETHOD.

  METHOD on_drop.
    IF e_dragdropobj->object IS NOT INSTANCE OF zcl_dtti_new_mapping_info.
      e_dragdropobj->abort( ).
      RETURN.
    ENDIF.

    DATA(mapping_info) = CAST zcl_dtti_new_mapping_info( e_dragdropobj->object ).
    IF mapping_info->target_col IS INITIAL.
      RETURN.
    ENDIF.

    mapping_info->source_col = e_column-fieldname.
    RAISE EVENT zif_dtti_change_mapping_event~change_mapping EXPORTING mapping_info = mapping_info.
  ENDMETHOD.

  METHOD on_added_function.
    CASE e_ucomm.
      WHEN c_functions-change_edit_mode.
        change_edit_mode(  ).

      WHEN c_functions-refresh.
        RAISE EVENT source_data_changed.

      WHEN c_functions-restore_initial.
        DATA(source_tab_ref) = source->get_source_table( ).
        ref_to_tab_fs source_tab_ext <source_ext>.
        ref_to_tab_fs source_tab_ref <source>.
        <source_ext> = CORRESPONDING #( <source> ).
        RAISE EVENT source_data_changed.
    ENDCASE.
    refresh( ).
  ENDMETHOD.

  METHOD change_edit_mode.
    is_in_edit_mode = xsdbool( is_in_edit_mode = abap_false ).
    functions->remove_all_functions( ).

    functions->add_function( VALUE #( function = c_functions-change_edit_mode icon = icon_toggle_display_change text = TEXT-002 ) ).
    IF is_in_edit_mode = abap_true.
      functions->add_function( VALUE #( function = c_functions-refresh icon = icon_refresh text = TEXT-003 ) ).
      functions->add_function( VALUE #( function = c_functions-restore_initial icon = icon_spool_request text = TEXT-004 ) ).
    ENDIF.

    alv_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(fc) ).
    LOOP AT source->source_field_info REFERENCE INTO DATA(source_field_info).
      fc[ fieldname = source_field_info->field ]-edit = is_in_edit_mode.
    ENDLOOP.
    alv_grid->set_frontend_fieldcatalog( fc ).
  ENDMETHOD.

ENDCLASS.
