CLASS zcl_dtti_target_alv DEFINITION PUBLIC INHERITING FROM zcl_ea_alv_table FINAL CREATE PRIVATE
  GLOBAL FRIENDS zcl_data_to_table_import.

  PUBLIC SECTION.
    INTERFACES:
      zif_dtti_change_mapping_event.

    METHODS:
      constructor IMPORTING mapping TYPE zif_dtti_target=>tt_target data_tab TYPE REF TO data container TYPE REF TO cl_gui_container
                            layout_key TYPE salv_s_layout_key OPTIONAL report_id  TYPE sy-repid DEFAULT sy-cprog,
      update_col_visibility IMPORTING mapping TYPE zcl_dtti_mapping_alv=>tt_mapping.
    DATA:
      container             TYPE REF TO cl_gui_container,
      unmapped_cols_visible TYPE abap_bool VALUE abap_true.
    EVENTS:
       unmapped_cols_vis_changed.
  PROTECTED SECTION.
    METHODS:
      on_drag REDEFINITION,
      on_drop REDEFINITION,
      on_added_function REDEFINITION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_functions,
        change_unmapped_visibility TYPE syst_ucomm VALUE 'CHANGE_UNMAPPED_VISIBILITY',
      END OF c_functions.
ENDCLASS.

CLASS zcl_dtti_target_alv IMPLEMENTATION.
  METHOD constructor.
    super->constructor( layout_key = layout_key report_id = report_id ).
    me->container = container.
    set_container( container ).

    set_data( data_tab ).

    LOOP AT mapping REFERENCE INTO DATA(map).
      columns->set_fixed_text( column = map->field text = map->field_description ).
      IF map->is_hidden = abap_true.
        columns->set_as_hidden( map->field ).
      ENDIF.
    ENDLOOP.
    me->grid_layout-sel_mode = 'A'.
    me->grid_layout-no_rowmark = abap_true.

    set_header( header = TEXT-001 header_size = 'X' ).
    functions->remove_all_functions( ).

    functions->add_function( VALUE #( function = c_functions-change_unmapped_visibility icon = '@D1@' text = TEXT-002 ) ).

    display_data( ).
  ENDMETHOD.

  METHOD on_drag.
    e_dragdropobj->object = NEW zcl_dtti_new_mapping_info( target_col = e_column-fieldname ).
  ENDMETHOD.

  METHOD on_drop.
    IF e_dragdropobj->object IS NOT INSTANCE OF zcl_dtti_new_mapping_info.
      e_dragdropobj->abort( ).
      RETURN.
    ENDIF.

    DATA(mapping_info) = CAST zcl_dtti_new_mapping_info( e_dragdropobj->object ).
    IF mapping_info->source_col IS INITIAL.
      RETURN.
    ENDIF.

    mapping_info->target_col = e_column-fieldname.
    RAISE EVENT zif_dtti_change_mapping_event~change_mapping EXPORTING mapping_info = mapping_info.
  ENDMETHOD.

  METHOD on_added_function.
    CASE e_ucomm.
      WHEN c_functions-change_unmapped_visibility.
        unmapped_cols_visible = xsdbool( unmapped_cols_visible = abap_false ).
        RAISE EVENT unmapped_cols_vis_changed.
    ENDCASE.
  ENDMETHOD.

  METHOD update_col_visibility.
    me->alv_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(fc) ).
    LOOP AT fc REFERENCE INTO DATA(field).
      LOOP AT mapping REFERENCE INTO DATA(map) USING KEY field WHERE field = field->fieldname AND is_hidden = abap_false.
        field->no_out = COND #( WHEN unmapped_cols_visible = abap_false AND map->source_field IS INITIAL THEN abap_true ELSE abap_false ).
      ENDLOOP.
    ENDLOOP.
    me->alv_grid->set_frontend_fieldcatalog( fc ).
  ENDMETHOD.

ENDCLASS.
