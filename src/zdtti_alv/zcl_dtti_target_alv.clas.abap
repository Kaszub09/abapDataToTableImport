CLASS zcl_dtti_target_alv DEFINITION PUBLIC INHERITING FROM zcl_ea_alv_table FINAL CREATE PRIVATE
  GLOBAL FRIENDS zcl_data_to_table_import.

  PUBLIC SECTION.
    INTERFACES:
      zif_dtti_change_mapping_event.

    METHODS:
      constructor IMPORTING mapping TYPE zif_dtti_target=>tt_target data_tab TYPE REF TO data container TYPE REF TO cl_gui_container
                            layout_key TYPE salv_s_layout_key OPTIONAL report_id  TYPE sy-repid DEFAULT sy-cprog.
    DATA:
          container   TYPE REF TO cl_gui_container.
  PROTECTED SECTION.
    METHODS:
      on_drag REDEFINITION,
      on_drop REDEFINITION.
  PRIVATE SECTION.

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
ENDCLASS.
