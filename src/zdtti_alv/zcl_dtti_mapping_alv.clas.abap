CLASS zcl_dtti_mapping_alv DEFINITION PUBLIC INHERITING FROM zcl_ea_alv_table CREATE PRIVATE GLOBAL FRIENDS zcl_data_to_table_import.

  PUBLIC SECTION.
    INTERFACES:
      zif_dtti_change_mapping_event.

    TYPES:
      BEGIN OF t_mapping.
        INCLUDE TYPE zif_dtti_target=>t_target.
      TYPES:
        is_alpha            TYPE abap_bool,
        key_info            TYPE string,
        required_info       TYPE string,
        source_field_button TYPE string,
        color               TYPE lvc_t_scol,
        cell_style          TYPE columns->tt_cell_style_col,
      END OF t_mapping,
      tt_mapping TYPE STANDARD TABLE OF t_mapping WITH EMPTY KEY
      WITH UNIQUE SORTED KEY field COMPONENTS field.

    METHODS:
      constructor IMPORTING mapping TYPE zif_dtti_target=>tt_target container TYPE REF TO cl_gui_container
                            layout_key TYPE salv_s_layout_key OPTIONAL report_id  TYPE sy-repid DEFAULT sy-cprog,
      "! <p class="shorttext synchronized" lang="en">Call after mapping has changed</p>
      refresh_mapping_metainfo IMPORTING source_field_info TYPE zif_dtti_source=>tt_source_field_info.

    DATA:
      "! <p class="shorttext synchronized" lang="en">Current mapping</p>
      mapping_ext TYPE tt_mapping,
      container   TYPE REF TO cl_gui_container.

  PROTECTED SECTION.
    METHODS:
      on_button_click REDEFINITION,
      on_drag REDEFINITION,
      on_drop REDEFINITION.

  PRIVATE SECTION.
    METHODS:
      prepare_columns,
      get_color_tab IMPORTING color TYPE lvc_col RETURNING VALUE(color_tab) TYPE lvc_t_scol.
ENDCLASS.

CLASS zcl_dtti_mapping_alv IMPLEMENTATION.
  METHOD constructor.
    super->constructor( layout_key = layout_key report_id = report_id ).
    me->container = container.
    set_container( container ).

    mapping_ext = CORRESPONDING #( mapping ).

    set_data( REF #( mapping_ext ) ).
    prepare_columns( ).

    "Some mapping info that can be set once
    LOOP AT mapping_ext REFERENCE INTO DATA(map).
      map->key_info = COND #( WHEN map->is_key = abap_true THEN '@3V@' ELSE '' ).
      map->required_info = COND #( WHEN map->is_required = abap_true THEN '@8R@' ELSE '' ).
      map->cell_style = VALUE #( ( fieldname = 'SOURCE_FIELD_BUTTON' style = cl_gui_alv_grid=>mc_style_button ) ).
      map->is_alpha = xsdbool( map->type IS INSTANCE OF cl_abap_elemdescr AND CAST cl_abap_elemdescr( map->type )->edit_mask = '==ALPHA' ).
    ENDLOOP.
    refresh_mapping_metainfo( VALUE #( ) ).

    columns->set_optimize( ).

    me->grid_layout-sel_mode = 'A'.
    me->grid_layout-no_rowmark = abap_true.
    set_header( header = TEXT-001 header_size = 'X' ).
    display_data( ).
    alv_grid->set_filter_criteria( VALUE #( ( fieldname = 'IS_HIDDEN' sign = 'E' option = 'EQ' low = 'X' ) ) ).

  ENDMETHOD.

  METHOD on_button_click.
    IF es_row_no-row_id = 0. RETURN. ENDIF.
    RAISE EVENT zif_dtti_change_mapping_event~change_mapping EXPORTING mapping_info = NEW #( target_col = mapping_ext[ es_row_no-row_id ]-field ).
  ENDMETHOD.

  METHOD refresh_mapping_metainfo.
    LOOP AT mapping_ext REFERENCE INTO DATA(map).
      map->color = COND #( WHEN map->is_required = abap_true AND map->source_field IS INITIAL THEN get_color_tab( 7 )
                           WHEN map->source_field IS NOT INITIAL THEN get_color_tab( 5 )
                           ELSE VALUE #( ) ).
      map->source_field_button = COND #( WHEN map->source_field IS INITIAL THEN '@BZ@'
                                  ELSE VALUE #( source_field_info[ KEY field field = map->source_field ]-description OPTIONAL ) ).
    ENDLOOP.
    columns->set_optimize( ).
  ENDMETHOD.

  METHOD prepare_columns.
    columns->set_as_hidden( 'IS_DDIC' ).
    columns->set_as_hidden( 'IS_KEY' ).
    columns->set_as_hidden( 'IS_REQUIRED' ).
    columns->set_as_hidden( 'IS_HIDDEN' ).
    columns->set_as_hidden( 'IS_ALPHA' ).

    columns->move_column( column_to_move = 'FIELD_DESCRIPTION' before = 'IS_KEY' ).
    columns->move_column( column_to_move = 'SOURCE_FIELD' before = 'SOURCE_FIELD_BUTTON' ).

    columns->set_as_color( 'COLOR' ).
    columns->set_as_cell_style( 'CELL_STYLE' ).

    columns->set_fixed_text( column = 'FIELD' text = TEXT-c01 ).
    columns->set_fixed_text( column = 'FIELD_DESCRIPTION' text = TEXT-c02 ).
    columns->set_fixed_text( column = 'KEY_INFO' text = TEXT-c03 ).
    columns->set_fixed_text( column = 'REQUIRED_INFO' text = TEXT-c04 ).
    columns->set_fixed_text( column = 'SOURCE_FIELD' text = TEXT-c05 ).
    columns->set_fixed_text( column = 'SOURCE_FIELD_BUTTON' text = TEXT-c06 ).
  ENDMETHOD.

  METHOD get_color_tab.
    color_tab = VALUE #( color = VALUE #( col = color ) ( fname  = 'FIELD' ) ( fname  = 'FIELD_DESCRIPTION' )
                ( fname  = 'KEY_INFO' ) ( fname  = 'REQUIRED_INFO' ) ( fname  = 'SOURCE_FIELD' ) ( fname  = 'SOURCE_FIELD_BUTTON' ) ).
  ENDMETHOD.

  METHOD on_drag.
    e_dragdropobj->object = NEW zcl_dtti_new_mapping_info( target_col = mapping_ext[ e_row-index ]-field ).
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

    mapping_info->target_col = mapping_ext[ e_row-index ]-field.
    RAISE EVENT zif_dtti_change_mapping_event~change_mapping EXPORTING mapping_info = mapping_info.
  ENDMETHOD.

ENDCLASS.
