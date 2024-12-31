"! <p class="shorttext synchronized">Import any data to desired structure. Check ZDTTI_EXAMPLES.</p>
"! <br/>TAGS: import;table;mapping
CLASS zcl_data_to_table_import DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_dtti_screen_handler.

    TYPES:
      BEGIN OF t_config,
        "! <p class="shorttext synchronized">Title displayed on titlebar</p>
        title TYPE string,
        "! <p class="shorttext synchronized">Info about documentation created in SE61/SO72. If not supplied, documentation button is not displayed.</p>
        BEGIN OF documentation,
          dokclass TYPE doku_class,
          dokname  TYPE string,
        END OF documentation,
      END OF t_config.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Unless user cancelled, target is updated with result of mapping, and table info is updated with source field names.</p>
      "! @parameter source | <p class="shorttext synchronized" lang="en">Get from ZCL_DTTI_SOURCE_FACTORY</p>
      "! @parameter target | <p class="shorttext synchronized" lang="en">Get from ZCL_DTTI_TARGET_FACTORY. Updated in function unless user cancelled.</p>
      "! @parameter user_confirmed | <p class="shorttext synchronized" lang="en">Returns abap_true if user confirmed, and abap_false if cancelled</p>
      run_mapping IMPORTING source TYPE REF TO zif_dtti_source target TYPE REF TO zif_dtti_target config TYPE t_config OPTIONAL
                  RETURNING VALUE(user_confirmed) TYPE abap_bool.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_select_field,
        field       TYPE fieldname,
        description TYPE zdtti_description,
      END OF t_select_field,
      tt_select_field TYPE STANDARD TABLE OF t_select_field WITH EMPTY KEY
      WITH UNIQUE SORTED KEY field COMPONENTS field.

    CONSTANTS:
        c_fg_name TYPE syrepid VALUE 'SAPLZDTTI'.

    METHODS:
      change_mapping FOR EVENT change_mapping OF zif_dtti_change_mapping_event IMPORTING mapping_info,
      on_source_data_changed FOR EVENT source_data_changed OF zcl_dtti_source_alv,
      refresh_mapping,
      initialize_alvs,
      close_alvs,
      ask_user IMPORTING title TYPE csequence DEFAULT space question TYPE csequence
               RETURNING VALUE(confirmed) TYPE abap_bool,
      command_show_docu,
      command_confirm,
      command_cancel,
      try_to_match_fields.

    DATA:
      BEGIN OF alv,
        mapping TYPE REF TO zcl_dtti_mapping_alv,
        source  TYPE REF TO zcl_dtti_source_alv,
        target  TYPE REF TO zcl_dtti_target_alv,
      END OF alv,
      config TYPE t_config,
      source TYPE REF TO zif_dtti_source,
      target TYPE REF TO zif_dtti_target.

    DATA:
      user_confirmed TYPE abap_bool.
ENDCLASS.

CLASS zcl_data_to_table_import IMPLEMENTATION.
  METHOD ask_user.
    DATA answer TYPE c LENGTH 1.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = title
        text_question         = question
        text_button_1         = TEXT-001
        icon_button_1         = '@0V@'  " Okay icon
        text_button_2         = TEXT-002
        icon_button_2         = '@0W@' " Cancel icon
        default_button        = '1'
        display_cancel_button = ||
        start_column          = 25
        start_row             = 6
        popup_type            = 'ICON_MESSAGE_QUESTION' " check ICON_MESSAGE_ in TYPE_POOL Icon
      IMPORTING
        answer                = answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_dtti_exception EXPORTING custom_message = |POPUP_TO_CONFIRM sy-subrc={ sy-subrc }|.
    ENDIF.
    confirmed = xsdbool( answer = '1' ).
  ENDMETHOD.

  METHOD change_mapping.
    IF mapping_info->target_col IS INITIAL.
      RETURN.
    ENDIF.

    IF mapping_info->source_col IS INITIAL.
      DATA(fields_to_select) = CORRESPONDING tt_select_field( source->source_field_info ).
      DATA(popup) = NEW lcl_new_column( ).
      mapping_info->source_col = popup->get_column( data_table = REF #( fields_to_select ) old_column = alv-mapping->mapping_ext[ KEY field field = mapping_info->target_col ]-source_field ).
    ENDIF.
    alv-mapping->mapping_ext[ field = mapping_info->target_col ]-source_field = mapping_info->source_col.

    alv-mapping->refresh_mapping_metainfo( source->source_field_info ).
    refresh_mapping( ).
  ENDMETHOD.

  METHOD close_alvs.
    alv-mapping->container->free( ).
    alv-source->container->free( ).
    alv-target->container->free( ).
  ENDMETHOD.

  METHOD initialize_alvs.
    alv-mapping = NEW #( layout_key = VALUE #( report = sy-cprog handle = 'SOUR' ) mapping = target->get_target_table_info( )
        container = NEW cl_gui_docking_container( repid = c_fg_name dynnr = '0001' side = cl_gui_docking_container=>dock_at_left ratio = 33 ) ).
    alv-source = NEW #( layout_key = VALUE #( report = sy-cprog handle = 'MAPP' ) source = source
        container = NEW cl_gui_docking_container( repid = c_fg_name dynnr = '0001' side = cl_gui_docking_container=>dock_at_top ratio = 66 ) ).
    alv-target = NEW #( layout_key = VALUE #( report = sy-cprog handle = 'TARG' ) mapping = target->get_target_table_info( ) data_tab = target->get_target_table( )
        container =  NEW cl_gui_docking_container( repid = c_fg_name dynnr = '0001' side = cl_gui_docking_container=>dock_at_top ratio = 33 ) ).

    SET HANDLER change_mapping FOR alv-mapping.
    SET HANDLER change_mapping FOR alv-source.
    SET HANDLER change_mapping FOR alv-target.
    SET HANDLER on_source_data_changed FOR alv-source.

    alv-mapping->drag_drop->add( flavor = 'DTTI' dragsrc = abap_true droptarget = abap_true ).
    alv-source->drag_drop->add( flavor = 'DTTI' dragsrc = abap_true droptarget = abap_true ).
    alv-target->drag_drop->add( flavor = 'DTTI' dragsrc = abap_true droptarget = abap_true ).
  ENDMETHOD.

  METHOD refresh_mapping.
    alv-source->update_row_info( zcl_dtti_mapper=>map( mapping = alv-mapping->mapping_ext
        source_tab = alv-source->source_tab_ext target_tab = target->get_target_table( ) ) ).

    alv-mapping->refresh( ).
    alv-source->refresh( ).
    alv-target->refresh( ).
  ENDMETHOD.

  METHOD run_mapping.
    me->user_confirmed = abap_false.
    me->config = config.
    me->target = target.
    me->source = source.

    initialize_alvs( ).
    try_to_match_fields( ).

    CALL FUNCTION 'ZDTTI_SET_HANDLER' EXPORTING new_screen_handler = me.

    CALL FUNCTION 'ZDTTI_CALL_SCREEN'  EXPORTING  screen_nr = '0001'.

    CALL FUNCTION 'ZDTTI_CLEAR_HANDLER'.
    close_alvs( ).

    user_confirmed = me->user_confirmed.
  ENDMETHOD.

  METHOD command_show_docu.
    IF config-documentation-dokname IS INITIAL.
      RETURN.
    ENDIF.
    DATA links TYPE STANDARD TABLE OF tline WITH EMPTY KEY.
    CALL FUNCTION 'HELP_OBJECT_SHOW'
      EXPORTING
        dokclass         = config-documentation-dokclass                         " Document class
        doklangu         = sy-langu                         " Language, for help -> always Sy-Langu
        dokname          = config-documentation-dokname                          " Document name
      TABLES
        links            = links
      EXCEPTIONS
        object_not_found = 1
        sapscript_error  = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_dtti_exception EXPORTING custom_message = |HELP_OBJECT_SHOW rc={ sy-subrc }|.
    ENDIF.
  ENDMETHOD.

  METHOD zif_dtti_screen_handler~pai.
    CASE command.
      WHEN 'CANCEL'. command_cancel( ).
      WHEN 'CONFIRM'. command_confirm( ).
      WHEN 'DOCU'. command_show_docu( ).
    ENDCASE.
  ENDMETHOD.

  METHOD zif_dtti_screen_handler~pbo.
    IF config-documentation-dokname IS INITIAL.
      SET PF-STATUS 'MAIN' OF PROGRAM c_fg_name EXCLUDING 'DOCU'.
    ELSE.
      SET PF-STATUS 'MAIN' OF PROGRAM c_fg_name.
    ENDIF.
    SET TITLEBAR 'MAIN' OF PROGRAM c_fg_name WITH config-title.
  ENDMETHOD.

  METHOD on_source_data_changed.
    refresh_mapping( ).
  ENDMETHOD.

  METHOD command_cancel.
    IF NOT ask_user( TEXT-003 ).
      RETURN.
    ENDIF.
    DATA(target_tab) = target->get_target_table( ).
    ref_to_tab_fs target_tab <target_table>.
    CLEAR <target_table>.
    LEAVE TO SCREEN 0.
  ENDMETHOD.

  METHOD command_confirm.
    " TODO: variable is assigned but never used (ABAP cleaner)
    LOOP AT alv-mapping->mapping_ext REFERENCE INTO DATA(map) WHERE is_required = abap_true AND source_field IS INITIAL.
      MESSAGE TEXT-009 TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDLOOP.
    IF NOT ask_user( TEXT-004 ).
      RETURN.
    ENDIF.
    refresh_mapping( ).
    target->set_target_table_info( CORRESPONDING #( alv-mapping->mapping_ext ) ).
    user_confirmed = abap_true.
    LEAVE TO SCREEN 0.
  ENDMETHOD.

  METHOD try_to_match_fields.
    LOOP AT source->source_field_info REFERENCE INTO DATA(source_field).
      DATA(source_name) = to_upper( condense( source_field->field ) ).
      DATA(source_description) = to_upper( condense( source_field->description ) ).
      LOOP AT alv-mapping->mapping_ext REFERENCE INTO DATA(mapping_field).
        IF source_name = to_upper( mapping_field->field ) OR ( strlen( source_description ) > 0
            AND ( to_upper( mapping_field->field ) = source_description
                  OR to_upper( mapping_field->field_description ) = source_description ) ).
          mapping_field->source_field = source_field->field.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    alv-mapping->refresh_mapping_metainfo( source->source_field_info ).
    refresh_mapping( ).
  ENDMETHOD.
ENDCLASS.
