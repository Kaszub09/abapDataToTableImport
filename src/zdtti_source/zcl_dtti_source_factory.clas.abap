CLASS zcl_dtti_source_factory DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS:
      create_from_ref IMPORTING source_table TYPE REF TO data RETURNING VALUE(source) TYPE REF TO zif_dtti_source,
      "! <p class="shorttext synchronized" lang="en">Source is not bound if user cancelled</p>
      create_from_user_input RETURNING VALUE(source) TYPE REF TO zif_dtti_source,
      "! <p class="shorttext synchronized" lang="en">Source is not bound if user cancelled</p>
      create_from_clipboard RETURNING VALUE(source) TYPE REF TO zif_dtti_source,
      "! <p class="shorttext synchronized" lang="en">Source is not bound if user cancelled.
      "! <br/>In case of text files expects simple format without escape characters inside quotes</p>
      create_from_file RETURNING VALUE(source) TYPE REF TO zif_dtti_source.

  PRIVATE SECTION.
    TYPES:
        tt_string TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    CLASS-METHODS:
      get_file_from_user RETURNING VALUE(file_path) TYPE string,
      get_data_table_from_excel IMPORTING file_path TYPE string RETURNING VALUE(data_table) TYPE REF TO data,
      get_data_table_from_txt IMPORTING file_path TYPE string RETURNING VALUE(data_table) TYPE REF TO data,
      get_data_from_text IMPORTING lines TYPE tt_string RETURNING VALUE(data_table) TYPE REF TO data,
      determine_structure IMPORTING line TYPE string EXPORTING separator TYPE string struct TYPE REF TO cl_abap_structdescr,
      get_wks_name IMPORTING excel TYPE REF TO cl_fdt_xl_spreadsheet  RETURNING VALUE(wks) TYPE string,
      get_col_letters_from_int IMPORTING index TYPE i RETURNING VALUE(letters) TYPE string,
      ask_user IMPORTING title TYPE csequence DEFAULT space question TYPE csequence
               RETURNING VALUE(confirmed) TYPE abap_bool.
ENDCLASS.

CLASS zcl_dtti_source_factory IMPLEMENTATION.
  METHOD create_from_ref.
    source = NEW zcl_dtti_source_table( source_table ).
  ENDMETHOD.

  METHOD create_from_user_input.
    DATA(ask_user_source) = NEW zcl_dtti_ask_user_source( ).
    CASE ask_user_source->ask_user_about_source( ).
      WHEN ask_user_source->c_source-clipboard.
        source = create_from_clipboard( ).
      WHEN ask_user_source->c_source-file.
        source = create_from_file( ).
    ENDCASE.
  ENDMETHOD.

  METHOD create_from_clipboard.
    TYPES: t_char_4096 TYPE c LENGTH 4096.
    DATA: lines TYPE STANDARD TABLE OF t_char_4096 WITH EMPTY KEY. "Can't be string - it makes import stuck in endless loop

    cl_gui_frontend_services=>clipboard_import( IMPORTING data = lines ).
    DATA(data_tab) = get_data_from_text( CONV #( lines ) ).

    IF data_tab IS NOT BOUND.
      RETURN.
    ENDIF.

    source = NEW zcl_dtti_source_table( source_table = data_tab is_first_row_headers = ask_user( question = CONV #( TEXT-002 ) ) ).
  ENDMETHOD.

  METHOD create_from_file.
    DATA(file_path) = get_file_from_user( ).
    IF strlen( file_path ) = 0.
      RETURN.
    ENDIF.

    SPLIT file_path AT '.' INTO TABLE DATA(splitted_path).
    DATA data_tab TYPE REF TO data.
    IF to_upper( splitted_path[ lines( splitted_path ) ] ) = 'XLSX'.
      data_tab = get_data_table_from_excel( file_path ).
    ELSEIF to_upper( splitted_path[ lines( splitted_path ) ] ) = 'TXT'.
      data_tab = get_data_table_from_txt( file_path ).
    ENDIF.

    IF data_tab IS NOT BOUND.
      RETURN.
    ENDIF.

    source = NEW zcl_dtti_source_table( source_table = data_tab is_first_row_headers = ask_user( question = CONV #( TEXT-002 ) ) ).
  ENDMETHOD.

  METHOD get_file_from_user.
    DATA:
      files       TYPE filetable,
      files_count TYPE i,
      user_action TYPE i.

    DATA desktop_folder TYPE string.

    cl_gui_frontend_services=>get_desktop_directory( CHANGING desktop_directory = desktop_folder ).
    cl_gui_cfw=>update_view( ). "Magic line to actually fill desktop_folder value

    cl_gui_frontend_services=>file_open_dialog( EXPORTING file_filter = CONV #( TEXT-001 ) initial_directory = desktop_folder
                                                CHANGING file_table = files rc = files_count user_action = user_action ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_dtti_exception EXPORTING custom_message = |FILE_OPEN_DIALOG sy-subrc={ sy-subrc }|.
    ENDIF.

    IF user_action = cl_gui_frontend_services=>action_ok AND files_count = 1 AND cl_gui_frontend_services=>file_exist( CONV #( files[ 1 ]-filename ) ).
      file_path = files[ 1 ]-filename.
    ENDIF.
  ENDMETHOD.

  METHOD get_data_table_from_excel.
    "Upload and parse excel file
    DATA it_bin_data TYPE w3mimetabtype.

    " TODO: variable is assigned but never used (ABAP cleaner)
    cl_gui_frontend_services=>gui_upload( EXPORTING filename = file_path filetype = 'BIN' IMPORTING filelength =  DATA(filelength) CHANGING data_tab = it_bin_data ).
    DATA(file_as_xstring) = cl_bcs_convert=>solix_to_xstring( it_bin_data ).
    DATA(excel) = NEW cl_fdt_xl_spreadsheet( document_name = file_path xdocument = file_as_xstring ).

    DATA(wks) = get_wks_name( excel ).
    IF strlen( wks ) = 0.
      RETURN.
    ENDIF.

    data_table = excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( wks ).
  ENDMETHOD.

  METHOD get_data_table_from_txt.
    DATA lines TYPE tt_string.

    cl_gui_frontend_services=>gui_upload( EXPORTING filename = file_path CHANGING data_tab = lines ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_dtti_exception EXPORTING custom_message = |GUI_UPLOAD sy-subrc={ sy-subrc }|.
    ENDIF.

    data_table = get_data_from_text( lines ).
  ENDMETHOD.

  METHOD get_data_from_text.
    IF lines( lines ) = 0.
      RETURN.
    ENDIF.

    determine_structure( EXPORTING line = lines[ 1 ] IMPORTING separator = DATA(separator) struct = DATA(struct) ).
    DATA(table_descr) = cl_abap_tabledescr=>get( p_line_type = struct p_key_kind = cl_abap_tabledescr=>keydefkind_empty ).
    CREATE DATA data_table TYPE HANDLE table_descr.

    FIELD-SYMBOLS <table> TYPE table.
    ASSIGN data_table->* TO <table>.

    LOOP AT lines REFERENCE INTO DATA(line).
      APPEND INITIAL LINE TO <table> ASSIGNING FIELD-SYMBOL(<row>).
      SPLIT line->* AT separator INTO TABLE DATA(splitted).
      LOOP AT splitted REFERENCE INTO DATA(field).
        ASSIGN COMPONENT sy-tabix OF STRUCTURE <row> TO FIELD-SYMBOL(<field>).
        <field> = field->*.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD determine_structure.
    separator = ','.
    SPLIT line AT ',' INTO TABLE DATA(splitted).
    DATA(split_count) = lines( splitted ).
    "--------------------------------------------------
    SPLIT line AT ';' INTO TABLE splitted.
    IF lines( splitted ) > split_count.
      separator = ';'.
      split_count = lines( splitted ).
    ENDIF.
    "--------------------------------------------------
    SPLIT line AT cl_abap_char_utilities=>horizontal_tab INTO TABLE splitted.
    IF lines( splitted ) > split_count.
      separator = cl_abap_char_utilities=>horizontal_tab.
      split_count = lines( splitted ).
    ENDIF.
    "--------------------------------------------------
    DATA(components) = VALUE cl_abap_structdescr=>component_table( ).
    DATA(index) = 1.
    WHILE index <= split_count + 1.
      APPEND VALUE #( name  = get_col_letters_from_int( index ) type = CAST #( cl_abap_typedescr=>describe_by_name( 'STRING' ) ) ) TO components.
      index = index + 1.
    ENDWHILE.

    struct = cl_abap_structdescr=>get( components ).
  ENDMETHOD.

  METHOD get_wks_name.
    excel->if_fdt_doc_spreadsheet~get_worksheet_names( IMPORTING worksheet_names = DATA(worksheets_names) ).
    TYPES:
      BEGIN OF t_worksheet,
        name TYPE string,
      END OF t_worksheet.
    DATA: worksheets TYPE STANDARD TABLE OF t_worksheet WITH EMPTY KEY.
    worksheets = VALUE #( FOR name IN worksheets_names ( name = name ) ). "SALV needs table with structure

    IF lines( worksheets ) > 1.
      DATA(popup) = NEW zcl_ea_salv_table_selectable( ).
      popup->set_data( REF #( worksheets ) ).
      popup->set_screen_popup( end_column = 48 end_line = 24 ).
      popup->set_header( TEXT-008 ).
      popup->display_selectable( IMPORTING selected_rows = DATA(selected_rows) user_confirmed = DATA(user_confirmed) ).
      IF user_confirmed = abap_false OR lines( selected_rows ) = 0.
        RETURN.
      ENDIF.
      wks = worksheets[ selected_rows[ 1 ] ]-name.

    ELSE.
      wks = worksheets[ 1 ]-name.
    ENDIF.
  ENDMETHOD.

  METHOD get_col_letters_from_int.
    IF index < 1.
      RETURN.
    ENDIF.
    DATA(number) = index.
    DO.
      DATA(mod) = ( number - 1 ) MOD 26.
      letters = |{ sy-abcde+mod(1) }{ letters }|.
      number = ( number - mod ) / 26.
      IF number <= 0.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD ask_user.
    DATA answer TYPE c LENGTH 1.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = title
        text_question         = question
        text_button_1         = TEXT-004
        icon_button_1         = '@0V@'  " Okay icon
        text_button_2         = TEXT-005
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
ENDCLASS.
