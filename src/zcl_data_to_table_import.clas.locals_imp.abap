*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_new_column DEFINITION INHERITING FROM zcl_ea_salv_table CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING layout_key TYPE salv_s_layout_key OPTIONAL report_id TYPE sy-repid DEFAULT sy-repid,
      get_column IMPORTING data_table TYPE REF TO data old_column TYPE fieldname RETURNING VALUE(new_column) TYPE fieldname.


  PROTECTED SECTION.
    CONSTANTS:
      BEGIN OF c_command,
        confirm TYPE syst_ucomm VALUE 'CONFIRM',
        delete  TYPE syst_ucomm VALUE 'DELETE',
        cancel  TYPE syst_ucomm VALUE 'CANCEL',
      END OF c_command.
    METHODS:
      on_added_function REDEFINITION,
      on_double_click REDEFINITION.
    DATA:
      user_command TYPE syst_ucomm.
ENDCLASS.

CLASS lcl_new_column IMPLEMENTATION.
  METHOD constructor.
    super->constructor( layout_key = layout_key report_id = report_id ).

    functions->add_function( function = c_command-confirm description = VALUE #( icon_id = icon_okay text = TEXT-010 icon_text = TEXT-010 ) ).
    functions->add_function( function = c_command-delete description = VALUE #( icon_id = icon_delete text = TEXT-011 icon_text = TEXT-011 ) ).
    functions->add_function( function = c_command-cancel description = VALUE #( icon_id = icon_cancel text = TEXT-012 icon_text = TEXT-012 ) ).

    alv_table->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>single ).
    set_screen_popup( end_column = 96 end_line = 16 ).
  ENDMETHOD.

  METHOD get_column.
    CLEAR user_command.
    new_column = old_column.

    set_data( data_table ).
    columns->set_optimize( ).
    display_data( ).

    CASE user_command.
        "--------------------------------------------------
      WHEN c_command-confirm.
        DATA(selected_rows) = alv_table->get_selections( )->get_selected_rows( ).
        IF lines( selected_rows ) > 0.
          FIELD-SYMBOLS <table> TYPE table.
          ASSIGN data_table->* TO <table>.
          ASSIGN <table>[ selected_rows[ 1 ] ] TO FIELD-SYMBOL(<row>).
          "Grab value from FIELD column or first field in table
          ASSIGN COMPONENT 'FIELD' OF STRUCTURE <row> TO FIELD-SYMBOL(<field_name>).
          IF sy-subrc = 0.
            new_column = <field_name>.
          ELSE.
            ASSIGN COMPONENT 1 OF STRUCTURE <row> TO FIELD-SYMBOL(<field_1>).
            new_column = <field_1>.
          ENDIF.
        ENDIF.
        "--------------------------------------------------
      WHEN c_command-delete.
        CLEAR new_column.
        "--------------------------------------------------
      WHEN c_command-cancel.
    ENDCASE.
  ENDMETHOD.

  METHOD on_added_function.
    user_command = function.
    alv_table->close_screen( ).
  ENDMETHOD.

  METHOD on_double_click.
    user_command = c_command-confirm.
    alv_table->close_screen( ).
  ENDMETHOD.

ENDCLASS.
