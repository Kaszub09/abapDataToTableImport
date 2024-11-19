CLASS zcl_dtti_ask_user_source DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_dtti_screen_handler.
    CONSTANTS:
      BEGIN OF c_source,
        none      TYPE i VALUE 0,
        clipboard TYPE i VALUE 1,
        file      TYPE i VALUE 2,
      END OF c_source.

    METHODS:
      ask_user_about_source RETURNING VALUE(source) TYPE i.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA:
        source TYPE i.
ENDCLASS.

CLASS zcl_dtti_ask_user_source IMPLEMENTATION.
  METHOD ask_user_about_source.
    me->source = c_source-none.

    CALL FUNCTION 'ZDTTI_SET_HANDLER' EXPORTING new_screen_handler = me.
    CALL FUNCTION 'ZDTTI_CALL_SCREEN'  EXPORTING screen_nr = '0002' col1 = 1 col2 = 48 line1 = 1 line2 = 1.
    CALL FUNCTION 'ZDTTI_CLEAR_HANDLER'.

    source = me->source.
  ENDMETHOD.

  METHOD zif_dtti_screen_handler~pai.
    CASE command.
      WHEN 'CLIPBOARD'.
        me->source = c_source-clipboard.
        LEAVE TO SCREEN 0.
      WHEN 'FILE'.
        me->source = c_source-file.
        LEAVE TO SCREEN 0.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
