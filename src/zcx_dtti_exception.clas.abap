
CLASS zcx_dtti_exception  DEFINITION INHERITING FROM cx_static_check PUBLIC .
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING custom_message TYPE string OPTIONAL,
      get_text REDEFINITION.

  PRIVATE SECTION.
    DATA:
          custom_message TYPE string.
ENDCLASS.

CLASS zcx_dtti_exception IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( ).
    me->custom_message = custom_message.
  ENDMETHOD.
  METHOD get_text.
    result = me->custom_message .
  ENDMETHOD.

ENDCLASS.
