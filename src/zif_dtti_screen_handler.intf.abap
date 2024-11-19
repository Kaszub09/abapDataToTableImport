INTERFACE zif_dtti_screen_handler
  PUBLIC .
  METHODS:
    pbo DEFAULT IGNORE,
    pai DEFAULT IGNORE IMPORTING VALUE(command) TYPE sy-ucomm.
ENDINTERFACE.
