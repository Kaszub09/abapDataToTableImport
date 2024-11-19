INTERFACE zif_dtti_change_mapping_event PUBLIC.
  EVENTS:
    change_mapping EXPORTING VALUE(mapping_info) TYPE REF TO zcl_dtti_new_mapping_info.
ENDINTERFACE.
