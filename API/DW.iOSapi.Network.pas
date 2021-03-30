unit DW.iOSapi.Network;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // Posix
  Posix.SysSocket,
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.Dispatch,
  // iOS
  iOSapi.CocoaTypes;

const
  NW_NOT_i386_MAC = 1;
  UINT32_MAX = 4294967295;
  NW_LISTENER_INFINITE_CONNECTION_LIMIT = UINT32_MAX;
  NW_FRAMER_CREATE_FLAGS_DEFAULT = $00;
  NW_FRAMER_WAKEUP_TIME_FOREVER = not 0;
  nw_interface_type_other = 0;
  nw_interface_type_wifi = 1;
  nw_interface_type_cellular = 2;
  nw_interface_type_wired = 3;
  nw_interface_type_loopback = 4;
  nw_txt_record_find_key_invalid = 0;
  nw_txt_record_find_key_not_present = 1;
  nw_txt_record_find_key_no_value = 2;
  nw_txt_record_find_key_empty_value = 3;
  nw_txt_record_find_key_non_empty_value = 4;
  nw_endpoint_type_invalid = 0;
  nw_endpoint_type_address = 1;
  nw_endpoint_type_host = 2;
  nw_endpoint_type_bonjour_service = 3;
  nw_endpoint_type_url = 4;
  nw_service_class_best_effort = 0;
  nw_service_class_background = 1;
  nw_service_class_interactive_video = 2;
  nw_service_class_interactive_voice = 3;
  nw_service_class_responsive_data = 4;
  nw_service_class_signaling = 5;
  nw_multipath_service_disabled = 0;
  nw_multipath_service_handover = 1;
  nw_multipath_service_interactive = 2;
  nw_multipath_service_aggregate = 3;
  nw_parameters_expired_dns_behavior_default = 0;
  nw_parameters_expired_dns_behavior_allow = 1;
  nw_parameters_expired_dns_behavior_prohibit = 2;
  nw_path_status_invalid = 0;
  nw_path_status_satisfied = 1;
  nw_path_status_unsatisfied = 2;
  nw_path_status_satisfiable = 3;
  nw_path_unsatisfied_reason_not_available = 0;
  nw_path_unsatisfied_reason_cellular_denied = 1;
  nw_path_unsatisfied_reason_wifi_denied = 2;
  nw_path_unsatisfied_reason_local_network_denied = 3;
  nw_error_domain_invalid = 0;
  nw_error_domain_posix = 1;
  nw_error_domain_dns = 2;
  nw_error_domain_tls = 3;
  nw_connection_state_invalid = 0;
  nw_connection_state_waiting = 1;
  nw_connection_state_preparing = 2;
  nw_connection_state_ready = 3;
  nw_connection_state_failed = 4;
  nw_connection_state_cancelled = 5;
  nw_listener_state_invalid = 0;
  nw_listener_state_waiting = 1;
  nw_listener_state_ready = 2;
  nw_listener_state_failed = 3;
  nw_listener_state_cancelled = 4;
  nw_ws_opcode_invalid = -1;
  nw_ws_opcode_cont = 0;
  nw_ws_opcode_text = 1;
  nw_ws_opcode_binary = 2;
  nw_ws_opcode_close = 8;
  nw_ws_opcode_ping = 9;
  nw_ws_opcode_pong = 10;
  nw_ws_close_code_normal_closure = 1000;
  nw_ws_close_code_going_away = 1001;
  nw_ws_close_code_protocol_error = 1002;
  nw_ws_close_code_unsupported_data = 1003;
  nw_ws_close_code_no_status_received = 1005;
  nw_ws_close_code_abnormal_closure = 1006;
  nw_ws_close_code_invalid_frame_payload_data = 1007;
  nw_ws_close_code_policy_violation = 1008;
  nw_ws_close_code_message_too_big = 1009;
  nw_ws_close_code_mandatory_extension = 1010;
  nw_ws_close_code_internal_server_error = 1011;
  nw_ws_close_code_tls_handshake = 1015;
  nw_ws_version_invalid = 0;
  nw_ws_version_13 = 1;
  nw_ws_response_status_invalid = 0;
  nw_ws_response_status_accept = 1;
  nw_ws_response_status_reject = 2;
  nw_browse_result_change_invalid = 0;
  nw_browse_result_change_identical = 1;
  nw_browse_result_change_result_added = 2;
  nw_browse_result_change_result_removed = 4;
  nw_browse_result_change_interface_added = 8;
  nw_browse_result_change_interface_removed = 16;
  nw_browse_result_change_txt_record_changed = 32;
  nw_browser_state_invalid = 0;
  nw_browser_state_ready = 1;
  nw_browser_state_failed = 2;
  nw_browser_state_cancelled = 3;
  nw_browser_state_waiting = 4;
  nw_ip_version_any = 0;
  nw_ip_version_4 = 4;
  nw_ip_version_6 = 6;
  nw_ip_local_address_preference_default = 0;
  nw_ip_local_address_preference_temporary = 1;
  nw_ip_local_address_preference_stable = 2;
  nw_ip_ecn_flag_non_ect = 0;
  nw_ip_ecn_flag_ect_0 = 2;
  nw_ip_ecn_flag_ect_1 = 1;
  nw_ip_ecn_flag_ce = 3;
  nw_connection_group_state_invalid = 0;
  nw_connection_group_state_waiting = 1;
  nw_connection_group_state_ready = 2;
  nw_connection_group_state_failed = 3;
  nw_connection_group_state_cancelled = 4;
  nw_report_resolution_source_query = 1;
  nw_report_resolution_source_cache = 2;
  nw_report_resolution_source_expired_cache = 3;
  nw_report_resolution_protocol_unknown = 0;
  nw_report_resolution_protocol_udp = 1;
  nw_report_resolution_protocol_tcp = 2;
  nw_report_resolution_protocol_tls = 3;
  nw_report_resolution_protocol_https = 4;
  nw_data_transfer_report_state_collecting = 1;
  nw_data_transfer_report_state_collected = 2;
  nw_ethernet_channel_state_invalid = 0;
  nw_ethernet_channel_state_waiting = 1;
  nw_ethernet_channel_state_preparing = 2;
  nw_ethernet_channel_state_ready = 3;
  nw_ethernet_channel_state_failed = 4;
  nw_ethernet_channel_state_cancelled = 5;
  nw_framer_start_result_ready = 1;
  nw_framer_start_result_will_mark_ready = 2;

type
  PUInt8 = ^UInt8;
  Pnw_object = Pointer;
  PPnw_object = ^Pnw_object;
  Pnw_interface = Pointer;
  PPnw_interface = ^Pnw_interface;
  Pnw_txt_record = Pointer;
  PPnw_txt_record = ^Pnw_txt_record;
  Pnw_endpoint = Pointer;
  PPnw_endpoint = ^Pnw_endpoint;
  Pnw_protocol_definition = Pointer;
  PPnw_protocol_definition = ^Pnw_protocol_definition;
  Pnw_protocol_options = Pointer;
  PPnw_protocol_options = ^Pnw_protocol_options;
  Pnw_protocol_metadata = Pointer;
  PPnw_protocol_metadata = ^Pnw_protocol_metadata;
  Pnw_resolver_config = Pointer;
  PPnw_resolver_config = ^Pnw_resolver_config;
  Pnw_privacy_context = Pointer;
  PPnw_privacy_context = ^Pnw_privacy_context;
  Pnw_parameters = Pointer;
  PPnw_parameters = ^Pnw_parameters;
  Pnw_protocol_stack = Pointer;
  PPnw_protocol_stack = ^Pnw_protocol_stack;
  Pnw_path = Pointer;
  PPnw_path = ^Pnw_path;
  Pnw_content_context = Pointer;
  PPnw_content_context = ^Pnw_content_context;
  Pnw_error = Pointer;
  PPnw_error = ^Pnw_error;
  Pnw_connection = Pointer;
  PPnw_connection = ^Pnw_connection;
  Pnw_advertise_descriptor = Pointer;
  PPnw_advertise_descriptor = ^Pnw_advertise_descriptor;
  Pnw_listener = Pointer;
  PPnw_listener = ^Pnw_listener;
  Pnw_browse_descriptor = Pointer;
  PPnw_browse_descriptor = ^Pnw_browse_descriptor;
  Pnw_ws_request = Pointer;
  PPnw_ws_request = ^Pnw_ws_request;
  Pnw_ws_response = Pointer;
  PPnw_ws_response = ^Pnw_ws_response;
  Pnw_browse_result = Pointer;
  PPnw_browse_result = ^Pnw_browse_result;
  Pnw_browser = Pointer;
  PPnw_browser = ^Pnw_browser;
  Pnw_path_monitor = Pointer;
  PPnw_path_monitor = ^Pnw_path_monitor;
  Pnw_group_descriptor = Pointer;
  PPnw_group_descriptor = ^Pnw_group_descriptor;
  Pnw_connection_group = Pointer;
  PPnw_connection_group = ^Pnw_connection_group;
  Pnw_establishment_report = Pointer;
  PPnw_establishment_report = ^Pnw_establishment_report;
  Pnw_resolution_report = Pointer;
  PPnw_resolution_report = ^Pnw_resolution_report;
  Pnw_data_transfer_report = Pointer;
  PPnw_data_transfer_report = ^Pnw_data_transfer_report;
  Pnw_ethernet_channel = Pointer;
  PPnw_ethernet_channel = ^Pnw_ethernet_channel;
  Pnw_framer = Pointer;
  PPnw_framer = ^Pnw_framer;

  nw_object_t = Pointer;
  Pnw_object_t = ^nw_object_t;
  nw_interface_t = Pointer;
  Pnw_interface_t = ^nw_interface_t;
  nw_txt_record_t = Pointer;
  Pnw_txt_record_t = ^nw_txt_record_t;

  nw_txt_record_find_key_t = UInt32;
  nw_interface_type_t = UInt32;
  nw_connection_state_t = UInt32;
  nw_listener_state_t = UInt32;
  nw_browser_state_t = UInt32;
  nw_connection_group_state_t = UInt32;
  nw_framer_start_result_t = UInt32;
  nw_report_resolution_source_t = UInt32;
  nw_report_resolution_protocol_t = UInt32;
  nw_ethernet_channel_state_t = UInt32;
  nw_endpoint_type_t = UInt32;
  nw_service_class_t = UInt32;
  nw_multipath_service_t = UInt32;
  nw_parameters_expired_dns_behavior_t = UInt32;
  nw_path_status_t = UInt32;
  nw_path_unsatisfied_reason_t = UInt32;
  nw_error_domain_t = UInt32;
  nw_ws_version_t = UInt32;
  nw_ws_opcode_t = UInt32;
  nw_ws_close_code_t = UInt32;
  nw_ws_response_status_t = UInt32;
  nw_ip_version_t = UInt32;
  nw_ip_local_address_preference_t = UInt32;
  nw_ip_ecn_flag_t = UInt32;
  nw_data_transfer_report_state_t = UInt32;
  sec_protocol_options_t = UInt32;
  sec_protocol_metadata_t = UInt32;

  dispatch_data_t = Pointer;

  nw_txt_record_access_key_t = function(key: PAnsiChar; found: nw_txt_record_find_key_t; value: PUInt8; value_len: NativeUInt): Boolean of object;

  nw_txt_record_access_bytes_t = function(raw_txt_record: PUInt8; len: NativeUInt): Boolean of object;

  nw_txt_record_applier_t = function(key: PAnsiChar; found: nw_txt_record_find_key_t; value: PUInt8; value_len: NativeUInt): Boolean of object;
  nw_endpoint_t = Pointer;
  Pnw_endpoint_t = ^nw_endpoint_t;
  nw_protocol_definition_t = Pointer;
  Pnw_protocol_definition_t = ^nw_protocol_definition_t;
  nw_protocol_options_t = Pointer;
  Pnw_protocol_options_t = ^nw_protocol_options_t;
  nw_protocol_metadata_t = Pointer;
  Pnw_protocol_metadata_t = ^nw_protocol_metadata_t;
  nw_resolver_config_t = Pointer;
  Pnw_resolver_config_t = ^nw_resolver_config_t;
  nw_privacy_context_t = Pointer;
  Pnw_privacy_context_t = ^nw_privacy_context_t;
  nw_parameters_t = Pointer;
  Pnw_parameters_t = ^nw_parameters_t;
  nw_protocol_stack_t = Pointer;
  Pnw_protocol_stack_t = ^nw_protocol_stack_t;

  nw_parameters_configure_protocol_block_t = procedure(options: nw_protocol_options_t) of object;

  nw_parameters_iterate_interfaces_block_t = function(&interface: nw_interface_t): Boolean of object;

  nw_parameters_iterate_interface_types_block_t = function(interface_type: nw_interface_type_t): Boolean of object;

  nw_protocol_stack_iterate_protocols_block_t = procedure(protocol: nw_protocol_options_t) of object;
  nw_path_t = Pointer;
  Pnw_path_t = ^nw_path_t;

  nw_path_enumerate_interfaces_block_t = function(&interface: nw_interface_t): Boolean of object;

  nw_path_enumerate_gateways_block_t = function(gateway: nw_endpoint_t): Boolean of object;
  nw_content_context_t = Pointer;
  Pnw_content_context_t = ^nw_content_context_t;
  nw_error_t = Pointer;
  Pnw_error_t = ^nw_error_t;
  nw_connection_t = Pointer;
  Pnw_connection_t = ^nw_connection_t;

  nw_connection_state_changed_handler_t = procedure(state: nw_connection_state_t; error: nw_error_t) of object;

  nw_connection_boolean_event_handler_t = procedure(value: Boolean) of object;

  nw_connection_path_event_handler_t = procedure(path: nw_path_t) of object;

  nw_connection_receive_completion_t = procedure(content: dispatch_data_t; context: nw_content_context_t; is_complete: Boolean;
    error: nw_error_t) of object;

  nw_connection_send_completion_t = procedure(error: nw_error_t) of object;
  nw_advertise_descriptor_t = Pointer;
  Pnw_advertise_descriptor_t = ^nw_advertise_descriptor_t;
  nw_listener_t = Pointer;
  Pnw_listener_t = ^nw_listener_t;

  nw_listener_state_changed_handler_t = procedure(state: nw_listener_state_t; error: nw_error_t) of object;

  nw_listener_new_connection_handler_t = procedure(connection: nw_connection_t) of object;

  nw_listener_advertised_endpoint_changed_handler_t = procedure(advertised_endpoint: nw_endpoint_t; added: Boolean) of object;
  nw_browse_descriptor_t = Pointer;
  Pnw_browse_descriptor_t = ^nw_browse_descriptor_t;

  nw_ws_pong_handler_t = procedure(error: nw_error_t) of object;
  nw_ws_request_t = Pointer;
  Pnw_ws_request_t = ^nw_ws_request_t;

  nw_ws_subprotocol_enumerator_t = function(subprotocol: PAnsiChar): Boolean of object;

  nw_ws_additional_header_enumerator_t = function(name: PAnsiChar; value: PAnsiChar): Boolean of object;
  nw_ws_response_t = Pointer;
  Pnw_ws_response_t = ^nw_ws_response_t;

  nw_ws_client_request_handler_t = function(request: nw_ws_request_t): nw_ws_response_t of object;
  nw_browse_result_t = Pointer;
  Pnw_browse_result_t = ^nw_browse_result_t;
  nw_browse_result_change_t = UInt64;

  nw_browse_result_enumerate_interface_t = function(&interface: nw_interface_t): Boolean of object;
  nw_browser_t = Pointer;
  Pnw_browser_t = ^nw_browser_t;

  nw_browser_browse_results_changed_handler_t = procedure(old_result: nw_browse_result_t; new_result: nw_browse_result_t;
    batch_complete: Boolean) of object;

  nw_browser_state_changed_handler_t = procedure(state: nw_browser_state_t; error: nw_error_t) of object;
  nw_path_monitor_t = Pointer;
  Pnw_path_monitor_t = ^nw_path_monitor_t;

  nw_path_monitor_cancel_handler_t = procedure of object;

  nw_path_monitor_update_handler_t = procedure(path: nw_path_t) of object;
  nw_group_descriptor_t = Pointer;
  Pnw_group_descriptor_t = ^nw_group_descriptor_t;

  nw_group_descriptor_enumerate_endpoints_block_t = function(endpoint: nw_endpoint_t): Boolean of object;
  nw_connection_group_t = Pointer;
  Pnw_connection_group_t = ^nw_connection_group_t;

  nw_connection_group_state_changed_handler_t = procedure(state: nw_connection_group_state_t; error: nw_error_t) of object;

  nw_connection_group_receive_handler_t = procedure(content: dispatch_data_t; context: nw_content_context_t; is_complete: Boolean) of object;

  nw_connection_group_send_completion_t = procedure(error: nw_error_t) of object;
  nw_establishment_report_t = Pointer;
  Pnw_establishment_report_t = ^nw_establishment_report_t;

  nw_establishment_report_access_block_t = procedure(report: nw_establishment_report_t) of object;
  nw_resolution_report_t = Pointer;
  Pnw_resolution_report_t = ^nw_resolution_report_t;

  nw_report_resolution_enumerator_t = function(source: nw_report_resolution_source_t; milliseconds: UInt64; endpoint_count: UInt32;
    successful_endpoint: nw_endpoint_t; preferred_endpoint: nw_endpoint_t): Boolean of object;

  nw_report_resolution_report_enumerator_t = function(resolution_report: nw_resolution_report_t): Boolean of object;

  nw_report_protocol_enumerator_t = function(protocol: nw_protocol_definition_t; handshake_milliseconds: UInt64;
    handshake_rtt_milliseconds: UInt64): Boolean of object;
  nw_data_transfer_report_t = Pointer;
  Pnw_data_transfer_report_t = ^nw_data_transfer_report_t;

  nw_data_transfer_report_collect_block_t = procedure(report: nw_data_transfer_report_t) of object;
  nw_ethernet_channel_t = Pointer;
  Pnw_ethernet_channel_t = ^nw_ethernet_channel_t;

  nw_ethernet_channel_state_changed_handler_t = procedure(state: nw_ethernet_channel_state_t; error: nw_error_t) of object;
  nw_ethernet_address_t = array [0..5] of Byte;

  nw_ethernet_channel_receive_handler_t = procedure(content: dispatch_data_t; vlan_tag: UInt16; local_address: PByte;
    remote_address: PByte) of object;

  nw_ethernet_channel_send_completion_t = procedure(error: nw_error_t) of object;
  nw_framer_t = Pointer;
  Pnw_framer_t = ^nw_framer_t;
  nw_framer_message_t = Pointer;
  Pnw_framer_message_t = ^nw_framer_message_t;

  nw_framer_message_dispose_value_t = procedure(value: Pointer) of object;

  nw_framer_start_handler_t = function(framer: nw_framer_t): nw_framer_start_result_t of object;

  nw_framer_input_handler_t = function(framer: nw_framer_t): NativeUInt of object;

  nw_framer_output_handler_t = procedure(framer: nw_framer_t; message: nw_framer_message_t; message_length: NativeUInt;
    is_complete: Boolean) of object;

  nw_framer_wakeup_handler_t = procedure(framer: nw_framer_t) of object;

  nw_framer_stop_handler_t = function(framer: nw_framer_t): Boolean of object;

  nw_framer_cleanup_handler_t = procedure(framer: nw_framer_t) of object;

  nw_framer_parse_completion_t = function(buffer: PUInt8; buffer_length: NativeUInt; is_complete: Boolean): NativeUInt of object;

  nw_framer_block_t = procedure of object;

function _nw_privacy_context_default_context: nw_privacy_context_t;
function kNWErrorDomainPOSIX: CFStringRef;
function kNWErrorDomainDNS: CFStringRef;
function kNWErrorDomainTLS: CFStringRef;
function _nw_content_context_default_message: nw_content_context_t;
function _nw_content_context_final_send: nw_content_context_t;
function _nw_content_context_default_stream: nw_content_context_t;

const
  libNetwork = '/System/Library/Frameworks/Network.framework/Network';

function nw_retain(obj: Pointer): Pointer; cdecl;
  external libNetwork name _PU + 'nw_retain';

procedure nw_release(obj: Pointer); cdecl;
  external libNetwork name _PU + 'nw_release';

function nw_interface_get_type(&interface: nw_interface_t): nw_interface_type_t; cdecl;
  external libNetwork name _PU + 'nw_interface_get_type';

function nw_interface_get_name(&interface: nw_interface_t): PAnsiChar; cdecl;
  external libNetwork name _PU + 'nw_interface_get_name';

function nw_interface_get_index(&interface: nw_interface_t): UInt32; cdecl;
  external libNetwork name _PU + 'nw_interface_get_index';

function nw_txt_record_create_with_bytes(txt_bytes: PUInt8; txt_len: NativeUInt): nw_txt_record_t; cdecl;
  external libNetwork name _PU + 'nw_txt_record_create_with_bytes';

function nw_txt_record_create_dictionary: nw_txt_record_t; cdecl;
  external libNetwork name _PU + 'nw_txt_record_create_dictionary';

function nw_txt_record_copy(txt_record: nw_txt_record_t): nw_txt_record_t; cdecl;
  external libNetwork name _PU + 'nw_txt_record_copy';

function nw_txt_record_find_key(txt_record: nw_txt_record_t; key: PAnsiChar): nw_txt_record_find_key_t; cdecl;
  external libNetwork name _PU + 'nw_txt_record_find_key';

function nw_txt_record_access_key(txt_record: nw_txt_record_t; key: PAnsiChar; access_value: nw_txt_record_access_key_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_txt_record_access_key';

function nw_txt_record_set_key(txt_record: nw_txt_record_t; key: PAnsiChar; value: PUInt8; value_len: NativeUInt): Boolean; cdecl;
  external libNetwork name _PU + 'nw_txt_record_set_key';

function nw_txt_record_remove_key(txt_record: nw_txt_record_t; key: PAnsiChar): Boolean; cdecl;
  external libNetwork name _PU + 'nw_txt_record_remove_key';

function nw_txt_record_get_key_count(txt_record: nw_txt_record_t): NativeUInt; cdecl;
  external libNetwork name _PU + 'nw_txt_record_get_key_count';

function nw_txt_record_access_bytes(txt_record: nw_txt_record_t; access_bytes: nw_txt_record_access_bytes_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_txt_record_access_bytes';

function nw_txt_record_apply(txt_record: nw_txt_record_t; applier: nw_txt_record_applier_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_txt_record_apply';

function nw_txt_record_is_equal(left: nw_txt_record_t; right: nw_txt_record_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_txt_record_is_equal';

function nw_txt_record_is_dictionary(txt_record: nw_txt_record_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_txt_record_is_dictionary';

function nw_endpoint_get_type(endpoint: nw_endpoint_t): nw_endpoint_type_t; cdecl;
  external libNetwork name _PU + 'nw_endpoint_get_type';

function nw_endpoint_create_host(hostname: PAnsiChar; port: PAnsiChar): nw_endpoint_t; cdecl;
  external libNetwork name _PU + 'nw_endpoint_create_host';

function nw_endpoint_get_hostname(endpoint: nw_endpoint_t): PAnsiChar; cdecl;
  external libNetwork name _PU + 'nw_endpoint_get_hostname';

function nw_endpoint_copy_port_string(endpoint: nw_endpoint_t): PAnsiChar; cdecl;
  external libNetwork name _PU + 'nw_endpoint_copy_port_string';

function nw_endpoint_get_port(endpoint: nw_endpoint_t): UInt16; cdecl;
  external libNetwork name _PU + 'nw_endpoint_get_port';

function nw_endpoint_create_address(address: Psockaddr): nw_endpoint_t; cdecl;
  external libNetwork name _PU + 'nw_endpoint_create_address';

function nw_endpoint_copy_address_string(endpoint: nw_endpoint_t): PAnsiChar; cdecl;
  external libNetwork name _PU + 'nw_endpoint_copy_address_string';

function nw_endpoint_get_address(endpoint: nw_endpoint_t): Psockaddr; cdecl;
  external libNetwork name _PU + 'nw_endpoint_get_address';

function nw_endpoint_create_bonjour_service(name: PAnsiChar; &type: PAnsiChar; domain: PAnsiChar): nw_endpoint_t; cdecl;
  external libNetwork name _PU + 'nw_endpoint_create_bonjour_service';

function nw_endpoint_get_bonjour_service_name(endpoint: nw_endpoint_t): PAnsiChar; cdecl;
  external libNetwork name _PU + 'nw_endpoint_get_bonjour_service_name';

function nw_endpoint_get_bonjour_service_type(endpoint: nw_endpoint_t): PAnsiChar; cdecl;
  external libNetwork name _PU + 'nw_endpoint_get_bonjour_service_type';

function nw_endpoint_get_bonjour_service_domain(endpoint: nw_endpoint_t): PAnsiChar; cdecl;
  external libNetwork name _PU + 'nw_endpoint_get_bonjour_service_domain';

function nw_endpoint_create_url(url: PAnsiChar): nw_endpoint_t; cdecl;
  external libNetwork name _PU + 'nw_endpoint_create_url';

function nw_endpoint_get_url(endpoint: nw_endpoint_t): PAnsiChar; cdecl;
  external libNetwork name _PU + 'nw_endpoint_get_url';

function nw_protocol_definition_is_equal(definition1: nw_protocol_definition_t; definition2: nw_protocol_definition_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_protocol_definition_is_equal';

function nw_protocol_options_copy_definition(options: nw_protocol_options_t): nw_protocol_definition_t; cdecl;
  external libNetwork name _PU + 'nw_protocol_options_copy_definition';

function nw_protocol_metadata_copy_definition(metadata: nw_protocol_metadata_t): nw_protocol_definition_t; cdecl;
  external libNetwork name _PU + 'nw_protocol_metadata_copy_definition';

function nw_resolver_config_create_https(url_endpoint: nw_endpoint_t): nw_resolver_config_t; cdecl;
  external libNetwork name _PU + 'nw_resolver_config_create_https';

function nw_resolver_config_create_tls(server_endpoint: nw_endpoint_t): nw_resolver_config_t; cdecl;
  external libNetwork name _PU + 'nw_resolver_config_create_tls';

procedure nw_resolver_config_add_server_address(config: nw_resolver_config_t; server_address: nw_endpoint_t); cdecl;
  external libNetwork name _PU + 'nw_resolver_config_add_server_address';

function nw_privacy_context_create(description: PAnsiChar): nw_privacy_context_t; cdecl;
  external libNetwork name _PU + 'nw_privacy_context_create';

procedure nw_privacy_context_flush_cache(privacy_context: nw_privacy_context_t); cdecl;
  external libNetwork name _PU + 'nw_privacy_context_flush_cache';

procedure nw_privacy_context_disable_logging(privacy_context: nw_privacy_context_t); cdecl;
  external libNetwork name _PU + 'nw_privacy_context_disable_logging';

procedure nw_privacy_context_require_encrypted_name_resolution(privacy_context: nw_privacy_context_t; require_encrypted_name_resolution: Boolean;
  fallback_resolver_config: nw_resolver_config_t); cdecl;
  external libNetwork name _PU + 'nw_privacy_context_require_encrypted_name_resolution';

function nw_parameters_create_secure_tcp(configure_tls: nw_parameters_configure_protocol_block_t;
  configure_tcp: nw_parameters_configure_protocol_block_t): nw_parameters_t; cdecl;
  external libNetwork name _PU + 'nw_parameters_create_secure_tcp';

function nw_parameters_create_secure_udp(configure_dtls: nw_parameters_configure_protocol_block_t;
  configure_udp: nw_parameters_configure_protocol_block_t): nw_parameters_t; cdecl;
  external libNetwork name _PU + 'nw_parameters_create_secure_udp';

function nw_parameters_create_custom_ip(custom_ip_protocol_number: UInt8; configure_ip:
  nw_parameters_configure_protocol_block_t): nw_parameters_t; cdecl;
  external libNetwork name _PU + 'nw_parameters_create_custom_ip';

function nw_parameters_create: nw_parameters_t; cdecl;
  external libNetwork name _PU + 'nw_parameters_create';

function nw_parameters_copy(parameters: nw_parameters_t): nw_parameters_t; cdecl;
  external libNetwork name _PU + 'nw_parameters_copy';

procedure nw_parameters_set_privacy_context(parameters: nw_parameters_t; privacy_context: nw_privacy_context_t); cdecl;
  external libNetwork name _PU + 'nw_parameters_set_privacy_context';

procedure nw_parameters_require_interface(parameters: nw_parameters_t; &interface: nw_interface_t); cdecl;
  external libNetwork name _PU + 'nw_parameters_require_interface';

function nw_parameters_copy_required_interface(parameters: nw_parameters_t): nw_interface_t; cdecl;
  external libNetwork name _PU + 'nw_parameters_copy_required_interface';

procedure nw_parameters_prohibit_interface(parameters: nw_parameters_t; &interface: nw_interface_t); cdecl;
  external libNetwork name _PU + 'nw_parameters_prohibit_interface';

procedure nw_parameters_clear_prohibited_interfaces(parameters: nw_parameters_t); cdecl;
  external libNetwork name _PU + 'nw_parameters_clear_prohibited_interfaces';

procedure nw_parameters_iterate_prohibited_interfaces(parameters: nw_parameters_t; iterate_block: nw_parameters_iterate_interfaces_block_t); cdecl;
  external libNetwork name _PU + 'nw_parameters_iterate_prohibited_interfaces';

procedure nw_parameters_set_required_interface_type(parameters: nw_parameters_t; interface_type: nw_interface_type_t); cdecl;
  external libNetwork name _PU + 'nw_parameters_set_required_interface_type';

function nw_parameters_get_required_interface_type(parameters: nw_parameters_t): nw_interface_type_t; cdecl;
  external libNetwork name _PU + 'nw_parameters_get_required_interface_type';

procedure nw_parameters_prohibit_interface_type(parameters: nw_parameters_t; interface_type: nw_interface_type_t); cdecl;
  external libNetwork name _PU + 'nw_parameters_prohibit_interface_type';

procedure nw_parameters_clear_prohibited_interface_types(parameters: nw_parameters_t); cdecl;
  external libNetwork name _PU + 'nw_parameters_clear_prohibited_interface_types';

procedure nw_parameters_iterate_prohibited_interface_types(parameters: nw_parameters_t;
  iterate_block: nw_parameters_iterate_interface_types_block_t); cdecl;
  external libNetwork name _PU + 'nw_parameters_iterate_prohibited_interface_types';

procedure nw_parameters_set_prohibit_expensive(parameters: nw_parameters_t; prohibit_expensive: Boolean); cdecl;
  external libNetwork name _PU + 'nw_parameters_set_prohibit_expensive';

function nw_parameters_get_prohibit_expensive(parameters: nw_parameters_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_parameters_get_prohibit_expensive';

procedure nw_parameters_set_prohibit_constrained(parameters: nw_parameters_t; prohibit_constrained: Boolean); cdecl;
  external libNetwork name _PU + 'nw_parameters_set_prohibit_constrained';

function nw_parameters_get_prohibit_constrained(parameters: nw_parameters_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_parameters_get_prohibit_constrained';

procedure nw_parameters_set_reuse_local_address(parameters: nw_parameters_t; reuse_local_address: Boolean); cdecl;
  external libNetwork name _PU + 'nw_parameters_set_reuse_local_address';

function nw_parameters_get_reuse_local_address(parameters: nw_parameters_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_parameters_get_reuse_local_address';

procedure nw_parameters_set_local_endpoint(parameters: nw_parameters_t; local_endpoint: nw_endpoint_t); cdecl;
  external libNetwork name _PU + 'nw_parameters_set_local_endpoint';

function nw_parameters_copy_local_endpoint(parameters: nw_parameters_t): nw_endpoint_t; cdecl;
  external libNetwork name _PU + 'nw_parameters_copy_local_endpoint';

procedure nw_parameters_set_include_peer_to_peer(parameters: nw_parameters_t; include_peer_to_peer: Boolean); cdecl;
  external libNetwork name _PU + 'nw_parameters_set_include_peer_to_peer';

function nw_parameters_get_include_peer_to_peer(parameters: nw_parameters_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_parameters_get_include_peer_to_peer';

procedure nw_parameters_set_fast_open_enabled(parameters: nw_parameters_t; fast_open_enabled: Boolean); cdecl;
  external libNetwork name _PU + 'nw_parameters_set_fast_open_enabled';

function nw_parameters_get_fast_open_enabled(parameters: nw_parameters_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_parameters_get_fast_open_enabled';

procedure nw_parameters_set_service_class(parameters: nw_parameters_t; service_class: nw_service_class_t); cdecl;
  external libNetwork name _PU + 'nw_parameters_set_service_class';

function nw_parameters_get_service_class(parameters: nw_parameters_t): nw_service_class_t; cdecl;
  external libNetwork name _PU + 'nw_parameters_get_service_class';

procedure nw_parameters_set_multipath_service(parameters: nw_parameters_t; multipath_service: nw_multipath_service_t); cdecl;
  external libNetwork name _PU + 'nw_parameters_set_multipath_service';

function nw_parameters_get_multipath_service(parameters: nw_parameters_t): nw_multipath_service_t; cdecl;
  external libNetwork name _PU + 'nw_parameters_get_multipath_service';

function nw_parameters_copy_default_protocol_stack(parameters: nw_parameters_t): nw_protocol_stack_t; cdecl;
  external libNetwork name _PU + 'nw_parameters_copy_default_protocol_stack';

procedure nw_protocol_stack_prepend_application_protocol(stack: nw_protocol_stack_t; protocol: nw_protocol_options_t); cdecl;
  external libNetwork name _PU + 'nw_protocol_stack_prepend_application_protocol';

procedure nw_protocol_stack_clear_application_protocols(stack: nw_protocol_stack_t); cdecl;
  external libNetwork name _PU + 'nw_protocol_stack_clear_application_protocols';

procedure nw_protocol_stack_iterate_application_protocols(stack: nw_protocol_stack_t;
  iterate_block: nw_protocol_stack_iterate_protocols_block_t); cdecl;
  external libNetwork name _PU + 'nw_protocol_stack_iterate_application_protocols';

function nw_protocol_stack_copy_transport_protocol(stack: nw_protocol_stack_t): nw_protocol_options_t; cdecl;
  external libNetwork name _PU + 'nw_protocol_stack_copy_transport_protocol';

procedure nw_protocol_stack_set_transport_protocol(stack: nw_protocol_stack_t; protocol: nw_protocol_options_t); cdecl;
  external libNetwork name _PU + 'nw_protocol_stack_set_transport_protocol';

function nw_protocol_stack_copy_internet_protocol(stack: nw_protocol_stack_t): nw_protocol_options_t; cdecl;
  external libNetwork name _PU + 'nw_protocol_stack_copy_internet_protocol';

procedure nw_parameters_set_local_only(parameters: nw_parameters_t; local_only: Boolean); cdecl;
  external libNetwork name _PU + 'nw_parameters_set_local_only';

function nw_parameters_get_local_only(parameters: nw_parameters_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_parameters_get_local_only';

procedure nw_parameters_set_prefer_no_proxy(parameters: nw_parameters_t; prefer_no_proxy: Boolean); cdecl;
  external libNetwork name _PU + 'nw_parameters_set_prefer_no_proxy';

function nw_parameters_get_prefer_no_proxy(parameters: nw_parameters_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_parameters_get_prefer_no_proxy';

procedure nw_parameters_set_expired_dns_behavior(parameters: nw_parameters_t; expired_dns_behavior: nw_parameters_expired_dns_behavior_t); cdecl;
  external libNetwork name _PU + 'nw_parameters_set_expired_dns_behavior';

function nw_parameters_get_expired_dns_behavior(parameters: nw_parameters_t): nw_parameters_expired_dns_behavior_t; cdecl;
  external libNetwork name _PU + 'nw_parameters_get_expired_dns_behavior';

function nw_path_get_status(path: nw_path_t): nw_path_status_t; cdecl;
  external libNetwork name _PU + 'nw_path_get_status';

function nw_path_get_unsatisfied_reason(path: nw_path_t): nw_path_unsatisfied_reason_t; cdecl;
  external libNetwork name _PU + 'nw_path_get_unsatisfied_reason';

procedure nw_path_enumerate_interfaces(path: nw_path_t; enumerate_block: nw_path_enumerate_interfaces_block_t); cdecl;
  external libNetwork name _PU + 'nw_path_enumerate_interfaces';

function nw_path_is_equal(path: nw_path_t; other_path: nw_path_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_path_is_equal';

function nw_path_is_expensive(path: nw_path_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_path_is_expensive';

function nw_path_is_constrained(path: nw_path_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_path_is_constrained';

function nw_path_has_ipv4(path: nw_path_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_path_has_ipv4';

function nw_path_has_ipv6(path: nw_path_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_path_has_ipv6';

function nw_path_has_dns(path: nw_path_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_path_has_dns';

function nw_path_uses_interface_type(path: nw_path_t; interface_type: nw_interface_type_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_path_uses_interface_type';

function nw_path_copy_effective_local_endpoint(path: nw_path_t): nw_endpoint_t; cdecl;
  external libNetwork name _PU + 'nw_path_copy_effective_local_endpoint';

function nw_path_copy_effective_remote_endpoint(path: nw_path_t): nw_endpoint_t; cdecl;
  external libNetwork name _PU + 'nw_path_copy_effective_remote_endpoint';

procedure nw_path_enumerate_gateways(path: nw_path_t; enumerate_block: nw_path_enumerate_gateways_block_t); cdecl;
  external libNetwork name _PU + 'nw_path_enumerate_gateways';

function nw_content_context_create(context_identifier: PAnsiChar): nw_content_context_t; cdecl;
  external libNetwork name _PU + 'nw_content_context_create';

function nw_content_context_get_identifier(context: nw_content_context_t): PAnsiChar; cdecl;
  external libNetwork name _PU + 'nw_content_context_get_identifier';

function nw_content_context_get_is_final(context: nw_content_context_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_content_context_get_is_final';

procedure nw_content_context_set_is_final(context: nw_content_context_t; is_final: Boolean); cdecl;
  external libNetwork name _PU + 'nw_content_context_set_is_final';

function nw_content_context_get_expiration_milliseconds(context: nw_content_context_t): UInt64; cdecl;
  external libNetwork name _PU + 'nw_content_context_get_expiration_milliseconds';

procedure nw_content_context_set_expiration_milliseconds(context: nw_content_context_t; expiration_milliseconds: UInt64); cdecl;
  external libNetwork name _PU + 'nw_content_context_set_expiration_milliseconds';

function nw_content_context_get_relative_priority(context: nw_content_context_t): Double; cdecl;
  external libNetwork name _PU + 'nw_content_context_get_relative_priority';

procedure nw_content_context_set_relative_priority(context: nw_content_context_t; relative_priority: Double); cdecl;
  external libNetwork name _PU + 'nw_content_context_set_relative_priority';

procedure nw_content_context_set_antecedent(context: nw_content_context_t; antecendent_context: nw_content_context_t); cdecl;
  external libNetwork name _PU + 'nw_content_context_set_antecedent';

function nw_content_context_copy_antecedent(context: nw_content_context_t): nw_content_context_t; cdecl;
  external libNetwork name _PU + 'nw_content_context_copy_antecedent';

procedure nw_content_context_set_metadata_for_protocol(context: nw_content_context_t; protocol_metadata: nw_protocol_metadata_t); cdecl;
  external libNetwork name _PU + 'nw_content_context_set_metadata_for_protocol';

function nw_content_context_copy_protocol_metadata(context: nw_content_context_t; protocol: nw_protocol_definition_t): nw_protocol_metadata_t; cdecl;
  external libNetwork name _PU + 'nw_content_context_copy_protocol_metadata';

type
  nw_content_context_foreach_protocol_metadata_foreach_block =
    procedure(definition: nw_protocol_definition_t; metadata: nw_protocol_metadata_t); cdecl;

procedure nw_content_context_foreach_protocol_metadata(context: nw_content_context_t;
  foreach_block: nw_content_context_foreach_protocol_metadata_foreach_block); cdecl;
  external libNetwork name _PU + 'nw_content_context_foreach_protocol_metadata';

function nw_error_get_error_domain(error: nw_error_t): nw_error_domain_t; cdecl;
  external libNetwork name _PU + 'nw_error_get_error_domain';

function nw_error_get_error_code(error: nw_error_t): Integer; cdecl;
  external libNetwork name _PU + 'nw_error_get_error_code';

function nw_error_copy_cf_error(error: nw_error_t): CFErrorRef; cdecl;
  external libNetwork name _PU + 'nw_error_copy_cf_error';

function nw_connection_create(endpoint: nw_endpoint_t; parameters: nw_parameters_t): nw_connection_t; cdecl;
  external libNetwork name _PU + 'nw_connection_create';

function nw_connection_copy_endpoint(connection: nw_connection_t): nw_endpoint_t; cdecl;
  external libNetwork name _PU + 'nw_connection_copy_endpoint';

function nw_connection_copy_parameters(connection: nw_connection_t): nw_parameters_t; cdecl;
  external libNetwork name _PU + 'nw_connection_copy_parameters';

procedure nw_connection_set_state_changed_handler(connection: nw_connection_t; handler: nw_connection_state_changed_handler_t); cdecl;
  external libNetwork name _PU + 'nw_connection_set_state_changed_handler';

procedure nw_connection_set_viability_changed_handler(connection: nw_connection_t; handler: nw_connection_boolean_event_handler_t); cdecl;
  external libNetwork name _PU + 'nw_connection_set_viability_changed_handler';

procedure nw_connection_set_better_path_available_handler(connection: nw_connection_t; handler: nw_connection_boolean_event_handler_t); cdecl;
  external libNetwork name _PU + 'nw_connection_set_better_path_available_handler';

procedure nw_connection_set_path_changed_handler(connection: nw_connection_t; handler: nw_connection_path_event_handler_t); cdecl;
  external libNetwork name _PU + 'nw_connection_set_path_changed_handler';

procedure nw_connection_set_queue(connection: nw_connection_t; queue: dispatch_queue_t); cdecl;
  external libNetwork name _PU + 'nw_connection_set_queue';

procedure nw_connection_start(connection: nw_connection_t); cdecl;
  external libNetwork name _PU + 'nw_connection_start';

procedure nw_connection_restart(connection: nw_connection_t); cdecl;
  external libNetwork name _PU + 'nw_connection_restart';

procedure nw_connection_cancel(connection: nw_connection_t); cdecl;
  external libNetwork name _PU + 'nw_connection_cancel';

procedure nw_connection_force_cancel(connection: nw_connection_t); cdecl;
  external libNetwork name _PU + 'nw_connection_force_cancel';

procedure nw_connection_cancel_current_endpoint(connection: nw_connection_t); cdecl;
  external libNetwork name _PU + 'nw_connection_cancel_current_endpoint';

procedure nw_connection_receive(connection: nw_connection_t; minimum_incomplete_length: UInt32; maximum_length: UInt32;
  completion: nw_connection_receive_completion_t); cdecl;
  external libNetwork name _PU + 'nw_connection_receive';

procedure nw_connection_receive_message(connection: nw_connection_t; completion: nw_connection_receive_completion_t); cdecl;
  external libNetwork name _PU + 'nw_connection_receive_message';

procedure nw_connection_send(connection: nw_connection_t; content: dispatch_data_t; context: nw_content_context_t; is_complete: Boolean;
  completion: nw_connection_send_completion_t); cdecl;
  external libNetwork name _PU + 'nw_connection_send';

procedure nw_connection_batch(connection: nw_connection_t; batch_block: dispatch_block_t); cdecl;
  external libNetwork name _PU + 'nw_connection_batch';

function nw_connection_copy_description(connection: nw_connection_t): PAnsiChar; cdecl;
  external libNetwork name _PU + 'nw_connection_copy_description';

function nw_connection_copy_current_path(connection: nw_connection_t): nw_path_t; cdecl;
  external libNetwork name _PU + 'nw_connection_copy_current_path';

function nw_connection_copy_protocol_metadata(connection: nw_connection_t; definition: nw_protocol_definition_t): nw_protocol_metadata_t; cdecl;
  external libNetwork name _PU + 'nw_connection_copy_protocol_metadata';

function nw_connection_get_maximum_datagram_size(connection: nw_connection_t): UInt32; cdecl;
  external libNetwork name _PU + 'nw_connection_get_maximum_datagram_size';

function nw_advertise_descriptor_create_bonjour_service(name: PAnsiChar; &type: PAnsiChar; domain: PAnsiChar): nw_advertise_descriptor_t; cdecl;
  external libNetwork name _PU + 'nw_advertise_descriptor_create_bonjour_service';

procedure nw_advertise_descriptor_set_txt_record(advertise_descriptor: nw_advertise_descriptor_t; txt_record: Pointer; txt_length: NativeUInt); cdecl;
  external libNetwork name _PU + 'nw_advertise_descriptor_set_txt_record';

procedure nw_advertise_descriptor_set_no_auto_rename(advertise_descriptor: nw_advertise_descriptor_t; no_auto_rename: Boolean); cdecl;
  external libNetwork name _PU + 'nw_advertise_descriptor_set_no_auto_rename';

function nw_advertise_descriptor_get_no_auto_rename(advertise_descriptor: nw_advertise_descriptor_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_advertise_descriptor_get_no_auto_rename';

procedure nw_advertise_descriptor_set_txt_record_object(advertise_descriptor: nw_advertise_descriptor_t; txt_record: nw_txt_record_t); cdecl;
  external libNetwork name _PU + 'nw_advertise_descriptor_set_txt_record_object';

function nw_advertise_descriptor_copy_txt_record_object(advertise_descriptor: nw_advertise_descriptor_t): nw_txt_record_t; cdecl;
  external libNetwork name _PU + 'nw_advertise_descriptor_copy_txt_record_object';

function nw_protocol_copy_udp_definition: nw_protocol_definition_t; cdecl;
  external libNetwork name _PU + 'nw_protocol_copy_udp_definition';

function nw_udp_create_options: nw_protocol_options_t; cdecl;
  external libNetwork name _PU + 'nw_udp_create_options';

procedure nw_udp_options_set_prefer_no_checksum(options: nw_protocol_options_t; prefer_no_checksum: Boolean); cdecl;
  external libNetwork name _PU + 'nw_udp_options_set_prefer_no_checksum';

function nw_udp_create_metadata: nw_protocol_metadata_t; cdecl;
  external libNetwork name _PU + 'nw_udp_create_metadata';

function nw_protocol_metadata_is_udp(metadata: nw_protocol_metadata_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_protocol_metadata_is_udp';

function nw_listener_create_with_port(port: PAnsiChar; parameters: nw_parameters_t): nw_listener_t; cdecl;
  external libNetwork name _PU + 'nw_listener_create_with_port';

function nw_listener_create(parameters: nw_parameters_t): nw_listener_t; cdecl;
  external libNetwork name _PU + 'nw_listener_create';

function nw_listener_create_with_connection(connection: nw_connection_t; parameters: nw_parameters_t): nw_listener_t; cdecl;
  external libNetwork name _PU + 'nw_listener_create_with_connection';

procedure nw_listener_set_queue(listener: nw_listener_t; queue: dispatch_queue_t); cdecl;
  external libNetwork name _PU + 'nw_listener_set_queue';

procedure nw_listener_set_state_changed_handler(listener: nw_listener_t; handler: nw_listener_state_changed_handler_t); cdecl;
  external libNetwork name _PU + 'nw_listener_set_state_changed_handler';

procedure nw_listener_set_new_connection_handler(listener: nw_listener_t; handler: nw_listener_new_connection_handler_t); cdecl;
  external libNetwork name _PU + 'nw_listener_set_new_connection_handler';

function nw_listener_get_new_connection_limit(listener: nw_listener_t): UInt32; cdecl;
  external libNetwork name _PU + 'nw_listener_get_new_connection_limit';

procedure nw_listener_set_new_connection_limit(listener: nw_listener_t; new_connection_limit: UInt32); cdecl;
  external libNetwork name _PU + 'nw_listener_set_new_connection_limit';

procedure nw_listener_set_advertise_descriptor(listener: nw_listener_t; advertise_descriptor: nw_advertise_descriptor_t); cdecl;
  external libNetwork name _PU + 'nw_listener_set_advertise_descriptor';

procedure nw_listener_set_advertised_endpoint_changed_handler(listener: nw_listener_t;
  handler: nw_listener_advertised_endpoint_changed_handler_t); cdecl;
  external libNetwork name _PU + 'nw_listener_set_advertised_endpoint_changed_handler';

function nw_listener_get_port(listener: nw_listener_t): UInt16; cdecl;
  external libNetwork name _PU + 'nw_listener_get_port';

procedure nw_listener_start(listener: nw_listener_t); cdecl;
  external libNetwork name _PU + 'nw_listener_start';

procedure nw_listener_cancel(listener: nw_listener_t); cdecl;
  external libNetwork name _PU + 'nw_listener_cancel';

function nw_browse_descriptor_create_bonjour_service(&type: PAnsiChar; domain: PAnsiChar): nw_browse_descriptor_t; cdecl;
  external libNetwork name _PU + 'nw_browse_descriptor_create_bonjour_service';

function nw_browse_descriptor_get_bonjour_service_type(descriptor: nw_browse_descriptor_t): PAnsiChar; cdecl;
  external libNetwork name _PU + 'nw_browse_descriptor_get_bonjour_service_type';

function nw_browse_descriptor_get_bonjour_service_domain(descriptor: nw_browse_descriptor_t): PAnsiChar; cdecl;
  external libNetwork name _PU + 'nw_browse_descriptor_get_bonjour_service_domain';

procedure nw_browse_descriptor_set_include_txt_record(descriptor: nw_browse_descriptor_t; include_txt_record: Boolean); cdecl;
  external libNetwork name _PU + 'nw_browse_descriptor_set_include_txt_record';

function nw_browse_descriptor_get_include_txt_record(descriptor: nw_browse_descriptor_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_browse_descriptor_get_include_txt_record';

function nw_protocol_copy_tls_definition: nw_protocol_definition_t; cdecl;
  external libNetwork name _PU + 'nw_protocol_copy_tls_definition';

function nw_tls_create_options: nw_protocol_options_t; cdecl;
  external libNetwork name _PU + 'nw_tls_create_options';

function nw_tls_copy_sec_protocol_options(options: nw_protocol_options_t): sec_protocol_options_t; cdecl;
  external libNetwork name _PU + 'nw_tls_copy_sec_protocol_options';

function nw_protocol_metadata_is_tls(metadata: nw_protocol_metadata_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_protocol_metadata_is_tls';

function nw_tls_copy_sec_protocol_metadata(metadata: nw_protocol_metadata_t): sec_protocol_metadata_t; cdecl;
  external libNetwork name _PU + 'nw_tls_copy_sec_protocol_metadata';

function nw_protocol_copy_ws_definition: nw_protocol_definition_t; cdecl;
  external libNetwork name _PU + 'nw_protocol_copy_ws_definition';

function nw_ws_create_options(version: nw_ws_version_t): nw_protocol_options_t; cdecl;
  external libNetwork name _PU + 'nw_ws_create_options';

procedure nw_ws_options_add_additional_header(options: nw_protocol_options_t; name: PAnsiChar; value: PAnsiChar); cdecl;
  external libNetwork name _PU + 'nw_ws_options_add_additional_header';

procedure nw_ws_options_add_subprotocol(options: nw_protocol_options_t; subprotocol: PAnsiChar); cdecl;
  external libNetwork name _PU + 'nw_ws_options_add_subprotocol';

procedure nw_ws_options_set_auto_reply_ping(options: nw_protocol_options_t; auto_reply_ping: Boolean); cdecl;
  external libNetwork name _PU + 'nw_ws_options_set_auto_reply_ping';

procedure nw_ws_options_set_skip_handshake(options: nw_protocol_options_t; skip_handshake: Boolean); cdecl;
  external libNetwork name _PU + 'nw_ws_options_set_skip_handshake';

procedure nw_ws_options_set_maximum_message_size(options: nw_protocol_options_t; maximum_message_size: NativeUInt); cdecl;
  external libNetwork name _PU + 'nw_ws_options_set_maximum_message_size';

function nw_protocol_metadata_is_ws(metadata: nw_protocol_metadata_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_protocol_metadata_is_ws';

function nw_ws_create_metadata(opcode: nw_ws_opcode_t): nw_protocol_metadata_t; cdecl;
  external libNetwork name _PU + 'nw_ws_create_metadata';

function nw_ws_metadata_get_opcode(metadata: nw_protocol_metadata_t): nw_ws_opcode_t; cdecl;
  external libNetwork name _PU + 'nw_ws_metadata_get_opcode';

procedure nw_ws_metadata_set_close_code(metadata: nw_protocol_metadata_t; close_code: nw_ws_close_code_t); cdecl;
  external libNetwork name _PU + 'nw_ws_metadata_set_close_code';

function nw_ws_metadata_get_close_code(metadata: nw_protocol_metadata_t): nw_ws_close_code_t; cdecl;
  external libNetwork name _PU + 'nw_ws_metadata_get_close_code';

procedure nw_ws_metadata_set_pong_handler(metadata: nw_protocol_metadata_t; client_queue: dispatch_queue_t; pong_handler: nw_ws_pong_handler_t); cdecl;
  external libNetwork name _PU + 'nw_ws_metadata_set_pong_handler';

function nw_ws_request_enumerate_subprotocols(request: nw_ws_request_t; enumerator: nw_ws_subprotocol_enumerator_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_ws_request_enumerate_subprotocols';

function nw_ws_request_enumerate_additional_headers(request: nw_ws_request_t; enumerator: nw_ws_additional_header_enumerator_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_ws_request_enumerate_additional_headers';

function nw_ws_response_create(status: nw_ws_response_status_t; selected_subprotocol: PAnsiChar): nw_ws_response_t; cdecl;
  external libNetwork name _PU + 'nw_ws_response_create';

function nw_ws_response_get_status(response: nw_ws_response_t): nw_ws_response_status_t; cdecl;
  external libNetwork name _PU + 'nw_ws_response_get_status';

function nw_ws_response_get_selected_subprotocol(response: nw_ws_response_t): PAnsiChar; cdecl;
  external libNetwork name _PU + 'nw_ws_response_get_selected_subprotocol';

procedure nw_ws_response_add_additional_header(response: nw_ws_response_t; name: PAnsiChar; value: PAnsiChar); cdecl;
  external libNetwork name _PU + 'nw_ws_response_add_additional_header';

function nw_ws_metadata_copy_server_response(metadata: nw_protocol_metadata_t): nw_ws_response_t; cdecl;
  external libNetwork name _PU + 'nw_ws_metadata_copy_server_response';

function nw_ws_response_enumerate_additional_headers(response: nw_ws_response_t; enumerator: nw_ws_additional_header_enumerator_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_ws_response_enumerate_additional_headers';

procedure nw_ws_options_set_client_request_handler(options: nw_protocol_options_t; client_queue: dispatch_queue_t;
  handler: nw_ws_client_request_handler_t); cdecl;
  external libNetwork name _PU + 'nw_ws_options_set_client_request_handler';

function nw_browse_result_copy_endpoint(result: nw_browse_result_t): nw_endpoint_t; cdecl;
  external libNetwork name _PU + 'nw_browse_result_copy_endpoint';

function nw_browse_result_get_changes(old_result: nw_browse_result_t; new_result: nw_browse_result_t): nw_browse_result_change_t; cdecl;
  external libNetwork name _PU + 'nw_browse_result_get_changes';

function nw_browse_result_get_interfaces_count(result: nw_browse_result_t): NativeUInt; cdecl;
  external libNetwork name _PU + 'nw_browse_result_get_interfaces_count';

function nw_browse_result_copy_txt_record_object(result: nw_browse_result_t): nw_txt_record_t; cdecl;
  external libNetwork name _PU + 'nw_browse_result_copy_txt_record_object';

procedure nw_browse_result_enumerate_interfaces(result: nw_browse_result_t; enumerator: nw_browse_result_enumerate_interface_t); cdecl;
  external libNetwork name _PU + 'nw_browse_result_enumerate_interfaces';

function nw_browser_create(descriptor: nw_browse_descriptor_t; parameters: nw_parameters_t): nw_browser_t; cdecl;
  external libNetwork name _PU + 'nw_browser_create';

procedure nw_browser_set_queue(browser: nw_browser_t; queue: dispatch_queue_t); cdecl;
  external libNetwork name _PU + 'nw_browser_set_queue';

procedure nw_browser_set_browse_results_changed_handler(browser: nw_browser_t; handler: nw_browser_browse_results_changed_handler_t); cdecl;
  external libNetwork name _PU + 'nw_browser_set_browse_results_changed_handler';

procedure nw_browser_set_state_changed_handler(browser: nw_browser_t; state_changed_handler: nw_browser_state_changed_handler_t); cdecl;
  external libNetwork name _PU + 'nw_browser_set_state_changed_handler';

procedure nw_browser_start(browser: nw_browser_t); cdecl;
  external libNetwork name _PU + 'nw_browser_start';

procedure nw_browser_cancel(browser: nw_browser_t); cdecl;
  external libNetwork name _PU + 'nw_browser_cancel';

function nw_browser_copy_parameters(browser: nw_browser_t): nw_parameters_t; cdecl;
  external libNetwork name _PU + 'nw_browser_copy_parameters';

function nw_browser_copy_browse_descriptor(browser: nw_browser_t): nw_browse_descriptor_t; cdecl;
  external libNetwork name _PU + 'nw_browser_copy_browse_descriptor';

function nw_path_monitor_create: nw_path_monitor_t; cdecl;
  external libNetwork name _PU + 'nw_path_monitor_create';

function nw_path_monitor_create_with_type(required_interface_type: nw_interface_type_t): nw_path_monitor_t; cdecl;
  external libNetwork name _PU + 'nw_path_monitor_create_with_type';

procedure nw_path_monitor_prohibit_interface_type(monitor: nw_path_monitor_t; interface_type: nw_interface_type_t); cdecl;
  external libNetwork name _PU + 'nw_path_monitor_prohibit_interface_type';

procedure nw_path_monitor_set_cancel_handler(monitor: nw_path_monitor_t; cancel_handler: nw_path_monitor_cancel_handler_t); cdecl;
  external libNetwork name _PU + 'nw_path_monitor_set_cancel_handler';

procedure nw_path_monitor_set_update_handler(monitor: nw_path_monitor_t; update_handler: nw_path_monitor_update_handler_t); cdecl;
  external libNetwork name _PU + 'nw_path_monitor_set_update_handler';

procedure nw_path_monitor_set_queue(monitor: nw_path_monitor_t; queue: dispatch_queue_t); cdecl;
  external libNetwork name _PU + 'nw_path_monitor_set_queue';

procedure nw_path_monitor_start(monitor: nw_path_monitor_t); cdecl;
  external libNetwork name _PU + 'nw_path_monitor_start';

procedure nw_path_monitor_cancel(monitor: nw_path_monitor_t); cdecl;
  external libNetwork name _PU + 'nw_path_monitor_cancel';

function nw_protocol_copy_ip_definition: nw_protocol_definition_t; cdecl;
  external libNetwork name _PU + 'nw_protocol_copy_ip_definition';

procedure nw_ip_options_set_version(options: nw_protocol_options_t; version: nw_ip_version_t); cdecl;
  external libNetwork name _PU + 'nw_ip_options_set_version';

procedure nw_ip_options_set_hop_limit(options: nw_protocol_options_t; hop_limit: UInt8); cdecl;
  external libNetwork name _PU + 'nw_ip_options_set_hop_limit';

procedure nw_ip_options_set_use_minimum_mtu(options: nw_protocol_options_t; use_minimum_mtu: Boolean); cdecl;
  external libNetwork name _PU + 'nw_ip_options_set_use_minimum_mtu';

procedure nw_ip_options_set_disable_fragmentation(options: nw_protocol_options_t; disable_fragmentation: Boolean); cdecl;
  external libNetwork name _PU + 'nw_ip_options_set_disable_fragmentation';

procedure nw_ip_options_set_calculate_receive_time(options: nw_protocol_options_t; calculate_receive_time: Boolean); cdecl;
  external libNetwork name _PU + 'nw_ip_options_set_calculate_receive_time';

procedure nw_ip_options_set_local_address_preference(options: nw_protocol_options_t; preference: nw_ip_local_address_preference_t); cdecl;
  external libNetwork name _PU + 'nw_ip_options_set_local_address_preference';

procedure nw_ip_options_set_disable_multicast_loopback(options: nw_protocol_options_t; disable_multicast_loopback: Boolean); cdecl;
  external libNetwork name _PU + 'nw_ip_options_set_disable_multicast_loopback';

function nw_ip_create_metadata: nw_protocol_metadata_t; cdecl;
  external libNetwork name _PU + 'nw_ip_create_metadata';

function nw_protocol_metadata_is_ip(metadata: nw_protocol_metadata_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_protocol_metadata_is_ip';

procedure nw_ip_metadata_set_ecn_flag(metadata: nw_protocol_metadata_t; ecn_flag: nw_ip_ecn_flag_t); cdecl;
  external libNetwork name _PU + 'nw_ip_metadata_set_ecn_flag';

function nw_ip_metadata_get_ecn_flag(metadata: nw_protocol_metadata_t): nw_ip_ecn_flag_t; cdecl;
  external libNetwork name _PU + 'nw_ip_metadata_get_ecn_flag';

procedure nw_ip_metadata_set_service_class(metadata: nw_protocol_metadata_t; service_class: nw_service_class_t); cdecl;
  external libNetwork name _PU + 'nw_ip_metadata_set_service_class';

function nw_ip_metadata_get_service_class(metadata: nw_protocol_metadata_t): nw_service_class_t; cdecl;
  external libNetwork name _PU + 'nw_ip_metadata_get_service_class';

function nw_ip_metadata_get_receive_time(metadata: nw_protocol_metadata_t): UInt64; cdecl;
  external libNetwork name _PU + 'nw_ip_metadata_get_receive_time';

function nw_group_descriptor_create_multicast(multicast_group: nw_endpoint_t): nw_group_descriptor_t; cdecl;
  external libNetwork name _PU + 'nw_group_descriptor_create_multicast';

function nw_group_descriptor_add_endpoint(descriptor: nw_group_descriptor_t; endpoint: nw_endpoint_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_group_descriptor_add_endpoint';

procedure nw_group_descriptor_enumerate_endpoints(descriptor: nw_group_descriptor_t;
  enumerate_block: nw_group_descriptor_enumerate_endpoints_block_t); cdecl;
  external libNetwork name _PU + 'nw_group_descriptor_enumerate_endpoints';

procedure nw_multicast_group_descriptor_set_specific_source(multicast_descriptor: nw_group_descriptor_t; source: nw_endpoint_t); cdecl;
  external libNetwork name _PU + 'nw_multicast_group_descriptor_set_specific_source';

procedure nw_multicast_group_descriptor_set_disable_unicast_traffic(multicast_descriptor: nw_group_descriptor_t;
  disable_unicast_traffic: Boolean); cdecl;
  external libNetwork name _PU + 'nw_multicast_group_descriptor_set_disable_unicast_traffic';

function nw_multicast_group_descriptor_get_disable_unicast_traffic(multicast_descriptor: nw_group_descriptor_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_multicast_group_descriptor_get_disable_unicast_traffic';

function nw_connection_group_create(group_descriptor: nw_group_descriptor_t; parameters: nw_parameters_t): nw_connection_group_t; cdecl;
  external libNetwork name _PU + 'nw_connection_group_create';

function nw_connection_group_copy_descriptor(group: nw_connection_group_t): nw_group_descriptor_t; cdecl;
  external libNetwork name _PU + 'nw_connection_group_copy_descriptor';

function nw_connection_group_copy_parameters(group: nw_connection_group_t): nw_parameters_t; cdecl;
  external libNetwork name _PU + 'nw_connection_group_copy_parameters';

procedure nw_connection_group_set_queue(group: nw_connection_group_t; queue: dispatch_queue_t); cdecl;
  external libNetwork name _PU + 'nw_connection_group_set_queue';

procedure nw_connection_group_set_state_changed_handler(group: nw_connection_group_t;
  state_changed_handler: nw_connection_group_state_changed_handler_t); cdecl;
  external libNetwork name _PU + 'nw_connection_group_set_state_changed_handler';

procedure nw_connection_group_set_receive_handler(group: nw_connection_group_t; maximum_message_size: UInt32; reject_oversized_messages: Boolean;
  receive_handler: nw_connection_group_receive_handler_t); cdecl;
  external libNetwork name _PU + 'nw_connection_group_set_receive_handler';

procedure nw_connection_group_start(group: nw_connection_group_t); cdecl;
  external libNetwork name _PU + 'nw_connection_group_start';

procedure nw_connection_group_cancel(group: nw_connection_group_t); cdecl;
  external libNetwork name _PU + 'nw_connection_group_cancel';

function nw_connection_group_copy_remote_endpoint_for_message(group: nw_connection_group_t; context: nw_content_context_t): nw_endpoint_t; cdecl;
  external libNetwork name _PU + 'nw_connection_group_copy_remote_endpoint_for_message';

function nw_connection_group_copy_local_endpoint_for_message(group: nw_connection_group_t; context: nw_content_context_t): nw_endpoint_t; cdecl;
  external libNetwork name _PU + 'nw_connection_group_copy_local_endpoint_for_message';

function nw_connection_group_copy_path_for_message(group: nw_connection_group_t; context: nw_content_context_t): nw_path_t; cdecl;
  external libNetwork name _PU + 'nw_connection_group_copy_path_for_message';

procedure nw_connection_group_reply(group: nw_connection_group_t; inbound_message: nw_content_context_t; outbound_message: nw_content_context_t;
  content: dispatch_data_t); cdecl;
  external libNetwork name _PU + 'nw_connection_group_reply';

function nw_connection_group_extract_connection_for_message(group: nw_connection_group_t; context: nw_content_context_t): nw_connection_t; cdecl;
  external libNetwork name _PU + 'nw_connection_group_extract_connection_for_message';

procedure nw_connection_group_send_message(group: nw_connection_group_t; content: dispatch_data_t; endpoint: nw_endpoint_t;
  context: nw_content_context_t; completion: nw_connection_group_send_completion_t); cdecl;
  external libNetwork name _PU + 'nw_connection_group_send_message';

procedure nw_connection_access_establishment_report(connection: nw_connection_t; queue: dispatch_queue_t;
  access_block: nw_establishment_report_access_block_t); cdecl;
  external libNetwork name _PU + 'nw_connection_access_establishment_report';

function nw_establishment_report_get_duration_milliseconds(report: nw_establishment_report_t): UInt64; cdecl;
  external libNetwork name _PU + 'nw_establishment_report_get_duration_milliseconds';

function nw_establishment_report_get_attempt_started_after_milliseconds(report: nw_establishment_report_t): UInt64; cdecl;
  external libNetwork name _PU + 'nw_establishment_report_get_attempt_started_after_milliseconds';

function nw_establishment_report_get_previous_attempt_count(report: nw_establishment_report_t): UInt32; cdecl;
  external libNetwork name _PU + 'nw_establishment_report_get_previous_attempt_count';

function nw_establishment_report_get_used_proxy(report: nw_establishment_report_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_establishment_report_get_used_proxy';

function nw_establishment_report_get_proxy_configured(report: nw_establishment_report_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_establishment_report_get_proxy_configured';

function nw_establishment_report_copy_proxy_endpoint(report: nw_establishment_report_t): nw_endpoint_t; cdecl;
  external libNetwork name _PU + 'nw_establishment_report_copy_proxy_endpoint';

function nw_resolution_report_get_source(resolution_report: nw_resolution_report_t): nw_report_resolution_source_t; cdecl;
  external libNetwork name _PU + 'nw_resolution_report_get_source';

function nw_resolution_report_get_milliseconds(resolution_report: nw_resolution_report_t): UInt64; cdecl;
  external libNetwork name _PU + 'nw_resolution_report_get_milliseconds';

function nw_resolution_report_get_endpoint_count(resolution_report: nw_resolution_report_t): UInt32; cdecl;
  external libNetwork name _PU + 'nw_resolution_report_get_endpoint_count';

function nw_resolution_report_copy_successful_endpoint(resolution_report: nw_resolution_report_t): nw_endpoint_t; cdecl;
  external libNetwork name _PU + 'nw_resolution_report_copy_successful_endpoint';

function nw_resolution_report_copy_preferred_endpoint(resolution_report: nw_resolution_report_t): nw_endpoint_t; cdecl;
  external libNetwork name _PU + 'nw_resolution_report_copy_preferred_endpoint';

function nw_resolution_report_get_protocol(resolution_report: nw_resolution_report_t): nw_report_resolution_protocol_t; cdecl;
  external libNetwork name _PU + 'nw_resolution_report_get_protocol';

procedure nw_establishment_report_enumerate_resolutions(report: nw_establishment_report_t; enumerate_block: nw_report_resolution_enumerator_t); cdecl;
  external libNetwork name _PU + 'nw_establishment_report_enumerate_resolutions';

procedure nw_establishment_report_enumerate_resolution_reports(report: nw_establishment_report_t;
  enumerate_block: nw_report_resolution_report_enumerator_t); cdecl;
  external libNetwork name _PU + 'nw_establishment_report_enumerate_resolution_reports';

procedure nw_establishment_report_enumerate_protocols(report: nw_establishment_report_t; enumerate_block: nw_report_protocol_enumerator_t); cdecl;
  external libNetwork name _PU + 'nw_establishment_report_enumerate_protocols';

function nw_connection_create_new_data_transfer_report(connection: nw_connection_t): nw_data_transfer_report_t; cdecl;
  external libNetwork name _PU + 'nw_connection_create_new_data_transfer_report';

function nw_data_transfer_report_get_state(report: nw_data_transfer_report_t): nw_data_transfer_report_state_t; cdecl;
  external libNetwork name _PU + 'nw_data_transfer_report_get_state';

procedure nw_data_transfer_report_collect(report: nw_data_transfer_report_t; queue: dispatch_queue_t;
  collect_block: nw_data_transfer_report_collect_block_t); cdecl;
  external libNetwork name _PU + 'nw_data_transfer_report_collect';

function nw_data_transfer_report_get_duration_milliseconds(report: nw_data_transfer_report_t): UInt64; cdecl;
  external libNetwork name _PU + 'nw_data_transfer_report_get_duration_milliseconds';

function nw_data_transfer_report_get_path_count(report: nw_data_transfer_report_t): UInt32; cdecl;
  external libNetwork name _PU + 'nw_data_transfer_report_get_path_count';

function nw_data_transfer_report_get_received_ip_packet_count(report: nw_data_transfer_report_t; path_index: UInt32): UInt64; cdecl;
  external libNetwork name _PU + 'nw_data_transfer_report_get_received_ip_packet_count';

function nw_data_transfer_report_get_sent_ip_packet_count(report: nw_data_transfer_report_t; path_index: UInt32): UInt64; cdecl;
  external libNetwork name _PU + 'nw_data_transfer_report_get_sent_ip_packet_count';

function nw_data_transfer_report_get_received_transport_byte_count(report: nw_data_transfer_report_t; path_index: UInt32): UInt64; cdecl;
  external libNetwork name _PU + 'nw_data_transfer_report_get_received_transport_byte_count';

function nw_data_transfer_report_get_received_transport_duplicate_byte_count(report: nw_data_transfer_report_t; path_index: UInt32): UInt64; cdecl;
  external libNetwork name _PU + 'nw_data_transfer_report_get_received_transport_duplicate_byte_count';

function nw_data_transfer_report_get_received_transport_out_of_order_byte_count(report: nw_data_transfer_report_t; path_index: UInt32): UInt64; cdecl;
  external libNetwork name _PU + 'nw_data_transfer_report_get_received_transport_out_of_order_byte_count';

function nw_data_transfer_report_get_sent_transport_byte_count(report: nw_data_transfer_report_t; path_index: UInt32): UInt64; cdecl;
  external libNetwork name _PU + 'nw_data_transfer_report_get_sent_transport_byte_count';

function nw_data_transfer_report_get_sent_transport_retransmitted_byte_count(report: nw_data_transfer_report_t; path_index: UInt32): UInt64; cdecl;
  external libNetwork name _PU + 'nw_data_transfer_report_get_sent_transport_retransmitted_byte_count';

function nw_data_transfer_report_get_transport_smoothed_rtt_milliseconds(report: nw_data_transfer_report_t; path_index: UInt32): UInt64; cdecl;
  external libNetwork name _PU + 'nw_data_transfer_report_get_transport_smoothed_rtt_milliseconds';

function nw_data_transfer_report_get_transport_minimum_rtt_milliseconds(report: nw_data_transfer_report_t; path_index: UInt32): UInt64; cdecl;
  external libNetwork name _PU + 'nw_data_transfer_report_get_transport_minimum_rtt_milliseconds';

function nw_data_transfer_report_get_transport_rtt_variance(report: nw_data_transfer_report_t; path_index: UInt32): UInt64; cdecl;
  external libNetwork name _PU + 'nw_data_transfer_report_get_transport_rtt_variance';

function nw_data_transfer_report_get_received_application_byte_count(report: nw_data_transfer_report_t; path_index: UInt32): UInt64; cdecl;
  external libNetwork name _PU + 'nw_data_transfer_report_get_received_application_byte_count';

function nw_data_transfer_report_get_sent_application_byte_count(report: nw_data_transfer_report_t; path_index: UInt32): UInt64; cdecl;
  external libNetwork name _PU + 'nw_data_transfer_report_get_sent_application_byte_count';

function nw_data_transfer_report_copy_path_interface(report: nw_data_transfer_report_t; path_index: UInt32): nw_interface_t; cdecl;
  external libNetwork name _PU + 'nw_data_transfer_report_copy_path_interface';

function nw_ethernet_channel_create(ether_type: UInt16; &interface: nw_interface_t): nw_ethernet_channel_t; cdecl;
  external libNetwork name _PU + 'nw_ethernet_channel_create';

procedure nw_ethernet_channel_set_state_changed_handler(ethernet_channel: nw_ethernet_channel_t;
  handler: nw_ethernet_channel_state_changed_handler_t); cdecl;
  external libNetwork name _PU + 'nw_ethernet_channel_set_state_changed_handler';

procedure nw_ethernet_channel_set_queue(ethernet_channel: nw_ethernet_channel_t; queue: dispatch_queue_t); cdecl;
  external libNetwork name _PU + 'nw_ethernet_channel_set_queue';

procedure nw_ethernet_channel_start(ethernet_channel: nw_ethernet_channel_t); cdecl;
  external libNetwork name _PU + 'nw_ethernet_channel_start';

procedure nw_ethernet_channel_cancel(ethernet_channel: nw_ethernet_channel_t); cdecl;
  external libNetwork name _PU + 'nw_ethernet_channel_cancel';

procedure nw_ethernet_channel_set_receive_handler(ethernet_channel: nw_ethernet_channel_t; handler: nw_ethernet_channel_receive_handler_t); cdecl;
  external libNetwork name _PU + 'nw_ethernet_channel_set_receive_handler';

procedure nw_ethernet_channel_send(ethernet_channel: nw_ethernet_channel_t; content: dispatch_data_t; vlan_tag: UInt16; remote_address: PByte;
  completion: nw_ethernet_channel_send_completion_t); cdecl;
  external libNetwork name _PU + 'nw_ethernet_channel_send';

function nw_framer_protocol_create_message(definition: nw_protocol_definition_t): nw_framer_message_t; cdecl;
  external libNetwork name _PU + 'nw_framer_protocol_create_message';

function nw_protocol_metadata_is_framer_message(metadata: nw_protocol_metadata_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_protocol_metadata_is_framer_message';

function nw_framer_message_create(framer: nw_framer_t): nw_framer_message_t; cdecl;
  external libNetwork name _PU + 'nw_framer_message_create';

procedure nw_framer_message_set_value(message: nw_framer_message_t; key: PAnsiChar; value: Pointer;
  dispose_value: nw_framer_message_dispose_value_t); cdecl;
  external libNetwork name _PU + 'nw_framer_message_set_value';

type
  nw_framer_message_access_value_access_value = function(value: Pointer): Boolean; cdecl;

function nw_framer_message_access_value(message: nw_framer_message_t; key: PAnsiChar; access_value:
  nw_framer_message_access_value_access_value): Boolean; cdecl;
  external libNetwork name _PU + 'nw_framer_message_access_value';

procedure nw_framer_message_set_object_value(message: nw_framer_message_t; key: PAnsiChar; value: Pointer); cdecl;
  external libNetwork name _PU + 'nw_framer_message_set_object_value';

function nw_framer_message_copy_object_value(message: nw_framer_message_t; key: PAnsiChar): Pointer; cdecl;
  external libNetwork name _PU + 'nw_framer_message_copy_object_value';

function nw_framer_create_definition(identifier: PAnsiChar; flags: UInt32; start_handler: nw_framer_start_handler_t): nw_protocol_definition_t; cdecl;
  external libNetwork name _PU + 'nw_framer_create_definition';

function nw_framer_create_options(framer_definition: nw_protocol_definition_t): nw_protocol_options_t; cdecl;
  external libNetwork name _PU + 'nw_framer_create_options';

procedure nw_framer_set_input_handler(framer: nw_framer_t; input_handler: nw_framer_input_handler_t); cdecl;
  external libNetwork name _PU + 'nw_framer_set_input_handler';

procedure nw_framer_set_output_handler(framer: nw_framer_t; output_handler: nw_framer_output_handler_t); cdecl;
  external libNetwork name _PU + 'nw_framer_set_output_handler';

procedure nw_framer_set_wakeup_handler(framer: nw_framer_t; wakeup_handler: nw_framer_wakeup_handler_t); cdecl;
  external libNetwork name _PU + 'nw_framer_set_wakeup_handler';

procedure nw_framer_set_stop_handler(framer: nw_framer_t; stop_handler: nw_framer_stop_handler_t); cdecl;
  external libNetwork name _PU + 'nw_framer_set_stop_handler';

procedure nw_framer_set_cleanup_handler(framer: nw_framer_t; cleanup_handler: nw_framer_cleanup_handler_t); cdecl;
  external libNetwork name _PU + 'nw_framer_set_cleanup_handler';

procedure nw_framer_mark_ready(framer: nw_framer_t); cdecl;
  external libNetwork name _PU + 'nw_framer_mark_ready';

function nw_framer_prepend_application_protocol(framer: nw_framer_t; protocol_options: nw_protocol_options_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_framer_prepend_application_protocol';

procedure nw_framer_mark_failed_with_error(framer: nw_framer_t; error_code: Integer); cdecl;
  external libNetwork name _PU + 'nw_framer_mark_failed_with_error';

function nw_framer_parse_input(framer: nw_framer_t; minimum_incomplete_length: NativeUInt; maximum_length: NativeUInt; temp_buffer: PUInt8;
  parse: nw_framer_parse_completion_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_framer_parse_input';

procedure nw_framer_deliver_input(framer: nw_framer_t; input_buffer: PUInt8; input_length: NativeUInt; message: nw_framer_message_t;
  is_complete: Boolean); cdecl;
  external libNetwork name _PU + 'nw_framer_deliver_input';

function nw_framer_deliver_input_no_copy(framer: nw_framer_t; input_length: NativeUInt; message: nw_framer_message_t;
  is_complete: Boolean): Boolean; cdecl;
  external libNetwork name _PU + 'nw_framer_deliver_input_no_copy';

procedure nw_framer_pass_through_input(framer: nw_framer_t); cdecl;
  external libNetwork name _PU + 'nw_framer_pass_through_input';

function nw_framer_parse_output(framer: nw_framer_t; minimum_incomplete_length: NativeUInt; maximum_length: NativeUInt; temp_buffer: PUInt8;
  parse: nw_framer_parse_completion_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_framer_parse_output';

procedure nw_framer_write_output(framer: nw_framer_t; output_buffer: PUInt8; output_length: NativeUInt); cdecl;
  external libNetwork name _PU + 'nw_framer_write_output';

procedure nw_framer_write_output_data(framer: nw_framer_t; output_data: dispatch_data_t); cdecl;
  external libNetwork name _PU + 'nw_framer_write_output_data';

function nw_framer_write_output_no_copy(framer: nw_framer_t; output_length: NativeUInt): Boolean; cdecl;
  external libNetwork name _PU + 'nw_framer_write_output_no_copy';

procedure nw_framer_pass_through_output(framer: nw_framer_t); cdecl;
  external libNetwork name _PU + 'nw_framer_pass_through_output';

procedure nw_framer_schedule_wakeup(framer: nw_framer_t; milliseconds: UInt64); cdecl;
  external libNetwork name _PU + 'nw_framer_schedule_wakeup';

procedure nw_framer_async(framer: nw_framer_t; async_block: nw_framer_block_t); cdecl;
  external libNetwork name _PU + 'nw_framer_async';

function nw_framer_copy_remote_endpoint(framer: nw_framer_t): nw_endpoint_t; cdecl;
  external libNetwork name _PU + 'nw_framer_copy_remote_endpoint';

function nw_framer_copy_local_endpoint(framer: nw_framer_t): nw_endpoint_t; cdecl;
  external libNetwork name _PU + 'nw_framer_copy_local_endpoint';

function nw_framer_copy_parameters(framer: nw_framer_t): nw_parameters_t; cdecl;
  external libNetwork name _PU + 'nw_framer_copy_parameters';

function nw_protocol_copy_tcp_definition: nw_protocol_definition_t; cdecl;
  external libNetwork name _PU + 'nw_protocol_copy_tcp_definition';

function nw_tcp_create_options: nw_protocol_options_t; cdecl;
  external libNetwork name _PU + 'nw_tcp_create_options';

procedure nw_tcp_options_set_no_delay(options: nw_protocol_options_t; no_delay: Boolean); cdecl;
  external libNetwork name _PU + 'nw_tcp_options_set_no_delay';

procedure nw_tcp_options_set_no_push(options: nw_protocol_options_t; no_push: Boolean); cdecl;
  external libNetwork name _PU + 'nw_tcp_options_set_no_push';

procedure nw_tcp_options_set_no_options(options: nw_protocol_options_t; no_options: Boolean); cdecl;
  external libNetwork name _PU + 'nw_tcp_options_set_no_options';

procedure nw_tcp_options_set_enable_keepalive(options: nw_protocol_options_t; enable_keepalive: Boolean); cdecl;
  external libNetwork name _PU + 'nw_tcp_options_set_enable_keepalive';

procedure nw_tcp_options_set_keepalive_count(options: nw_protocol_options_t; keepalive_count: UInt32); cdecl;
  external libNetwork name _PU + 'nw_tcp_options_set_keepalive_count';

procedure nw_tcp_options_set_keepalive_idle_time(options: nw_protocol_options_t; keepalive_idle_time: UInt32); cdecl;
  external libNetwork name _PU + 'nw_tcp_options_set_keepalive_idle_time';

procedure nw_tcp_options_set_keepalive_interval(options: nw_protocol_options_t; keepalive_interval: UInt32); cdecl;
  external libNetwork name _PU + 'nw_tcp_options_set_keepalive_interval';

procedure nw_tcp_options_set_maximum_segment_size(options: nw_protocol_options_t; maximum_segment_size: UInt32); cdecl;
  external libNetwork name _PU + 'nw_tcp_options_set_maximum_segment_size';

procedure nw_tcp_options_set_connection_timeout(options: nw_protocol_options_t; connection_timeout: UInt32); cdecl;
  external libNetwork name _PU + 'nw_tcp_options_set_connection_timeout';

procedure nw_tcp_options_set_persist_timeout(options: nw_protocol_options_t; persist_timeout: UInt32); cdecl;
  external libNetwork name _PU + 'nw_tcp_options_set_persist_timeout';

procedure nw_tcp_options_set_retransmit_connection_drop_time(options: nw_protocol_options_t; retransmit_connection_drop_time: UInt32); cdecl;
  external libNetwork name _PU + 'nw_tcp_options_set_retransmit_connection_drop_time';

procedure nw_tcp_options_set_retransmit_fin_drop(options: nw_protocol_options_t; retransmit_fin_drop: Boolean); cdecl;
  external libNetwork name _PU + 'nw_tcp_options_set_retransmit_fin_drop';

procedure nw_tcp_options_set_disable_ack_stretching(options: nw_protocol_options_t; disable_ack_stretching: Boolean); cdecl;
  external libNetwork name _PU + 'nw_tcp_options_set_disable_ack_stretching';

procedure nw_tcp_options_set_enable_fast_open(options: nw_protocol_options_t; enable_fast_open: Boolean); cdecl;
  external libNetwork name _PU + 'nw_tcp_options_set_enable_fast_open';

procedure nw_tcp_options_set_disable_ecn(options: nw_protocol_options_t; disable_ecn: Boolean); cdecl;
  external libNetwork name _PU + 'nw_tcp_options_set_disable_ecn';

function nw_protocol_metadata_is_tcp(metadata: nw_protocol_metadata_t): Boolean; cdecl;
  external libNetwork name _PU + 'nw_protocol_metadata_is_tcp';

function nw_tcp_get_available_receive_buffer(metadata: nw_protocol_metadata_t): UInt32; cdecl;
  external libNetwork name _PU + 'nw_tcp_get_available_receive_buffer';

function nw_tcp_get_available_send_buffer(metadata: nw_protocol_metadata_t): UInt32; cdecl;
  external libNetwork name _PU + 'nw_tcp_get_available_send_buffer';

implementation

uses
  Posix.Dlfcn,
  iOSapi.Foundation;

var
  NetworkModule: THandle;

function _nw_privacy_context_default_context: nw_privacy_context_t;
begin
  Result := PPnw_privacy_context(CocoaPointerConst(libNetwork, '_nw_privacy_context_default_context'))^;
end;

function kNWErrorDomainPOSIX: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libNetwork, 'kNWErrorDomainPOSIX'));
end;

function kNWErrorDomainDNS: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libNetwork, 'kNWErrorDomainDNS'));
end;

function kNWErrorDomainTLS: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libNetwork, 'kNWErrorDomainTLS'));
end;

function _nw_content_context_default_message: nw_content_context_t;
begin
  Result := PPnw_content_context(CocoaPointerConst(libNetwork, '_nw_content_context_default_message'))^;
end;

function _nw_content_context_final_send: nw_content_context_t;
begin
  Result := PPnw_content_context(CocoaPointerConst(libNetwork, '_nw_content_context_final_send'))^;
end;

function _nw_content_context_default_stream: nw_content_context_t;
begin
  Result := PPnw_content_context(CocoaPointerConst(libNetwork, '_nw_content_context_default_stream'))^;
end;

initialization
  NetworkModule := dlopen(MarshaledAString(libNetwork), RTLD_LAZY);

finalization
  dlclose(NetworkModule)

end.