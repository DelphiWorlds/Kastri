unit DW.Consts.Android;

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

const
  cPermissionAccessBackgroundLocation = 'android.permission.ACCESS_BACKGROUND_LOCATION';
  cPermissionAccessCoarseLocation = 'android.permission.ACCESS_COARSE_LOCATION';
  cPermissionAccessFineLocation = 'android.permission.ACCESS_FINE_LOCATION';
  cPermissionBluetooth = 'android.permission.BLUETOOTH';
  cPermissionCamera = 'android.permission.CAMERA';
  cPermissionReadCallLog = 'android.permission.READ_CALL_LOG';
  cPermissionReadContacts = 'android.permission.READ_CONTACTS';
  cPermissionReadExternalStorage = 'android.permission.READ_EXTERNAL_STORAGE';
  cPermissionReadPhoneState = 'android.permission.READ_PHONE_STATE';
  cPermissionReadSMS = 'android.permission.READ_SMS';
  cPermissionReceiveMMS = 'android.permission.RECEIVE_MMS';
  cPermissionReceiveSMS = 'android.permission.RECEIVE_SMS';
  cPermissionReceiveWAPPush = 'android.permission.RECEIVE_WAP_PUSH';
  cPermissionRecordAudio = 'android.permission.RECORD_AUDIO';
  cPermissionSendSMS = 'android.permission.SEND_SMS';
  cPermissionUseFingerprint = 'android.permission.USE_FINGERPRINT';
  cPermissionWriteContacts = 'android.permission.WRITE_CONTACTS';
  cPermissionWriteExternalStorage = 'android.permission.WRITE_EXTERNAL_STORAGE';

  cMetadataFCMDefaultChannelId = 'com.google.firebase.messaging.default_notification_channel_id';
  cMetadataFCMDefaultIcon = 'com.google.firebase.messaging.default_notification_icon';

  cLocationPriorityBalancedPowerAccuracy = 102;
  cLocationPriorityHighAccuracy = 100;
  cLocationPriorityLowPower = 104;
  cLocationPriorityNoPower = 105;

  cLocationBroadcastAction = 'com.delphiworlds.kastri.LOCATION_BROADCAST_ACTION';
  cLocationBroadcastExtraLatitude = 'Latitude';
  cLocationBroadcastExtraLongitude = 'Longitude';
  cLocationBroadcastExtraAltitude = 'Altitude';
  cLocationBroadcastExtraBearing = 'Bearing';
  cLocationBroadcastExtraFlags = 'Flags';
  cLocationBroadcastExtraSpeed = 'Speed';
  cLocationBroadcastExtraAccuracy = 'Accuracy';
  cLocationBroadcastExtraSource = 'Source';

  cDWBroadcastReceiverName = 'com.delphiworlds.kastri.DWMultiBroadcastReceiver';
  cDWBroadcastReceiverActionAlarmTimer = cDWBroadcastReceiverName + '.ACTION_ALARM_TIMER';
  cDWBroadcastReceiverActionStartAlarm = cDWBroadcastReceiverName + '.ACTION_START_ALARM';
  cDWBroadcastReceiverActionServiceAlarm = cDWBroadcastReceiverName + '.ACTION_SERVICE_ALARM';
  cDWBroadcastReceiverActionServiceRestart = cDWBroadcastReceiverName + '.ACTION_SERVICE_RESTART';
  cDWBroadcastReceiverExtraJobId = cDWBroadcastReceiverName + '.EXTRA_JOB_ID';
  cDWBroadcastReceiverExtraServiceRestart = cDWBroadcastReceiverName + '.EXTRA_SERVICE_RESTART';
  cDWBroadcastReceiverExtraServiceClassName = cDWBroadcastReceiverName + '.EXTRA_SERVICE_CLASS_NAME';
  cDWBroadcastReceiverExtraStartUnlock = cDWBroadcastReceiverName + '.EXTRA_START_UNLOCK';

  cDWFusedLocationClientName = 'com.delphiworlds.kastri.DWFusedLocationClient';
  cDWFusedLocationClientKeyServiceClassName = cDWFusedLocationClientName + '.KEY_SERVICE_CLASS_NAME';
  cDWFusedLocationClientKeyJobId = cDWFusedLocationClientName + '.KEY_JOB_ID';
  cDWFusedLocationClientActionAlarm = cDWFusedLocationClientName + '.ACTION_ALARM';
  cDWFusedLocationClientExtraAlarmInterval = cDWFusedLocationClientName + '.EXTRA_ALARM_INTERVAL';
  cDWFusedLocationClientExtraAlarmTimestamp = cDWFusedLocationClientName + '.EXTRA_ALARM_TIMESTAMP';

  cServiceCommandAction = 'com.delphiworlds.action.SERVICE_COMMAND';
  cServiceMessageAction = 'com.delphiworlds.action.SERVICE_MESSAGE';
  cServiceStateAction = 'com.delphiworlds.action.SERVICE_STATE';
  cServiceBroadcastParamCommand = 'COMMAND';
  cServiceBroadcastParamMessage = 'MESSAGE';
  cServiceBroadcastParamState = 'STATE';
  cServiceCommandAppBecameActive = 1;
  cServiceCommandAppEnteredBackground = 2;
  cServiceCommandAppWillTerminate = 3;
  cServiceCommandAppIsRequestingPermissions = 4;
  cServiceCommandAppCheckState = 5;

  cEMBTJavaServicePrefix = 'com.embarcadero.services.';

implementation

end.
