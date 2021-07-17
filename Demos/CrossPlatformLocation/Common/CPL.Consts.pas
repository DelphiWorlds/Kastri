unit CPL.Consts;

interface

const
  cServiceName = 'CrossPlatformLocationService';

  cServiceCommandStartLocationUpdates = 100;
  cServiceCommandStopLocationUpdates = 101;
  cServiceCommandCheckState = 102;
  cServiceStateLocationUpdatesUnavailable = 200;
  cServiceStateLocationUpdatesResumed = 201;
  cServiceStateLocationUpdatesPaused = 202;

  cCloudLoggingHost = ''; // Change this value to wherever your cloud logging host is
  cCloudLoggingName = 'CrossPlatformLocation';

implementation

end.
