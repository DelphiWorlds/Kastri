unit DW.Androidapi.JNI.AndroidX.Camera.Lifecycle;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

// (androidx-)camera-lifecycle-x.x.x.jar

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes,
  // DW
  DW.Androidapi.JNI.AndroidX.Camera, DW.Androidapi.JNI.Concurrent, DW.Androidapi.JNI.AndroidX.Lifecycle, DW.Androidapi.JNI.Util,
  DW.Androidapi.JNI.Guava;

type
  JLifecycleCamera = interface;
  JLifecycleCameraProvider = interface;
  JLifecycleCameraRepository = interface;
  JProcessCameraProvider = interface;

  JLifecycleCameraClass = interface(JLifecycleObserverClass)
    ['{33507CDC-5D6B-41BC-90C2-1EB532EBA333}']
  end;

  [JavaSignature('androidx/camera/lifecycle/LifecycleCamera')]
  JLifecycleCamera = interface(JLifecycleObserver)
    ['{49F3CFAC-8AEF-4D3F-B081-798C4C0C3C06}']
    procedure bind(collection: JCollection); cdecl;
    function getCameraControl: JCameraControl; cdecl;
    function getCameraInfo: JCameraInfo; cdecl;
    function getCameraInternals: JLinkedHashSet; cdecl;
    function getCameraUseCaseAdapter: JCameraUseCaseAdapter; cdecl;
    function getExtendedConfig: JCameraConfig; cdecl;
    function getLifecycleOwner: JLifecycleOwner; cdecl;
    function getUseCases: JList; cdecl;
    function isActive: Boolean; cdecl;
    function isBound(useCase: JUseCase): Boolean; cdecl;
    procedure onDestroy(lifecycleOwner: JLifecycleOwner); cdecl;
    procedure onStart(lifecycleOwner: JLifecycleOwner); cdecl;
    procedure onStop(lifecycleOwner: JLifecycleOwner); cdecl;
    procedure release; cdecl;
    procedure setExtendedConfig(cameraConfig: JCameraConfig); cdecl;
    procedure suspend; cdecl;
    procedure unbind(collection: JCollection); cdecl;
    procedure unsuspend; cdecl;
  end;
  TJLifecycleCamera = class(TJavaGenericImport<JLifecycleCameraClass, JLifecycleCamera>) end;

  JLifecycleCameraProviderClass = interface(IJavaClass)
    ['{54CD7697-A991-46E7-A089-05410410D8D3}']
  end;

  [JavaSignature('androidx/camera/lifecycle/LifecycleCameraProvider')]
  JLifecycleCameraProvider = interface(IJavaInstance)
    ['{9A9FF3A6-A4DB-43AD-8CC5-5260530907D1}']
    function hasCamera(cameraSelector: JCameraSelector): Boolean; cdecl;
    function isBound(useCase: JUseCase): Boolean; cdecl;
    procedure unbindAll; cdecl;
  end;
  TJLifecycleCameraProvider = class(TJavaGenericImport<JLifecycleCameraProviderClass, JLifecycleCameraProvider>) end;

  JLifecycleCameraRepositoryClass = interface(JObjectClass)
    ['{1BF0D9A8-2B71-41AA-B5AE-ECEEBAFAD508}']
  end;

  [JavaSignature('androidx/camera/lifecycle/LifecycleCameraRepository')]
  JLifecycleCameraRepository = interface(JObject)
    ['{32F43142-17F8-4967-BCE4-5E3058BF1A10}']
    procedure bindToLifecycleCamera(lifecycleCamera: JLifecycleCamera; viewPort: JViewPort; collection: JCollection); cdecl;
    procedure clear; cdecl;
    function getLifecycleCamera(lifecycleOwner: JLifecycleOwner; cameraId: JCameraUseCaseAdapter_CameraId): JLifecycleCamera; cdecl;
    function getLifecycleCameras: JCollection; cdecl;
    procedure setInactive(lifecycleOwner: JLifecycleOwner); cdecl;
    procedure unbind(collection: JCollection); cdecl;
    procedure unbindAll; cdecl;
  end;
  TJLifecycleCameraRepository = class(TJavaGenericImport<JLifecycleCameraRepositoryClass, JLifecycleCameraRepository>) end;

  JProcessCameraProviderClass = interface(JLifecycleCameraProviderClass)
    ['{A784CB28-B83B-4EA0-AB5D-AF956944C94C}']
    {class} procedure configureInstance(cameraXConfig: JCameraXConfig); cdecl;
    {class} function getInstance(context: JContext): JListenableFuture; cdecl;
  end;

  [JavaSignature('androidx/camera/lifecycle/ProcessCameraProvider')]
  JProcessCameraProvider = interface(JLifecycleCameraProvider)
    ['{63A92EA8-893E-45F3-929D-C87F892E1B8A}']
    function bindToLifecycle(lifecycleOwner: JLifecycleOwner; cameraSelector: JCameraSelector; useCaseGroup: JUseCaseGroup): Jcore_Camera; cdecl;
    function hasCamera(cameraSelector: JCameraSelector): Boolean; cdecl;
    function isBound(useCase: JUseCase): Boolean; cdecl;
    function shutdown: JListenableFuture; cdecl;
    procedure unbindAll; cdecl;
  end;
  TJProcessCameraProvider = class(TJavaGenericImport<JProcessCameraProviderClass, JProcessCameraProvider>) end;

implementation

end.
