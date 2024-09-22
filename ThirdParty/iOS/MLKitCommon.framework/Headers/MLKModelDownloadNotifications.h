#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

/**
 * `Notification` name for observing model download tasks that succeed. The user info dictionary
 * will contain `{ModelDownloadUserInfoKey.remoteModel : RemoteModel}`.
 */
extern NSNotificationName const MLKModelDownloadDidSucceedNotification
    NS_SWIFT_NAME(mlkitModelDownloadDidSucceed);

/**
 * `Notification` name for observing model download tasks that fail. The user info dictionary will
 * contain `{ModelDownloadUserInfoKey.remoteModel : RemoteModel}` and
 * `{ModelDownloadUserInfoKey.error : NSError}`.
 */
extern NSNotificationName const MLKModelDownloadDidFailNotification
    NS_SWIFT_NAME(mlkitModelDownloadDidFail);

/**
 * The type used for retrieving information from the `Notification` user info dictionary for remote
 * model downloading.
 */
typedef NSString *MLKModelDownloadUserInfoKey NS_TYPED_ENUM NS_SWIFT_NAME(ModelDownloadUserInfoKey);

/** The key for retrieving the `RemoteModel` from the user info dictionary. */
extern MLKModelDownloadUserInfoKey const MLKModelDownloadUserInfoKeyRemoteModel;

/**
 * The key for retrieving the `NSError` from the user info dictionary. The corresponding value is
 * `nil` if the model download completed successfully.
 */
extern MLKModelDownloadUserInfoKey const MLKModelDownloadUserInfoKeyError;

NS_ASSUME_NONNULL_END
