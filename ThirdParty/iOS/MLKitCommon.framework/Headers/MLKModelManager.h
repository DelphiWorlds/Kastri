#import <Foundation/Foundation.h>

@class MLKModelDownloadConditions;
@class MLKRemoteModel;

NS_ASSUME_NONNULL_BEGIN

/** Manages models that are used by MLKit features. */
NS_SWIFT_NAME(ModelManager)
@interface MLKModelManager : NSObject

/**
 * Returns a `ModelManager` instance.
 *
 * @return A `ModelManager` instance.
 */
+ (instancetype)modelManager NS_SWIFT_NAME(modelManager());

/** Unavailable. Use the `modelManager()` class method. */
- (instancetype)init NS_UNAVAILABLE;

/**
 * Checks whether the given model has been downloaded.
 *
 * @param remoteModel The model to check the download status for.
 * @return Whether the given model has been downloaded.
 */
- (BOOL)isModelDownloaded:(MLKRemoteModel *)remoteModel;

/**
 * Downloads the given model from the server to a local directory on the device. Use
 * `isModelDownloaded(_:)` to check the download status for the model. If this method is invoked and
 * the model has already been downloaded, a request is made to check if a newer version of the model
 * is available for download. If available, the new version of the model is downloaded.
 *
 * @discussion To know when this method is done, observe the `.mlkitModelDownloadDidSucceed` and
 *       `.mlkitModelDownloadDidFail` notifications defined in `MLKModelDownloadNotifications.h`.
 *       If the latest model is already downloaded, completes without additional work and posts
 *       `.mlkitModelDownloadDidSucceed` notification, indicating that the model is ready to use.
 *
 * @param remoteModel The model to download.
 * @param conditions The conditions for downloading the model.
 * @return Progress for downloading the model.
 */
- (NSProgress *)downloadModel:(MLKRemoteModel *)remoteModel
                   conditions:(MLKModelDownloadConditions *)conditions
    NS_SWIFT_NAME(download(_:conditions:));

/**
 * Deletes the downloaded model from the device.
 *
 * @param remoteModel The downloaded model to delete.
 * @param completion Handler to call back on the main queue when the model deletion completed
 *     successfully or failed with the given `error`.
 */
- (void)deleteDownloadedModel:(MLKRemoteModel *)remoteModel
                   completion:(void (^)(NSError *_Nullable error))completion;

@end

NS_ASSUME_NONNULL_END
