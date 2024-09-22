#import <CoreMedia/CoreMedia.h>
#import <UIKit/UIKit.h>



#import "MLKCompatibleImage.h"

NS_ASSUME_NONNULL_BEGIN

/** An image or image buffer used for vision detection. */
NS_SWIFT_NAME(VisionImage)

@interface MLKVisionImage : NSObject <MLKCompatibleImage>

/** The display orientation of the image. The default is `.up`. */
@property(nonatomic) UIImageOrientation orientation;

/**
 * Initializes a `VisionImage` object with the given image.
 *
 * @param image Image to use in vision detection. The given image should be rotated, so its
 *      `imageOrientation` property is set to `.up`. The `UIImage` must have non-NULL `CGImage`
 *      property.
 * @return A `VisionImage` instance with the given image.
 */
- (instancetype)initWithImage:(UIImage *)image NS_DESIGNATED_INITIALIZER;

/**
 * Initializes a `VisionImage` object with the given image buffer. To improve performance, it is
 * recommended to minimize the lifespan and number of instances of this class when initializing with
 * a `CMSampleBufferRef`.
 *
 * @param sampleBuffer Image buffer to use in vision detection. The buffer must be based on
 *       a pixel buffer (not compressed data), and the pixel format must be one of:
 *         - `kCVPixelFormatType_32BGRA`
 *         - `kCVPixelFormatType_420YpCbCr8BiPlanarFullRange`
 *         - `kCVPixelFormatType_420YpCbCr8BiPlanarVideoRange`
 *       In practice: this works with the video output of the phone's camera, but not other
 *       arbitrary sources of `CMSampleBufferRef`s.
 * @return A `VisionImage` instance with the given image buffer.
 */
- (instancetype)initWithBuffer:(CMSampleBufferRef)sampleBuffer NS_DESIGNATED_INITIALIZER;

/** Unavailable. */
- (instancetype)init NS_UNAVAILABLE;

@end

NS_ASSUME_NONNULL_END
