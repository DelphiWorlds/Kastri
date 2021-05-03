#import <CoreGraphics/CoreGraphics.h>
#import <Foundation/Foundation.h>


#import <MLKitVision/MLKVisionPoint.h>

NS_ASSUME_NONNULL_BEGIN

/**
 * A three-dimensional point in an image. The point's coordinates have the same scale as the
 * original image.
 */
NS_SWIFT_NAME(Vision3DPoint)
@interface MLKVision3DPoint : MLKVisionPoint

/** The z-coordinate of the point. */
@property(nonatomic, readonly) CGFloat z;

/** Unavailable. */
- (instancetype)init NS_UNAVAILABLE;

@end

NS_ASSUME_NONNULL_END
