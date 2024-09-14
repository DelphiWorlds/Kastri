//
//  NotificationService.m
//  service
//
//  Created by Dave Nottage on 9/12/2023.
//

#import "NotificationService.h"

@interface NotificationService ()

@property (nonatomic, strong) void (^contentHandler)(UNNotificationContent *contentToDeliver);
@property (nonatomic, strong) UNMutableNotificationContent *content;

@end

@implementation NotificationService

- (void)downloadAndAttachImageFromURL:(NSURL *)url withContent:(UNMutableNotificationContent *)content contentHandler:(void (^)(UNNotificationContent *))contentHandler {
    NSURLSession *session = [NSURLSession sharedSession];
    NSURLSessionDownloadTask *downloadTask = [session downloadTaskWithURL:url
        completionHandler:^(NSURL *location, NSURLResponse *response, NSError *error) {
            if (location) {
                NSString *temporaryPath = [location.path stringByAppendingString:@".jpg"];
                NSError *fileError;
                [[NSFileManager defaultManager] moveItemAtPath:location.path toPath:temporaryPath error:&fileError];
                
                if (!fileError) {
                    UNNotificationAttachment *attachment = [UNNotificationAttachment attachmentWithIdentifier:@"image"
                                                                                                        URL:[NSURL fileURLWithPath:temporaryPath]
                                                                                                    options:nil
                                                                                                      error:nil];
                    if (attachment) {
                        content.attachments = @[attachment];
                    }
                }
            }
            contentHandler(content);
        }];
    [downloadTask resume];
}

- (void)didReceiveNotificationRequest:(UNNotificationRequest *)request withContentHandler:(void (^)(UNNotificationContent * _Nonnull))contentHandler {
    NSLog(@"DEBUG: NotificationService - didReceiveNotificationRequest");
    self.contentHandler = contentHandler;
    self.content = [request.content mutableCopy];
    
    NSString *imageUrl = request.content.userInfo[@"imageUrl"];
    if (imageUrl != nil && ![imageUrl isEqualToString:@""]) {
        NSLog(@"DEBUG: imageUrl: %@", imageUrl);
        [self downloadAndAttachImageFromURL:[NSURL URLWithString:imageUrl] withContent:self.content contentHandler:contentHandler];
        return;
    }
    
    self.contentHandler(self.content);
}

- (void)serviceExtensionTimeWillExpire {
    // Called just before the extension will be terminated by the system.
    // Use this as an opportunity to deliver your "best attempt" at modified content, otherwise the original push payload will be used.
    self.contentHandler(self.content);
}

@end
