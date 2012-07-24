# Erlang library for working with APNs

from [dieselpuppet.com](http://dieselpuppet.com/).


Server sends push notification to iOS devices with Apple Push Notification Service.

[Apple Push Notification Service documentation](http://developer.apple.com/library/mac/#documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/ApplePushService/ApplePushService.html#//apple_ref/doc/uid/TP40008194-CH100-SW9)

[Great tutorial about using APNs](http://www.raywenderlich.com/3443/apple-push-notification-services-tutorial-part-12)


NOTE: Don't forget to replace my **cert/ck.pem** with yours :)

NOTE: device token is hex number but not string :)

There are invalid tokens:

      DeviceToken = "8253de13 f71d310d 05a13135 e09e09b6 32c478d5 32313723 1f04a7c7 b5de947d"
      DeviceToken = "8253de13f71d310d05a13135e09e09b632c478d5323137231f04a7c7b5de947d"
      DeviceToken = <<"8253de13f71d310d05a13135e09e09b632c478d5323137231f04a7c7b5de947d">>


This is valid token:

     DeviceToken = 16#8253de13f71d310d05a13135e09e09b632c478d5323137231f04a7c7b5de947d

