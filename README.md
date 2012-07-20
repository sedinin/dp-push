Server sends push notification to iOS devices with Apple Push Notification Service.

NOTE: Don't forget to replace my **cert/ck.pem** with yours :)

NOTE: device token is hex number but not string :)

There are invalid tokens:

      DeviceToken = "9253de12 f71d300d 05a11135 e09e09b6 32c478d5 32313723 1f04a7c7 b4de947d"
      DeviceToken = "9253de12f71d300d05a11135e09e09b632c478d5323137231f04a7c7b4de947d"
      DeviceToken = <<"9253de12f71d300d05a11135e09e09b632c478d5323137231f04a7c7b4de947d">>


This is valid token:

     DeviceToken = 16#9253de12f71d300d05a11135e09e09b632c478d5323137231f04a7c7b4de947d

