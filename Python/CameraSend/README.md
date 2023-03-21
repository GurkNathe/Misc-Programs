# CameraSend

This is a small python script that enables record from your camera and detect a person. Then send a notification about said recording to your phone.

### Credits

The majority of this code is from [Tech with Tim](https://www.youtube.com/watch?v=Exic9E5rNok) and [Alfredo Sequeida](https://www.youtube.com/watch?v=4-ysecoraKo). The camera detection is from Tech with Tim and the message sending is from Alfredo Sequeida.

### Set up:

- config.env file
- Configure parameters for detect_person function if defaults aren't preferred

### config.env format:

```
NUMBER="number-to-send-to"
PROVIDER="provider-name"
PROVIDER_SCHEMA="method-for-sending"
EMAIL="email-to-send-from"
PASSWORD="app-password-for-email"
```

The providers listed in the providers.py are what should be entered for the PROVIDER field (spelling matters). You can easily add your own providers to said file as well.

The PROVIDER_SCHEMA designates which server to use as designated by the PROVIDERS dictionary in providers.py. The same rules apply here as the PROVIDER field.

Depending on the mail service used, you may need to set up an additional password for it. For Gmail, you can navigate to [myaccount.google.com/apppasswords](myaccount.google.com/apppasswords) and create a new app password.
