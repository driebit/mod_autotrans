Automatic Translations for Zotonic Resources
============================================

When enabled (and configured) this module automatically translates edited
texts in a source language to a destination language.

It uses:

 * Microsoft Cognitive Text Translation Services [1]
 * Google Cloud Translate API [2]

How does it work?
-----------------

After a resource is updated this module queues a translation task into
the Zotonic task queue.

The translation task checks all texts in the source language for changes.

If a text was changed then it will request a translation at one of the
configured services and replaces the texts in the target language with the
newly translated text.

After the update a notification will be sent, which enabled the
admin edit form to update its fields with the new translation.

If this happens, you will see a growl message which also mentions the
target language.

Configuration
-------------

The following keys need to be configured:

  * `mod_autotrans.source_language` The two letter ISO code of the source language
  * `mod_autotrans.target_language` The two letter ISO code of the destination language

For Microsoft Cognitive Text Translation Services:

 * `mod_autotrans.microsoft_api_secret` One of the API keys from the Translation Services.
   Currently this module uses the generic endpoint, so no project endpoint needs to be configured.

For Google Translator:

 * `mod_autotrans.google_api_secret` The API Key from the Credentials page on your
   Google Cloud console at https://console.cloud.google.com/ The Key must have access
   to the Cloud Translation API

Notes
-----

Please note that this module sends your texts to Microsoft and/or Google.

If you are editing sensitive information then it might not be a good idea to use this service.

Also, if you allow user-entered content, then you should add to your Privacy Declaration the use
of these services.

References
----------

 - [1] https://docs.microsoft.com/en-us/azure/cognitive-services/translator/reference/v3-0-translate
 - [2] https://cloud.google.com/translate/docs/quickstarts

