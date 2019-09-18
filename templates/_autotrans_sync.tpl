{# Arguments: form_id and id #}

{% javascript %}
    pubzub.subscribe('~site/rsc/{{ id }}/autotrans', function(topic, msg) {
        var targetLanguage = msg.payload.language;
        var translations = msg.payload.translations;

        $("#{{ form_id }} input[name='language'][value='"+targetLanguage+"']").prop("checked",true);
        $("#{{ form_id }} .tab-"+targetLanguage).show();

        for (var p in translations) {
            if (translations.hasOwnProperty(p)) {
                var name = p + "$" + targetLanguage;
                var $elt = $("#{{ form_id }} [name='"+name+"']");
                if ($elt.length > 0) {
                    $elt.val(translations[p]);
                    if ($elt.hasClass('z_editor-installed')) {
                        tinymce.get($elt.attr('id')).setContent(translations[p]);
                    }
                }
            }
        }

        z_growl_add("{_ Automatically updated translations to: _}" + targetLanguage);
    });
{% endjavascript %}
