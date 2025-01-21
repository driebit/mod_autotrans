{# Arguments: form_id and id #}

{% javascript %}
    pubzub.subscribe('~site/rsc/{{ id }}/autotrans', function(topic, msg) {
        var targetLanguage = msg.payload.language;
        var translations = msg.payload.translations;

        $("#{{ form_id }} input[name='language'][value='"+targetLanguage+"']").prop("checked",true);
        $("#{{ form_id }} .tab-"+targetLanguage).show();

        console.log(translations);

        for (var p in translations) {
            if (translations.hasOwnProperty(p)) {
                var $elt;

                if (p.startsWith("blocks$")) {
                    var split = p.split("$");
                    var block_name = split[1];
                    var block_prop = split[2];

                    var $nameinput = $("#{{ form_id }} input.block-name")
                        .filter(function() { return $(this).val() == block_name; });
                    if ($nameinput.length > 0) {
                        var bid = $nameinput.attr('id').replace(/-name$/, '')
                                + '-' + block_prop
                                + '--' + targetLanguage;
                        $elt = $('#' + bid);
                    }
                } else {
                    var name = p + "$" + targetLanguage;
                    $elt = $("#{{ form_id }} [name='"+name+"']");
                }
                if ($elt.length > 0) {
                    $elt.val(translations[p]);
                    if ($elt.hasClass('z_editor-installed')) {
                        tinymce.get($elt.attr('id')).setContent(translations[p]);
                    }
                }
            }
        }

        z_growl_add("{_ Automatically updated translations to: _}<b>" + targetLanguage + "</b>");
    });
{% endjavascript %}
