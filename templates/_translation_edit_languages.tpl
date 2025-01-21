{% overrules %}

{% block language_options %}

{% inherit %}

<div class="form-group">

    {% if not m.config.mod_autotrans.google_api_secret.value
          and not m.config.mod_autotrans.microsoft_api_secret.value
    %}
        <p class="help-block"><span class="glyphicon glyphicon-info-sign"></span> {_ Automatic translation disabled. To enable, set the configuration values <tt>mod_autotrans.google_api_secret</tt> or <tt>mod_autotrans.microsoft_api_secret</tt> _}</p>
    {% elseif m.config.mod_autotrans.source_language and m.config.mod_autotrans.target_language %}
        {% if m.config.mod_autotrans.automatic.value %}
            <p class="help-block">
                <span class="glyphicon glyphicon-info-sign"></span>
                {_ Automatic translation from _}
                <b>{{ m.config.mod_autotrans.source_language.value|escape }}</b>
                {_ to _} <b>{{ m.config.mod_autotrans.target_language.value|escape }}</b>
                {_ is enabled. Save the page to translate. _}
            </p>
        {% else %}
            {% button
                text=_"Auto translate the saved texts"
                class="btn btn-default"
                postback={auto_translate id=id}
                delegate="mod_autotrans"
                action={alert
                    title=_"Automatic translation"
                    text=[
                        _"Automatically translate from ",
                        " <b>",
                        m.config.mod_autotrans.source_language.value|escape,
                        "</b> ",
                        _"to",
                        " <b>",
                        m.config.mod_autotrans.target_language.value|escape,
                        "</b> in the background.", "<br>",
                        _"Wait a bit, you will get a notification when the translation is done."
                    ]
                }
            %}

            <p class="help-block">
                {_ Translate the saved texts of this page from _}
                <b>{{ m.config.mod_autotrans.source_language.value|escape }}</b>
                {_ to _} <b>{{ m.config.mod_autotrans.target_language.value|escape }}</b>.
                <br><b>{_ Note: _}</b> {_ First save the page. _}
            </p>
        {% endif %}
    {% else %}
        <p class="help-block"><span class="glyphicon glyphicon-info-sign"></span> {_ Automatic translation disabled. To enable, set the configuration values <tt>mod_autotrans.source_language</tt> and <tt>mod_autotrans.target_language</tt> _}</p>
    {% endif %}
</div>

{% endblock %}
