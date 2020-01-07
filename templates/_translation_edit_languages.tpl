{% overrules%}

{% block language_options %}

{% inherit %}

<div class="form-group">
    {% button
        text=_"Auto translate this page"
        class="btn btn-default"
        postback={auto_translate id=id}
        delegate="mod_autotrans"
    %}
</div>

{% endblock %}
