if type kubectl >/dev/null 2>&1; then
    # fzf
    function list_k8s_contexts {
        LBUFFER="${LBUFFER}$(kubectl config get-contexts -o name | fzf --height=50%)";
        zle reset-prompt;
    }
    zle -N list_k8s_contexts
    bindkey '^xkc' list_k8s_contexts

    function list_k8s_pods {
        LBUFFER="${LBUFFER}$(kubectl get pods -o name | sed -E "s/pod\///g" | fzf --height=50%)";
        zle reset-prompt;
    }
    zle -N list_k8s_pods
    bindkey '^xkp' list_k8s_pods

    source <(kubectl completion zsh)

    function kezf {
        pod=$(kubectl get pods -oname | sed -E 's/pod\///g' | fzf);
        kubectl exec -it $(echo $@ | sed -E "s/@/${service}/");
    }
fi
