(require 'pcomplete)

(defconst pcmpl-kubectl-commands
  '((create
     clusterrole clusterrolebinding configmap cronjob deployment ingress
     job namespace poddisruptionbudget priorityclass quota role rolebinding
     (secret docker-registry generic tls)
     (service clusterip externalname loadbalancer nodeport)
     serviceaccount token)
    (get pods pod po services service svc deployments deploy ingress ing replicationcontroller namespace ns
         roles clusterroles rolebindings clusterrolebindings)
    (run nil)
    (expose pod service rc rs deployment)
    (delete pod service deployment namespace)
    (apply edit-last-applied set-last-applied view-last-applied)
    (annotate pods)
    (autoscale deployment rc)
    (debug nil) (diff nil) (edit nil) (kustomize nil)
    (label pods)
    (patch node pod deployment)
    (replace nil)
    (rollout history pause restart resume status undo)
    (scale nil)
    (set env image resources selector serviceaccount subject)
    (wait nil) (attach nil)
    (auth can-i reconcile)
    (cp nil)
    (describe nodes pods pod service)
    (events nil) (exec nil) (logs nil) (port-forward nil) (proxy nil)
    (top node pod)
    (api-versions nil) (api-resources nil)
    (certificate approve deny)
    (cluster-info dump)
    (cordon nil) (drain nil)
    (taint node nodes)
    (uncordon nil)
    (alpha (auth whoami))
    (completion bash zsh fish powershell)
    (config current-context delete-cluster delete-context delete-user get-clusters
            get-contexts get-users rename-context set set-cluster set-context
            set-credentials unset use-context view)
    (explain pods)
    (options nil)
    (plugin list)
    (version nil))
  "List of `kubectl' commands.")

(defconst pcmpl-kubectl-arguments
  '((create
     allow-missing-template-keys=true dry-run=none edit=false field-manager=kubectl-create 
     filename= kustomize output=json raw= record=false recursive=false save-config=false
     selector= show-managed-fields=false template= validate=strict windows-line-endings=false)
    (create->clusterrole ;;TODO
     aggregation-rule= allow-missing-template-keys=true dry-run=none field-manager=kubectl-create
     non-resource-url= output=json resource= resource-name= save-config=false show-managed-fields=false
     template= validate=strict verb=)
    (create->clusterrolebinding ;;TODO
     allow-missing-template-keys=true clusterrole= dry-run=none field-manager=kubectl-create group=
     output=json save-config=false serviceaccount= show-managed-fields=false template= user=
     validate=strict)
    (create->configmap ;;TODO
     allow-missing-template-keys=true append-hash=false dry-run=none field-manager=kubectl-create
     from-env-file= from-file= from-literal= output=json save-config=false show-managed-fields=false
     template= validate=strict)
    (create->cronjob ;;TODO
     allow-missing-template-keys=true dry-run=none field-manager=kubectl-create image= output=json
     restart= save-config=false schedule= show-managed-fields=false template= validate=strict)
    (create->deployment ;;TODO
     allow-missing-template-keys=true dry-run=none field-manager=kubectl-create image= output=json
     port=-1 replicas=1 save-config=false show-managed-fields=false template= validate=strict)
    (create->ingress ;;TODO
     allow-missing-template-keys=true annotation= class= default-backend= dry-run=none
     field-manager=kubectl-create output=json rule= save-config=false show-managed-fields=false
     template= validate=strict)
    (create->job ;;TODO
     allow-missing-template-keys=true dry-run=none field-manager=kubectl-create from= image=
     output=json save-config=false show-managed-fields=false template= validate=strict)
    (create->namespace ;;TODO
     allow-missing-template-keys=true dry-run=none field-manager=kubectl-create output=json
     save-config=false show-managed-fields=false template= validate=strict)
    (create->poddisruptionbudget ;;TODO
     allow-missing-template-keys=true dry-run=none field-manager=kubectl-create max-unavailable=
     min-available= output=json save-config=false selector= show-managed-fields=false template=
     validate=strict)
    (create->priorityclass ;;TODO
     allow-missing-template-keys=true description= dry-run=none field-manager=kubectl-create
     global-default=false output=json preemption-policy=PreemptLowerPriority save-config=false
     show-managed-fields=false template= validate=strict value=0)
    (create->quota ;;TODO
     allow-missing-template-keys=true dry-run=none field-manager=kubectl-create hard= output=json
     save-config=false scopes= show-managed-fields=false template= validate=strict)
    (create->role ;;TODO
     allow-missing-template-keys=true dry-run=none field-manager=kubectl-create output=json
     resource= resource-name= save-config=false show-managed-fields=false template= validate=strict
     verb=)
    (create->rolebinding ;;TODO
     allow-missing-template-keys=true dry-run=none field-manager=kubectl-create group= output=json
     role= save-config=false serviceaccount= show-managed-fields=false template= user= validate=strict)
    (create->secret->docker-registry ;;TODO
     allow-missing-template-keys=true append-hash=false docker-email= docker-password=
     "docker-server=https://index.docker.io/v1/" docker-username= dry-run=none field-manager=kubectl-create
     from-file= output=json save-config=false show-managed-fields=false template= validate=)
    (create->secret->generic ;;TODO
     allow-missing-template-keys=true append-hash=false dry-run=none field-manager=kubectl-create
     from-env-file= from-file= from-literal= output=json save-config=false show-managed-fields=false
     template= validate=strict)
    (create->secret->tls ;;TODO
     allow-missing-template-keys=true append-hash=false cert= dry-run=none field-manager=kubectl-create
     key= output=json save-config=false show-managed-fields=false template= validate=strict)
    (create->service->clusterip ;;TODO
     allow-missing-template-keys=true clusterip= dry-run=none field-manager=kubectl-create output=json
     save-config=false show-managed-fields=false tcp= template= validate=strict)
    (create->service->externalname ;;TODO
     allow-missing-template-keys=true dry-run=none external-name= field-manager=kubectl-create
     output=json save-config=false show-managed-fields=false tcp= template= validate=strict)
    (create->service->loadbalancer ;;TODO
     allow-missing-template-keys=true dry-run=none field-manager=kubectl-create output=json
     save-config=false show-managed-fields=false tcp= template= validate=strict)
    (create->service->nodeport ;;TODO
     allow-missing-template-keys=true dry-run=none field-manager=kubectl-create node-port=
     output=json save-config=false show-managed-fields=false tcp= template= validate=strict)
    (create->serviceaccount ;;TODO
     allow-missing-template-keys=true dry-run=none field-manager=kubectl-create output=json
     save-config=true show-managed-fields=false template= validate=strict)
    (create->token ;;TODO
     allow-missing-template-keys=true audience= bound-object-kind= bound-object-name= bound-object-uid=
     duration=0s output=json show-managed-fields=false template=)
    (get
     all-namespaces=false allow-missing-template-keys=true chunk-size=500 field-selector filename=
     ignore-not-found=false kustomize label-columns no-headers=false output=json output-watch-events=false
     raw recursive=false selector= server-print=true show-kind=false show-labels=false
     show-managed-fields=false sort-by= subresource=status template= use-openapi-print-columns=false
     watch=false watch-only=false)
    (run
     allow-missing-template-keys=true annotations= attach=false cascade=background command=false
     dry-run=none env= expose=false field-manager=kubectl-run filename= force=false grace-period=-1
     image= image-pull-policy= kustomize labels= leave-stdin-open=false output=json override-type=merge
     overrides= pod-running-timeout=1m0s port= privileged=false quiet=false record=false recursive=false
     restart=Always rm=false save-config=false show-managed-fields=false stdin=false template=
     timeout=0s tty=false wait=false)
    (expose
     allow-missing-template-keys=true cluster-ip= dry-run=none external-ip= field-manager=kubectl-expose
     filename= kustomize labels= load-balancer-ip= name= output=json override-type=merge overrides=
     port= protocol=TCP record=false recursive=false save-config=false selector= show-managed-fields=false
     target-port= template= type=ClusterIP)
    (delete
     all=false all-namespaces=false cascade=background dry-run=none field-selector= filename= force=false
     grace-period=-1 ignore-not-found=false kustomize now=false output=json raw= recursive=false selector=
     timeout=0s wait=true)
    (apply
     all=false allow-missing-template-keys=true cascade=background dry-run=none
     field-manager=kubectl-client-side-apply filename= force=false force-conflicts=false grace-period=-1
     kustomize openapi-patch=true output= overwrite=false prune=false prune-allowlist= prune-whitelist=
     record=false recursive=false selector= server-side=false show-managed-fields=false template=
     timeout=0s validate=strict wait=false)
    (apply->edit-last-applied ;;TODO
     allow-missing-template-keys=true field-manager=kubectl-client-side-apply filename= kustomize
     output= record=false recursive=false show-managed-fields=false template= validate=strict
     windows-line-endings=false)
    (apply->set-last-applied ;;TODO
     allow-missing-template-keys=true create-annotation=false dry-run=none filename= output=json
     show-managed-fields=false template=)
    (apply->view-last-applied ;;TODO
     all=false filename= kustomize output=yaml recursive=false selector=)
    (annotate
     all=false allow-missing-template-keys=true dry-run=none field-manager=kubectl-annotate field-selector=
     filename= kustomize list=false local=false output=json overwrite=false record=false recursive=false
     resource-version= selector= show-managed-fields=false template=)
    (autoscale
     allow-missing-template-keys=false cpu-percent=-1 dry-run=none field-manager=kubectl-autoscale
     filename= kustomize max=-1 min=-1 name= output=json record=false recursive=false save-config=false
     show-managed-fields=false template=)
    (debug
     arguments-only=false attach=false container= copy-to= env= image= image-pull-policy= profile=legacy
     quiet=false replace=false same-node=false set-image= share-processes=true stdin=false
     target= tty=false)
    (diff
     field-manager=kubectl-client-side-apply filename= force-conflicts=false kustomize prune=false
     prune-allowlist= recursive=false selector= server-side=false how-managed-fields=false)
    (edit
     allow-missing-template-keys=true field-manager=kubectl-edit filename= kustomize output=json
     output-patch=false record=false recursive=false save-config=false show-managed-fields=false
     subresource= template= validate=strict windows-line-endings=false)
    (kustomize
     as-current-user=false enable-alpha-plugins=false enable-helm=false enable-managedby-label=false
     env= helm-command=helm load-restrictor=LoadRestrictionsRootOnly mount= network=false
     network-name=bridge output= reorder=legacy)
    (label
     all=false all-namespaces=false allow-missing-template-keys=true dry-run=none
     field-manager=kubectl-label field-selector= filename= kustomize list=false local=false
     output=json overwrite=false record=false recursive=false resource-version= selector=
     show-managed-fields=false template=)
    (patch
     allow-missing-template-keys=true dry-run=none field-manager=kubectl-patch filename=
     kustomize local=false output= patch= patch-file= record=false recursive=false
     show-managed-fields=false subresource= template= type=strategic)
    (replace
     allow-missing-template-keys=true cascade=background dry-run=none field-manager=kubectl-replace
     filename= force=false grace-period=-1 kustomize output= raw= recursive=false save-config=false
     show-managed-fields=false subresource= template= timeout=0s validate=strict wait=false)
    (rollout->history ;;TODO
     allow-missing-template-keys=true filename= kustomize output= recursive=false revision=0
     selector= show-managed-fields=false template=)
    (rollout->pause ;;TODO
     allow-missing-template-keys=true field-manager=kubectl-rollout filename= kustomize
     output=json recursive=false selector= show-managed-fields=false template=)
    (rollout->restart ;TODO
     allow-missing-template-keys=true field-manager=kubectl-rollout filename= kustomize
     output=json recursive=false selector= show-managed-fields=false template=)
    (rollout->resume ;;TODO
     allow-missing-template-keys=true field-manager=kubectl-rollout filename= kustomize
     output=json recursive=false selector= show-managed-fields=false template=)
    (rollout->status
     filename= kustomize recursive=false revision=0 selector= timeout=0s watch=true)
    (rollout->undo ;;TODO
     allow-missing-template-keys=true dry-run=none filename= kustomize output=json recursive=false
     selector= show-managed-fields=false template= to-revision=0)
    (scale
     all=false allow-missing-template-keys=true current-replicas=-1 dry-run=none filename=
     kustomize output= record=false recursive=false replicas=0 resource-version= selector=
     show-managed-fields=false template= timeout=0s)
    (set->env ;;TODO
     all=false allow-missing-template-keys=true containers= dry-run=none env= field-manager=kubectl-set
     filename= from= keys= kustomize list=false local=false output= overwrite=true prefix=
     recursive=false resolve=false selector= show-managed-fields=false template=)
    (set->image ;;TODO
     all=false allow-missing-template-keys=true dry-run=none field-manager=kubectl-set filename=
     kustomize local=false output=json record=false recursive=false selector= show-managed-fields=false
     template=)
    (set->resources ;;TODO
     all=false allow-missing-template-keys=true containers=* dry-run=none field-manager=kubectl-set
     filename= kustomize limits= local=false output=json record=false recursive=false requests=
     selector= show-managed-fields=false template=)
    (set->selector ;;TODO
     all=false allow-missing-template-keys=true dry-run=none field-manager=kubectl-set filename=
     local=false output=json record=false recursive=true resource-version= show-managed-fields=false
     template=)
    (set->serviceaccount ;;TODO
     all=false allow-missing-template-keys=true dry-run=none field-manager=kubectl-set filename=
     kustomize local output=json record=false recursive=false show-managed-fields=false template=)
    (set->subject ;;TODO
     all=false allow-missing-template-keys=true dry-run=none field-manager=kubectl-set filename=
     group= kustomize local=false output=json recursive=false selector= serviceaccount=
     show-managed-fields=false template= user)
    (wait
     all=false all-namespaces=false allow-missing-template-keys=true field-selector= filename=
     for= local=false output= recursive=true selector= show-managed-fields=false
     template= timeout=30s)
    (attach
     container= pod-running-timeout=1m0s quiet=false stdin=false tty=false)
    (auth->can-i ;;TODO
     all-namespaces=false list=false no-headers=false quiet=false subresource=)
    (auth->reconcile ;;TODO
     allow-missing-template-keys=true dry-run=none filename= kustomize output=json recursive=false
     remove-extra-permissions=false remove-extra-subjects=false show-managed-fields=false template=)
    (describe
     all-namespaces=false chunk-size=500 filename= kustomize recursive=false selector=
     show-events=true)
    (events
     all-namespaces=false allow-missing-template-keys=true chunk-size=500 for= no-headers=false
     output=json show-managed-fields=false template= types= watch=false)
    (exec
     container= filename= pod-running-timeout=1m0s quiet=false stdin=false tty=false)
    (logs
     all-containers=false container= follow=false ignore-errors=false insecure-skip-tls-verify-backend=false
     limit-bytes=0 max-log-requests=5 pod-running-timeout=20s prefix=false previous=false selector=
     since=0s since-time= tail=-1 timestamps=false)
    (port-forward
     address=localhost pod-running-timeout=1m0s)
    (proxy
     "accept-hosts=^localhost$,^127.0.0.1$,^[::1]$" "accept-paths=^.*" address=127.0.0.1
     api-prefix=/ append-server-path=false disable-filter=false keepalive=0s port=8001
     reject-methods=^$ "reject-paths=^/api/./pods/./exec,^/api/./pods/./attach"
     unix-socket= www= www-prefix=/static/)
    (top->node ;;TODO
     no-headers=false selector= show-capacity=false sort-by= use-protocol-buffers=true)
    (top->pod ;;TODO
     all-namespaces=false containers=false field-selector= no-headers=false selector= sort-by=
     sum=false use-protocol-buffers=true)
    (api-resources
     api-group= categories= namespaced=true no-headers=false output= sort-by= verbs=)
    (certificate->approve ;;TODO
     allow-missing-template-keys=true filename= force=false kustomize output=json recursive=false
     show-managed-fields=false template=)
    (certificate->deny ;;TODO
     allow-missing-template-keys=true filename= force=false kustomize output=json recursive=false
     show-managed-fields=false template=)
    (cluster-info->dump ;;TODO
     all-namespaces=false allow-missing-template-keys=true namespaces= output=json output-directory=
     pod-running-timeout=20s show-managed-fields=false template=)
    (cordon
     dry-run=none selector=)
    (drain
     chunk-size=500 delete-emptydir-data=false delete-local-data=false disable-eviction=false
     dry-run=none force=false grace-period=-1 ignore-daemonsets=false pod-selecto=
     selector= skip-wait-for-delete-timeout=0 timeout=0s)
    (taint
     all=false allow-missing-template-keys=true dry-run=none field-manager= kubectl-taint
     output=json overwrite=false selector= show-managed-fields=false template= validate=strict)
    (uncordon dry-run=none selector=)
    (alpha->auth->whoami ;;TODO
     allow-missing-template-keys=true output=json show-managed-fields=false template=)
    (config
     kubeconfig)
    (config->get-contexts ;;TODO
     no-headers=false output=json)
    (config->set
     set-raw-bytes=false)
    (config->set-cluster
     certificate-authority= embed-certs=false insecure-skip-tls-verify=false proxy-url=
     server= tls-server-name=)
    (config->set-context
     cluster= current=false namespace= user=)
    (config->set-credentials
     auth-provider= auth-provider-arg= client-certificate= client-key= embed-certs=false
     exec-api-version= exec-arg= exec-command= exec-env= password= token= username=)
    (config->view
     allow-missing-template-keys=true flatten=false merge=true minify=false output=yaml
     raw=false show-managed-fields=false template=)
    (explain
     api-version= recursive=false)
    (plugin->list ;;TODO
     name-only=false)
    (version
     client=false output=json short=false))
  "List of `kubectl' command arguments.")

;; (defun pcomplete--kubectl-args (args commands)
;;   ;;(message (format "args: %s, commands: %s" args commands))
;;   (cond ((and (consp args) (not (string-empty-p (car args))))
;;          (let* ((arg (car args))
;;                 (result (cdr (assoc (intern arg) commands))))
;;            ;;(message (format "result: %s" result))
;;            (if (consp result)
;;                (pcomplete--kubectl-args (cdr args) result)
;;              (pcomplete--kubectl-args '() commands))))

;;         (t
;;          (let ((r (mapcar #'(lambda (x) (if (consp x) (car x) x)) commands)))
;;            (unless (eq 'nop (car r))
;;              ;;(message (format "-->final result: %s" r))
;;              r))          
;;          )))

;; (defun pcomplete/kubectl ()
;;   "Completion for `kubectl'."
;;   ;; Completion for the command argument.

;;   (pcomplete-here* (mapcar #'(lambda (x) (car x)) pcmpl-kubectl-commands))
;;   (let ((res (pcomplete--kubectl-args (rest pcomplete-args) pcmpl-kubectl-commands)))
;;     (pcomplete-here res)
;;     ))

(defun pcomplete--kubectl-commands (args commands)
  (cond ((and (consp args) (not (string-empty-p (car args))))
         (let* ((arg (car args))
                (result (cdr (assoc (intern arg) commands))))
           (cond ((consp result)
                  (when (< pcomplete-index pcomplete-last)
                    (pcomplete-next-arg))
                  (pcomplete--kubectl-commands (cdr args) result))
                 (t (pcomplete--kubectl-commands '() commands)))))
        (t
         (let ((r (pcomplete--kubectl-flatten-commands commands)))
           (unless (eq 'nil (car r)) r))          
         )))

(defun pcomplete--kubectl-flatten-commands (commands)
  (mapcar #'(lambda (x) (if (consp x) (car x) x)) commands))

(defun pcomplete--kubectl-arguments (command)
  (mapcar (lambda (x) (format "--%s" x)) (cdr (assoc (intern command) pcmpl-kubectl-arguments))))

(defun pcomplete/kubectl ()
  "Completion for `kubectl'."
  ;; Completion for the command argument.

  (let ((prec (pcomplete-arg 'first 1)))
    (cond ((pcomplete-match "^-\\{2\\}" 'last)
           (while (pcomplete-here
                   (pcomplete--kubectl-arguments prec)))))

    (let ((args (pcomplete--kubectl-commands (rest pcomplete-args) pcmpl-kubectl-commands)))
      (pcomplete-here* args))))

(defun pcomplete/k ()
  "Alias for `kubectl`.''"
  (pcomplete/kubectl))
