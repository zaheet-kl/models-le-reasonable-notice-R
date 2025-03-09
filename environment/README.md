# Azure ML Environment Register Setup
az ml environment create --name reasonable-notice-env \
                         --resource-group $AML_RG_NAME \
                         --workspace-name $AML_WS_NAME \
                         --build-context ./environment \
                         --dockerfile-path ./Dockerfile
                         