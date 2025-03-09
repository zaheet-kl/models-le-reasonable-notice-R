import argparse
from azureml.core import Workspace, Model
from azureml.core.webservice import AciWebservice, Webservice

def deploy_model(resource_group, workspace_name, model_name, endpoint_name, deployment_config):
    # Load the workspace
    ws = Workspace.get(name=workspace_name, resource_group=resource_group)

    # Load the registered model
    model = Model(ws, name=model_name)

    # Load the deployment configuration
    aci_config = AciWebservice.deploy_configuration(
        cpu_cores=deployment_config["cpu_cores"],
        memory_gb=deployment_config["memory_gb"],
        auth_enabled=deployment_config["auth_enabled"]
    )

    # Deploy the model
    service = Model.deploy(ws, endpoint_name, [model], aci_config, overwrite=True)
    service.wait_for_deployment(show_output=True)

    print(f"Service state: {service.state}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--resource-group", type=str, required=True)
    parser.add_argument("--workspace-name", type=str, required=True)
    parser.add_argument("--model-name", type=str, required=True)
    parser.add_argument("--endpoint-name", type=str, required=True)
    parser.add_argument("--deployment-config", type=str, required=True)
    args = parser.parse_args()

    # Load deployment config
    import yaml
    with open(args.deployment_config, "r") as f:
        deployment_config = yaml.safe_load(f)

    deploy_model(
        args.resource_group,
        args.workspace_name,
        args.model_name,
        args.endpoint_name,
        deployment_config
    )