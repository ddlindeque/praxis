# Use the [Choice] comment to indicate option arguments that should appear in VS Code UX
#
# [Choice] Haskell version: 9, 8
ARG VARIANT=8
FROM haskell:${VARIANT}

# Use the [Option] comment to specify true/false arguments that should appear in VS Code UX
#
# [Option] Install zsh
# ARG INSTALL_ZSH="true"
# [Option] Upgrade OS packages to their latest versions
# ARG UPGRADE_PACKAGES="false"

RUN groupadd --gid 1000 vscode
RUN useradd -s /bin/bash --uid 1000 --gid vscode -m vscode
RUN usermod -aG sudo vscode

RUN apt-get update && export DEBIAN_FRONTEND=noninteractive

# [Optional] Uncomment this section to install additional OS packages.
# RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
#     && apt-get -y install --no-install-recommends <your-package-list-here>
