FROM mcr.microsoft.com/dotnet/core/sdk:3.1-alpine3.12
COPY /deploy /
EXPOSE 8085
ENTRYPOINT [ "dotnet", "Server.dll" ]