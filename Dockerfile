FROM mcr.microsoft.com/dotnet/core/aspnet:3.1-focal
COPY /deploy /
EXPOSE 8085
ENTRYPOINT [ "dotnet", "Server.dll" ]