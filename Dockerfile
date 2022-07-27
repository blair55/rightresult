FROM mcr.microsoft.com/dotnet/aspnet:6.0
COPY /deploy /
EXPOSE 8085
ENTRYPOINT [ "dotnet", "Server.dll" ]