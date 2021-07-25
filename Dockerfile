FROM mcr.microsoft.com/dotnet/aspnet:5.0
COPY /deploy /
EXPOSE 8085
ENTRYPOINT [ "dotnet", "Server.dll" ]