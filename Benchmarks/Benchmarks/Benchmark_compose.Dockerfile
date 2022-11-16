# Must be run in docker-compose git repo.
FROM mcr.microsoft.com/dotnet/sdk:6.0 AS base
WORKDIR /src
COPY ./benchmarks/. ./
RUN dotnet restore "Benchmarks/Benchmarks.csproj"
RUN dotnet build "Benchmarks/Benchmarks.csproj" -c Release -o /src/bin
RUN dotnet publish "Benchmarks/Benchmarks.csproj" -c Release -o /src/bin/publish
RUN dotnet test

WORKDIR /src/bin/publish
ENTRYPOINT ["dotnet", "Benchmarks.dll"]


