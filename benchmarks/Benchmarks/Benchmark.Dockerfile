# Must be run in docker-compose git repo.
FROM mcr.microsoft.com/dotnet/sdk:6.0 AS base

ARG CLIENT_PORT
ARG CLIENT_TARGET


WORKDIR /src
COPY ./benchmarks/. ./
ENV PORT=$CLIENT_PORT HOST=$CLIENT_TARGET
RUN dotnet restore "Benchmarks/Benchmarks.csproj"
RUN dotnet build "Benchmarks/Benchmarks.csproj" -c Release -o /src/bin
RUN dotnet publish "Benchmarks/Benchmarks.csproj" -c Release -o /src/bin/publish

WORKDIR /src/bin/publish
ENTRYPOINT ["dotnet", "Benchmarks.dll"]


