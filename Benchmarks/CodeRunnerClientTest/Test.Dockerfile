FROM mcr.microsoft.com/dotnet/runtime:6.0 AS base
WORKDIR /app

FROM mcr.microsoft.com/dotnet/sdk:6.0 AS build
WORKDIR /src
COPY ["CodeRunnerClientTest/CodeRunnerClientTest.csproj", "CodeRunnerClientTest/"]
RUN dotnet restore "CodeRunnerClientTest/CodeRunnerClientTest.csproj"

COPY . .
RUN dotnet test
