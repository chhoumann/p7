FROM mcr.microsoft.com/dotnet/runtime:6.0 AS base
WORKDIR /app

FROM mcr.microsoft.com/dotnet/sdk:6.0 AS build
WORKDIR /src
COPY ["CodeRunnerBenchmarks/CodeRunnerBenchmarks.csproj", "CodeRunnerBenchmarks/"]
COPY ["CodeRunnerClientTest/CodeRunnerClientTest.csproj", "CodeRunnerClientTest/"]
RUN dotnet restore "CodeRunnerBenchmarks/CodeRunnerBenchmarks.csproj"
RUN dotnet restore "CodeRunnerClientTest/CodeRunnerClientTest.csproj"


COPY . .
RUN dotnet test
WORKDIR "/src/CodeRunnerBenchmarks"
RUN dotnet build "CodeRunnerBenchmarks.csproj" -c Release -o /app/build

FROM build AS publish
RUN dotnet publish "CodeRunnerBenchmarks.csproj" -c Release -o /app/publish

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "CodeRunnerBenchmarks.dll"]
