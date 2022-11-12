
FROM mcr.microsoft.com/dotnet/sdk:6.0 AS base
WORKDIR /src
COPY . .
RUN dotnet restore "Benchmarks/Benchmarks.csproj"
RUN dotnet build "Benchmarks/Benchmarks.csproj" -c Release -o /src/bin
RUN dotnet publish "Benchmarks/Benchmarks.csproj" -c Release -o /src/bin/publish

WORKDIR /src/bin/publish
ENTRYPOINT ["dotnet", "Benchmarks.dll"]


