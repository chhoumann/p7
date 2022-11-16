# Must be run in docker-compose git repo.



FROM mcr.microsoft.com/dotnet/sdk:6.0
#Get arguments on build time

ARG CLIENT_PORT
ARG CLIENT_TARGET
 
COPY ./benchmarks/CodeRunnerClientTest/. ./CodeRunnerClientTest
COPY ./benchmarks/Clients/. ./Clients/


#Set environment for the process on build time
ENV PORT=$CLIENT_PORT HOST=$CLIENT_TARGET TEST=ONEOOOOOO
RUN echo "Setting up tests with client sending to $CLIENT_TARGET $CLIENT_PORT"
RUN dotnet restore "./CodeRunnerClientTest/CodeRunnerClientTest.csproj"
RUN dotnet restore "./Clients/Clients.csproj"
CMD ["dotnet", "test", "./CodeRunnerClientTest/CodeRunnerClientTest.csproj"]