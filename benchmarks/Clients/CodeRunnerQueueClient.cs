using System.Reflection;
using System.Text.Json;
using CodeRunnerClients.DataTransfer;

namespace CodeRunnerClients;

public class CodeRunnerQueueClient : CodeRunnerClient
{
    private static readonly Uri _getResultUrl = CreateUri("haskell/getResult/");

    private Task<HttpResponseMessage> GetByToken(string pollToken)
    {
        return _client.GetAsync($"{_getResultUrl}{pollToken}");
    }

    private async Task<TestRunResult?> GetTestRunResult(string id)
    {
        return await JsonSerializer.DeserializeAsync<TestRunResult>(await GetByToken(id).Result.Content.ReadAsStreamAsync());
    }

    private async Task<TestRunResult> PullUntilResponseReady(TimeSpan timeBetweenPulls, string id)
    {
        PeriodicTimer periodicTimer = new(timeBetweenPulls);
        
        while (await periodicTimer.WaitForNextTickAsync())
        {
            TestRunResult? codeRunResult = await GetTestRunResult(id);

            if (codeRunResult?.success != null)
            {
                return codeRunResult;
            }
        }

        throw new TimeoutException("Did not get a response from server.");
    }

    public Task<TestRunResult> PostAndGetHaskellResultTask(string code, string test, TimeSpan timeBetweenPulls)
    {
        Task<PullIdResponse?> postCodeRequest = PostCodeRequest(code, test);
        
        PullIdResponse? tokenResponse = postCodeRequest.Result;
        string id = tokenResponse?.id is null
            ? throw new InvalidOperationException("Token string was null!")
            : tokenResponse.id;

        return PullUntilResponseReady(timeBetweenPulls, id);
    }
}

internal class EnvironmentNotSetException : Exception
{
    public EnvironmentNotSetException(string ip) : base($" \n \n The environment did not contain variable {ip}." +
                                                       $" Did you include it in the .env file for the docker compose?" +
                                                       $" Did you also set it in the Dockerfile for the project depending on {Assembly.GetCallingAssembly().FullName}? \n \n")
    {

    }
}