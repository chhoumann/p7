using Benchmarks;
using dotenv.net;
using Xunit;

namespace CodeRunnerClientTest;

public class CodeRunnerQueueClientTest
{
    private readonly CodeRunnerQueueClient _client = new();

    public CodeRunnerQueueClientTest()
    {
        DotEnv.Fluent()
            .WithExceptions()
            .WithEnvFiles()
            .Load();
    }

    [Fact]
    public async Task CanPostRequest()
    {
        var response = await _client.PostCodeRequest("", "");
        Assert.False(string.IsNullOrWhiteSpace(response?.id));
    }

    [Fact]
    public async Task CanPostRequestAndFetchResultBefore30Sec()
    {
        TestRunResult testRunResultTask = await _client.PostAndGetHaskellResultTask("", "", TimeSpan.FromMilliseconds(500));
        
        Assert.False(string.IsNullOrWhiteSpace(testRunResultTask.output));
    }
    
    [Fact]
    public Task CanTestMultipleResultsOneAtATime()
    {
        const int numberOfRequests = 50;
        
        Task<TestRunResult>[] clientActions = new Task<TestRunResult>[numberOfRequests];
        TimeSpan timeBetweenPulls = TimeSpan.FromSeconds(3);

        for (int i = 0; i < numberOfRequests; i++)
        {
            CodeRunnerQueueClient codeRunnerQueueClient = new();
            clientActions[i] = codeRunnerQueueClient.PostAndGetHaskellResultTask("", "", timeBetweenPulls);
        }

        Task.WhenAll(clientActions);
        
        return Task.CompletedTask;
    }
    [Fact]
    public Task CanTestMultipleResultsMoreAtATime()
    {
        const int numberOfRequests = 10;
        
        Task<TestRunResult>[] clientTasks = new Task<TestRunResult> [numberOfRequests];
        TimeSpan timeBetweenPulls = TimeSpan.FromSeconds(3);
        
        for (int i = 0; i < clientTasks.Length; i++)
        {
            CodeRunnerQueueClient codeRunnerQueueClient = new();
            clientTasks[i] = codeRunnerQueueClient.PostAndGetHaskellResultTask("", "", timeBetweenPulls);
        }

        Task.WhenAll(clientTasks);
        
        return Task.CompletedTask;
    }
}