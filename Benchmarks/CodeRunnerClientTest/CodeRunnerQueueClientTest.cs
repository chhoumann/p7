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
        var timeBetweenPulls = new TimeSpan(0, 0, 0, 0, 500);
        var testRunResultTask = await _client.PostAndGetHaskellResultTask("", "", timeBetweenPulls);
        
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
            clientActions[i] = codeRunnerQueueClient.CreatePostAndGetHaskellResult("", "", timeBetweenPulls).Invoke();
        }

        Task.WaitAll(clientActions);
        return Task.CompletedTask;
    }
    [Fact]
    public Task CanTestMultipleResultsMoreAtATime()
    {
        const int numberOfRequests = 10;
        Func<Task<TestRunResult>>[] clientActions = new Func<Task<TestRunResult>> [numberOfRequests];
        TimeSpan timeBetweenPulls = TimeSpan.FromSeconds(3);
        
        for (int i = 0; i < clientActions.Length; i++)
        {
            CodeRunnerQueueClient codeRunnerQueueClient = new();
            clientActions[i] = codeRunnerQueueClient.CreatePostAndGetHaskellResult("", "", timeBetweenPulls);
        }

        Task[] res = new Task[numberOfRequests];
        
        for (int index = 0; index < clientActions.Length; index++)
        {
            var clientAction = clientActions[index];
            res[index] = (clientAction.Invoke());
        }

        Task.WaitAll(res);
        return Task.CompletedTask;
    }
}