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
        var timeToFail = Task.Delay(TimeSpan.FromSeconds(30));
        Task<TestRunResult> testRunResultTask = await _client.PostAndGetHaskellResultTask("", "",
            new TimeSpan(0, 0, 0, 0, 500));
        if (await Task.WhenAny(testRunResultTask, timeToFail) == testRunResultTask)
        {
            TestRunResult response = await testRunResultTask;
            Assert.False(string.IsNullOrWhiteSpace(response.output));
        }
        else
        {
            throw new TimeoutException();
        }
    }
    
    [Fact]
    public Task CanTestMultipleResultsOneAtATime()
    {
        int numberOfRequests = 5;
        Task<Task<TestRunResult>>[] clientActions = new Task<Task<TestRunResult>> [50];

        for (int i = 0; i < clientActions.Length; i++)
        {
            CodeRunnerQueueClient codeRunnerQueueClient = new();
            clientActions[0] = codeRunnerQueueClient.CreatePostAndGetHaskellResult(
                "", "", TimeSpan.FromSeconds(3)).Invoke();
        }



        Task.WaitAll(clientActions);
        return Task.CompletedTask;
    }
    [Fact]
    public Task CanTestMultipleResultsMoreAtATime()
    {
        const int numberOfRequests = 10;
        Func<Task<Task<TestRunResult>>>[] clientActions = new Func<Task<Task<TestRunResult>>> [numberOfRequests];

        for (int i = 0; i < clientActions.Length; i++)
        {
            CodeRunnerQueueClient codeRunnerQueueClient = new();
            clientActions[i] = codeRunnerQueueClient.CreatePostAndGetHaskellResult(
                "", "", TimeSpan.FromSeconds(3));
        }

        var res = new Task[numberOfRequests];
        for (var index = 0; index < clientActions.Length; index++)
        {
            var clientAction = clientActions[index];
            res[index] = (clientAction.Invoke());
        }

        Task.WaitAll(res);
        return Task.CompletedTask;
    }
}