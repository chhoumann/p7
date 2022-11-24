
using CodeRunnerClients;
using Client.DataTransfer;
using Xunit;

namespace CodeRunnerClientTest;

public class CodeRunnerQueueClientTest
{
    [Fact]
    public async Task CanPostRequest()
    {
        CodeRunnerQueueClient _client = new();
        PullIdResponse? response = await _client.PostCodeRequest("", "");
        Assert.False(string.IsNullOrWhiteSpace(response?.id));
    }

    [Fact]
    public async Task CanPostRequestAndFetchResultBefore30Sec()
    {
        CodeRunnerQueueClient _client = new();
        TestRunResult testRunResultTask = await _client.PostAndGetHaskellResultTask("", "", TimeSpan.FromMilliseconds(500));
        
        Assert.False(string.IsNullOrWhiteSpace(testRunResultTask.output));
    }
    
    [Fact]
    public Task CanTestMultipleResultsOneAtATime()
    {
        const int numberOfRequests = 10;
        
        Task<TestRunResult>[] clientActions = new Task<TestRunResult>[numberOfRequests];
        TimeSpan timeBetweenPulls = TimeSpan.FromSeconds(3);

        for (int i = 0; i < numberOfRequests; i++)
        {
            CodeRunnerQueueClient codeRunnerQueueClient = new();
            clientActions[i] = codeRunnerQueueClient.PostAndGetHaskellResultTask("", "", timeBetweenPulls);
        }

        Task.WhenAll(clientActions).Wait();
        
        return Task.CompletedTask;
    }
    
    [Fact]
    public Task CanTestMultipleResultsMoreAtATime()
    {
        TimeSpan timeBetweenPulls = TimeSpan.FromSeconds(3);

        IEnumerable<Task> tasks = TaskBuilder.BuildClientTaskList<CodeRunnerQueueClient>(10, client =>
        {
            client.PostAndGetHaskellResultTask("", "", timeBetweenPulls);
        });

        Task.WhenAll(tasks).Wait();
        
        return Task.CompletedTask;
    }
}