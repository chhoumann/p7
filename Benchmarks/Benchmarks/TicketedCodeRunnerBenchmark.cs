using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Configs;
using BenchmarkDotNet.Order;

namespace Benchmarks;

[HtmlExporter]
[Orderer(SummaryOrderPolicy.FastestToSlowest)]
[GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByMethod)]
public class TicketedCodeRunnerBenchmark
{
    private const string CorrectCode = "module Code where\n\nadd x y = x - y";
    private const string IncorrectTest = "a";
    
    private readonly CodeRunnerQueueClient _codeRunnerQueueClient = new();

    [Params(10, 20)]
    public int NumberOfRequests { get; set; }

    [Benchmark]
    [BenchmarkCategory("Sending without fetching results")]
    public void PostAndWaitForResponseReceived()
    {
        Task<HttpResponseMessage>[] taskList = _codeRunnerQueueClient.PostCodeRequest(CorrectCode, IncorrectTest, NumberOfRequests);
        
        Task.WaitAll(taskList);
    }

    [Benchmark]
    [BenchmarkCategory("Sending and then fetch results - 5 second pull time")]
    public void PostAndWaitForAllResultsFetched_5SecondsBetweenPolls()
    {
        TimeSpan pullTime = TimeSpan.FromSeconds(5);
        Task<TestRunResult>[] taskList = BuildTaskList(pullTime);
        
        Task.WaitAll(taskList);
    }

    [Benchmark]
    [BenchmarkCategory("Sending and then fetch results - 2 second pull time")]
    public void PostAndWaitForAllResultsFetched_2SecondsBetweenPolls()
    {
        TimeSpan pullTime = TimeSpan.FromSeconds(2);
        Task<TestRunResult>[] taskList = BuildTaskList(pullTime);
        
        Task.WaitAll(taskList);
    }

    [Benchmark]
    [BenchmarkCategory("Sending and then fetch results - 1 second pull time")]
    public void PostAndWaitForAllResultsFetched_1SecondsBetweenPolls()
    {
        TimeSpan pullTime = TimeSpan.FromSeconds(1);
        Task<TestRunResult>[] taskList = BuildTaskList(pullTime);
        
        Task.WaitAll(taskList);
    }

    [Benchmark]
    [BenchmarkCategory("Sending and then fetch results - 0.5 second pull time")]
    public void PostAndWaitForAllResultsFetched_HalfSecondsBetweenPolls()
    {
        TimeSpan pullTime = TimeSpan.FromMilliseconds(500);
        Task<TestRunResult>[] taskList = BuildTaskList(pullTime);
        
        Task.WaitAll(taskList);
    }

    private Task<TestRunResult>[] BuildTaskList(TimeSpan pullTime)
    {
        Task<TestRunResult>[] clientActions = new Task<TestRunResult>[NumberOfRequests];
        
        for (int i = 0; i < NumberOfRequests; i++)
        {
            CodeRunnerQueueClient codeRunnerQueueClient = new();
            
            clientActions[i] = codeRunnerQueueClient.PostAndGetHaskellResultTask(CorrectCode, IncorrectTest, pullTime);
        }

        return clientActions;
    }
}