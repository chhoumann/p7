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

    [Params(10, 20)] public int _numberOfRequests;

    [Benchmark]
    [BenchmarkCategory("Sending without fetching results")]
    public void PostAndWaitForResponseReceived()
    {
        Task[] taskList = _codeRunnerQueueClient.PostCodeRequest(CorrectCode, IncorrectTest, _numberOfRequests);
        Task.WaitAll(taskList);
    }

    [Benchmark]
    [BenchmarkCategory("Sending and then fetch results - 5 second pull time")]
    public void PostAndWaitForAllResultsFetched_5SecondsBetweenPolls()
    {
        TimeSpan pullTime = new(0, 0, 0, 5);
        var taskList = BuildTaskList(pullTime);
        Task.WaitAll(taskList);
    }

    [Benchmark]
    [BenchmarkCategory("Sending and then fetch results - 2 second pull time")]
    public void PostAndWaitForAllResultsFetched_2SecondsBetweenPolls()
    {
        TimeSpan pullTime = new(0, 0, 0, 2);
        var taskList = BuildTaskList(pullTime);
        Task.WaitAll(taskList);
    }

    [Benchmark]
    [BenchmarkCategory("Sending and then fetch results - 1 second pull time")]
    public void PostAndWaitForAllResultsFetched_1SecondsBetweenPolls()
    {
        TimeSpan pullTime = new(0, 0, 0, 1);
        var taskList = BuildTaskList(pullTime);
        Task.WaitAll(taskList);
    }

    [Benchmark]
    [BenchmarkCategory("Sending and then fetch results - 0.5 second pull time")]
    public void PostAndWaitForAllResultsFetched_HalfSecondsBetweenPolls()
    {
        TimeSpan pullTime = new(0, 0, 0, 0, 500);
        var taskList = BuildTaskList(pullTime);
        Task.WaitAll(taskList);
    }


    private async Task<IEnumerable<Task<Task<TestRunResult>>>> BuildTaskList(TimeSpan pullTime)
    {
        var clientActions = new List<Func<Task<TestRunResult>>>(_numberOfRequests);
        
        for (int i = 0; i < _numberOfRequests; i++)
        {
            CodeRunnerQueueClient codeRunnerQueueClient = new();
            
            clientActions[i] = codeRunnerQueueClient.CreatePostAndGetHaskellResult(CorrectCode, IncorrectTest, pullTime);
        }

        return clientActions.Select(a => Task.FromResult(a.Invoke())) as Task<Task<TestRunResult>>[] ?? throw new InvalidOperationException();
    }
}