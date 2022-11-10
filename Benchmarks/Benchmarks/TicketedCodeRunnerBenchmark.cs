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
        Console.WriteLine("Running benchmark PostAndWaitForResponseReceived.");
        
        Task<HttpResponseMessage>[] tasks = _codeRunnerQueueClient.PostCodeRequest(CorrectCode, IncorrectTest, NumberOfRequests);
        
        Task.WhenAll(tasks).Wait();
    }

    [Benchmark]
    [BenchmarkCategory("Sending and then fetch results - 5 second pull time")]
    public void PostAndWaitForAllResultsFetched_5SecondsBetweenPolls()
    {
        Console.WriteLine("Running benchmark PostAndWaitForAllResultsFetched_5SecondsBetweenPolls.");
        
        TimeSpan pullTime = TimeSpan.FromSeconds(5);
        IEnumerable<Task> taskList = BuildTaskList(pullTime);
        
        Task.WhenAll(taskList);
    }

    [Benchmark]
    [BenchmarkCategory("Sending and then fetch results - 2 second pull time")]
    public void PostAndWaitForAllResultsFetched_2SecondsBetweenPolls()
    {
        Console.WriteLine("Running benchmark PostAndWaitForAllResultsFetched_2SecondsBetweenPolls.");
        
        TimeSpan pullTime = TimeSpan.FromSeconds(2);
        IEnumerable<Task> taskList = BuildTaskList(pullTime);
        
        Task.WhenAll(taskList);
    }

    [Benchmark]
    [BenchmarkCategory("Sending and then fetch results - 1 second pull time")]
    public void PostAndWaitForAllResultsFetched_1SecondsBetweenPolls()
    {
        Console.WriteLine("Running benchmark PostAndWaitForAllResultsFetched_1SecondsBetweenPolls.");
        
        TimeSpan pullTime = TimeSpan.FromSeconds(1);
        IEnumerable<Task> taskList = BuildTaskList(pullTime);
        
        Task.WhenAll(taskList);
    }

    [Benchmark]
    [BenchmarkCategory("Sending and then fetch results - 0.5 second pull time")]
    public void PostAndWaitForAllResultsFetched_HalfSecondsBetweenPolls()
    {
        Console.WriteLine("Running benchmark PostAndWaitForAllResultsFetched_HalfSecondsBetweenPolls.");
        
        TimeSpan pullTime = TimeSpan.FromMilliseconds(500);
        IEnumerable<Task> clientActions = BuildTaskList(pullTime);

        Task.WhenAll(clientActions).Wait();
    }

    private IEnumerable<Task> BuildTaskList(TimeSpan pullTime)
    {
        List<Action> clientActions = new(NumberOfRequests);
        List<Task> tasks = new(NumberOfRequests);
        CodeRunnerQueueClient codeRunnerQueueClient = new();
            
        for (int i = 0; i < NumberOfRequests; i++)
        {
            clientActions.Add(() => codeRunnerQueueClient.PostAndGetHaskellResultTask(CorrectCode, IncorrectTest, pullTime));
        }
        
        foreach (Action clientAction in clientActions)
        {
            tasks.Add(Task.Run(clientAction));
        }

        return tasks;
    }
}