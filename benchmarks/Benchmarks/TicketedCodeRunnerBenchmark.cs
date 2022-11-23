using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Configs;
using BenchmarkDotNet.Order;
using CodeRunnerClients;
using Client.DataTransfer;

namespace Benchmarks;

[StopOnFirstError]

public class TicketedCodeRunnerBenchmark
{
    [Params(0.5, 1, 2, 5)]
    public double PollTime { get; set; }
    
    [Params(10, 20, 50, 100)]
    public int NumberOfRequests { get; set; }
    
    [ParamsSource(nameof(CodeSubmissions))]
    public CodeSubmission CodeSubmission { get; set; }

    public static IEnumerable<CodeSubmission> CodeSubmissions => CodeLoader.Load();

    [Benchmark]
    [BenchmarkCategory("Sending without fetching results.")]
    public void PostAndWaitForResponseReceived()
    {
        IEnumerable<Task> clientActions = TaskBuilder.BuildClientTaskList(NumberOfRequests, client =>
        {
            client.Post(CodeSubmission);
        });
        
        Task.WhenAll(clientActions).Wait();
    }

    [Benchmark]
    [BenchmarkCategory("Sending and then fetch results.")]
    public void PostAndWaitForAllResultsFetched()
    {
        TimeSpan timeBetweenPulls = TimeSpan.FromSeconds(PollTime);
        
        IEnumerable<Task> clientActions = TaskBuilder.BuildClientTaskList(NumberOfRequests, client =>
        {
            client.PostAndGetHaskellResultTask(CodeSubmission.code, CodeSubmission.test, timeBetweenPulls);
        });
        
        Task.WhenAll(clientActions).Wait();
    }
}