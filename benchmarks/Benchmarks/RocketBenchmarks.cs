using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Engines;
using CodeRunnerClients;
using CodeRunnerClients.DataTransfer;

namespace Benchmarks;

[StopOnFirstError]
//launchCount = #processes, warmup iterations, target is actual bench count.
[SimpleJob(RunStrategy.Monitoring, launchCount: 5, warmupCount: 10, targetCount: 40)]
public class RocketBenchmarks
{
    [Params(10, 20, 50, 100)]
    public int NumberOfRequests { get; set; }
    
    [ParamsSource(nameof(CodeSubmissions))]
    public CodeSubmission CodeSubmission { get; set; }
    
    public static IEnumerable<CodeSubmission> CodeSubmissions => CodeLoader.Load();

    [Benchmark]
    public void PostAndWaitForResponseReceived()
    {
        IEnumerable<Task> clientActions = TaskBuilder.BuildClientTaskList<CodeRunnerClient>(NumberOfRequests, Action);
        Task.WhenAll(clientActions).Wait();
    }

    private async void Action(CodeRunnerClient client)
    { 
        HttpResponseMessage  response = await client.Post(CodeSubmission);
        Console.WriteLine("\n \n \n HERE");

        Console.WriteLine(await response.Content.ReadAsStringAsync());
        if (!response.IsSuccessStatusCode)
        {
            throw new Exception();
        }
    }
}