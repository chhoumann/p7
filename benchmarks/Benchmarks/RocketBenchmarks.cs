using System.Text.Json;
using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Engines;
using CodeRunnerClients;
using CodeRunnerClients.DataTransfer;

namespace Benchmarks;

[StopOnFirstError]
//launchCount = #processes, warmup iterations, target is actual bench count.
[SimpleJob(RunStrategy.Monitoring, launchCount: 5, warmupCount: 0, targetCount: 10)]
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
        var clientActions = new List<Task>(NumberOfRequests);
        List<Task> tasks = new(NumberOfRequests);
        
        for (int i = 0; i < NumberOfRequests; i++)
        {
            CodeRunnerClient client = new();
             p;
            p.Add(async () =>
            {
                var Result = await JsonSerializer.DeserializeAsync<PullIdResponse>(await client.Post(CodeSubmission).Result.Content.ReadAsStreamAsync());

                
            };
        }
        
        Task.WhenAll(clientActions).Wait();
    }

    private async void Action(CodeRunnerClient client)
    { 
        HttpResponseMessage  response = await client.Post(CodeSubmission);
        response.EnsureSuccessStatusCode();
    }
}