using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Engines;
using CodeRunnerClients;
using CodeRunnerClients.DataTransfer;

namespace Benchmarks;

[StopOnFirstError]
[CsvMeasurementsExporter]
[HtmlExporter]
[CsvExporter]
[MarkdownExporterAttribute.Default]
[SimpleJob(RunStrategy.Monitoring, launchCount: 1, warmupCount: 5, targetCount: 40)]
public class TicketedCodeRunnerBenchmark
{
    [Params(0.5, 1.0, 2.0, 3.0)]
    public double PollTime { get; set; }
    
    [Params(10, 20, 50, 100)]
    public int NumberOfConcurrentRequests { get; set; }
    
    [ParamsSource(nameof(CodeSubmissions))]
    public CodeSubmission CodeSubmission { get; set; }

    public static IEnumerable<CodeSubmission> CodeSubmissions => CodeLoader.Load();

    [Benchmark]
    public void PostAndWaitForAllResultsFetched()
    {
        TimeSpan timeBetweenPolls = TimeSpan.FromSeconds(PollTime);
        
        IEnumerable<Task> clientActions = TaskBuilder.BuildClientTaskList<CodeRunnerQueueClient>(NumberOfConcurrentRequests, client =>
            client.PostAndGetHaskellResultTask(CodeSubmission.code, CodeSubmission.test, timeBetweenPolls));

        Task.WhenAll(clientActions).Wait();
    }
}