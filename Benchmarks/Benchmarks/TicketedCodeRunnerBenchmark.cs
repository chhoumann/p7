using System;
using System.Collections.Generic;
using System.Threading.Tasks;
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
    
    [Params(10, 20)]
    public int NumberOfRequests { get; set; }

    [Benchmark]
    [BenchmarkCategory("Sending without fetching results")]
    public void PostAndWaitForResponseReceived()
    {
        Console.WriteLine("Running benchmark PostAndWaitForResponseReceived.");

        CodeSubmit submission = new(CorrectCode, IncorrectTest);
        
        IEnumerable<Task> clientActions = TaskBuilder.BuildClientTaskList(NumberOfRequests, client =>
        {
            client.Post(submission);
        });
        
        Task.WhenAll(clientActions).Wait();
    }

    [Benchmark]
    [BenchmarkCategory("Sending and then fetch results - 5 second pull time")]
    public void PostAndWaitForAllResultsFetched_5SecondsBetweenPolls()
    {
        Console.WriteLine("Running benchmark PostAndWaitForAllResultsFetched_5SecondsBetweenPolls.");
        
        IEnumerable<Task> clientActions = TaskBuilder.BuildClientTaskList(NumberOfRequests, client =>
        {
            client.PostAndGetHaskellResultTask(CorrectCode, IncorrectTest, TimeSpan.FromSeconds(5));
        });
        
        Task.WhenAll(clientActions).Wait();
    }

    [Benchmark]
    [BenchmarkCategory("Sending and then fetch results - 2 second pull time")]
    public void PostAndWaitForAllResultsFetched_2SecondsBetweenPolls()
    {
        Console.WriteLine("Running benchmark PostAndWaitForAllResultsFetched_2SecondsBetweenPolls.");
        
        IEnumerable<Task> clientActions = TaskBuilder.BuildClientTaskList(NumberOfRequests, client =>
        {
            client.PostAndGetHaskellResultTask(CorrectCode, IncorrectTest, TimeSpan.FromSeconds(2));
        });
        
        Task.WhenAll(clientActions).Wait();
    }

    [Benchmark]
    [BenchmarkCategory("Sending and then fetch results - 1 second pull time")]
    public void PostAndWaitForAllResultsFetched_1SecondsBetweenPolls()
    {
        Console.WriteLine("Running benchmark PostAndWaitForAllResultsFetched_1SecondsBetweenPolls.");
        
        IEnumerable<Task> clientActions = TaskBuilder.BuildClientTaskList(NumberOfRequests, client =>
        {
            client.PostAndGetHaskellResultTask(CorrectCode, IncorrectTest, TimeSpan.FromSeconds(1));
        });
        
        Task.WhenAll(clientActions).Wait();
    }

    [Benchmark]
    [BenchmarkCategory("Sending and then fetch results - 0.5 second pull time")]
    public void PostAndWaitForAllResultsFetched_HalfSecondsBetweenPolls()
    {
        Console.WriteLine("Running benchmark PostAndWaitForAllResultsFetched_HalfSecondsBetweenPolls.");
        
        IEnumerable<Task> clientActions = TaskBuilder.BuildClientTaskList(NumberOfRequests, client =>
        {
            client.PostAndGetHaskellResultTask(CorrectCode, IncorrectTest, TimeSpan.FromMilliseconds(500));
        });
        
        Task.WhenAll(clientActions).Wait();
    }
}