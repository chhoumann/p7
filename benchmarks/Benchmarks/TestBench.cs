using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Engines;

namespace Benchmarks;

[SimpleJob(RunStrategy.ColdStart, targetCount: 5)]
[MinColumn, MaxColumn, MeanColumn, MedianColumn]
public class TestBench
{
    private bool firstCall;

    [Benchmark]
    public void Foo()
    {
        if (firstCall == false)
        {
            firstCall = true;
            Console.WriteLine("// First call");
            Thread.Sleep(100);
        }
        else
            Thread.Sleep(10);
    }
}