using BenchmarkDotNet.Running;
using dotenv.net;

namespace Benchmarks;

public static class Program
{
    public static void Main(string[] args)
    {
        DotEnv.Fluent()
            .WithExceptions()
            .WithEnvFiles()
            .Load();

        BenchmarkRunner.Run<TicketedCodeRunnerBenchmark>();
    }
}