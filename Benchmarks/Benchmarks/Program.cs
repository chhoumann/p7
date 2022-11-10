// See https://aka.ms/new-console-template for more information

using BenchmarkDotNet.Running;
using dotenv.net;

namespace Benchmarks;

class Program
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