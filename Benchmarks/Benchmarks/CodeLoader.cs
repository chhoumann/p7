namespace Benchmarks;

public class CodeLoader
{
    private const string CodeDirectoryName = "BenchmarkCode";
    
    public void Load()
    {
        string currentDir = Directory.GetCurrentDirectory();
        string codeDir = Path.Combine(currentDir, CodeDirectoryName);

        if (!Directory.Exists(codeDir))
        {
            throw new IOException($"Benchmark code directory does not exist at path \"{codeDir}\".");
        }
        
    }
}