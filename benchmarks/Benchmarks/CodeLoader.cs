using CodeRunnerClients.DataTransfer;

namespace Benchmarks;

public static class CodeLoader
{
    private const string CodeDirectoryName = "BenchmarkCode";
    private const string CodeFileName = "code.hs";
    private const string TestFileName = "test.hs";
    
    public static IEnumerable<CodeSubmission> Load()
    {
        List<CodeSubmission> codeSubmissions = new();
        
        string currentDir = Directory.GetCurrentDirectory();
        string codeDir = Path.Combine(currentDir, CodeDirectoryName);

        if (!Directory.Exists(codeDir))
        {
            throw new IOException($"Benchmark code directory does not exist at path \"{codeDir}\".");
        }

        string[] directories = Directory.GetDirectories(codeDir);

        foreach (string directory in directories)
        {
            string code = File.ReadAllText(Path.Combine(directory, CodeFileName));
            string test = File.ReadAllText(Path.Combine(directory, TestFileName));
            
            codeSubmissions.Add(new CodeSubmission(code, test));
        }

        return codeSubmissions;
    }
}